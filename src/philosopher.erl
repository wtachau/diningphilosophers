%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile philosopher
%% erl -noshell -run philosopher main <name> <neighbor1> <neighbor2> -run init stop -noshell
%% erl -noshell -run philosopher main <name> <neighbor1> -run init stop -noshell
%%
%% To run tester:
%% erl -noshell -run philosopherTester main -run init stop -noshell
%% >>> become_hungry <name>@<host>

-module(philosopher).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main(Params) ->
	Name = hd(Params),
	% The rest of the parameters are the philosopher's neighbors
	Neighbors = tl(Params),
	% Philosopher gives all neighbors dirty forks, has none
	Tokens = [],
	State = joining,
	EatRequests = [],
	
	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	register(philosopher, self()),
	io:format("Registered under philosopher at node ~p, waiting for messages.~n", [node()]),

	% Initial join takes an empty list (confirmed neighbors), and Neighbors
	joining(Neighbors, []).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
% Two handle_message functions, since hungry needs to keep track of its controller

% Handles a message sent to philosopher (every state except hungry)
handle_message(State, Neighbors, Tokens, EatRequests) ->
	io:format("(~p) is ~p, waiting for message..~n", [node(), State]),
	io:format("(~p)'s forks are ~p~n", [node(), Tokens]),
	receive
		% CONTROLLER METHOD %
		% Should only receive when thinking. Transition to hungry.
		{PID, Ref, become_hungry} ->
			io:format("(~p) Received 'become_hungry' check message from ~p~n", [node(), PID]),
			% first ask all neighbors for forks, and go to hungry
			send_message(Neighbors, eat_request),
			hungry(Neighbors, Tokens, EatRequests, PID, Ref);
		
		% CONTROLLER METHOD %
		% Should only receive when eating. Transition to thinking.
		{PID, Ref, stop_eating} ->
			io:format("(~p) Received 'stop_eating' check message from ~p~n", [node(), PID]),
			% send out forks to who asked for it, and go to thinking
			send_message(EatRequests, give_fork),

			% turn all our forks dirty
			DirtyTokens = make_dirty(Tokens),
			io:format("(~p) all our forks: ~p~n", [node(), DirtyTokens]),

			% Now get rid of those which have been asked for
			{DeleteTokens, _} = priority(DirtyTokens, EatRequests),
			io:format("(~p) Should get rid of these tokens: ~p~n", [node(), DeleteTokens]),
			thinking(Neighbors, DirtyTokens -- DeleteTokens); %fixme: this doesn't do what we want
		
		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("(~p) Received 'leave' message from ~p~n", [node(), PID]),
			leaving(Neighbors, PID, Ref);
		
		% Confirm neighbor is joining
		{PID, Ref, joining} ->
			io:format("(~p) Received request to join as neighbor, sending confirmation to ~p~n", [node(), PID]),
			PID ! {self(), Ref, confirmed},
			monitor(process, PID),
			handle_message(State, Neighbors++[PID], Tokens++[{PID, clean}], EatRequests);
		
		% One of its friends is leaving
		{PID, Ref, gone} ->
			io:format("(~p) Received notice that ~p is leaving~n", [node(), PID]),
			NewTokens = Tokens -- [{PID, clean}] -- [{PID, dirty}],
			handle_message(State, Neighbors--[PID], NewTokens, EatRequests);
		
		% Received in thinking or eating
		{PID, Ref, eat_request} ->
			io:format("(~p) received 'eat_request' from (~p)~n", [node(), PID]),
			case State of
				% If I'm thinking, try to relinquish fork
				thinking  -> 
					% We don't know if fork is clean or dirty, so subtract both
					io:format("(~p) is thinking, give up my fork ~p (<- may be dirty)~n", [node(), [{PID, clean}]]),
					TempTokens = Tokens -- [{PID, clean}],
					FinalTokens = TempTokens -- [{PID, dirty}], 
					io:format("(~p) forks are now ~p~n", [node(), FinalTokens]),
					send_message([PID], give_fork),
					thinking(Neighbors, FinalTokens);
				% If I'm eating, stay eating and add to eat requests
				eating -> 
					io:format("(~p) Received request for fork from ~p, but I'm eating~n", [node(), PID]),
					eating(Neighbors, Tokens, EatRequests++[PID])
			end;
			
		% a process died
		{'DOWN', Ref, process, PID, Reason} ->
			io:format("(~p) Received a DOWN message from process ~p because ~p~n", [node(), PID, Reason]),
			handle_message(State, Neighbors--[PID], Tokens--[{PID, dirty}, {PID, clean}], EatRequests--[PID])
	end.

% Handles a message sent to philosopher (**only called when hungry!)
handle_message(State, Neighbors, Tokens, EatRequests, Controller, ControllerRef) ->
	io:format("(~p) is ~p, waiting for message (with Controller ID)...~n", [node(), State]),
	io:format("(~p) is hungry! Forks are ~p~n", [node(), Tokens]),
	receive
		% Was asked for a fork (also called above)
		{PID, Ref, eat_request} ->
			io:format("(~p) received 'eat_request' from (~p)~n", [node(), PID]),
			hungry(Neighbors, Tokens, EatRequests++[PID], Controller, ControllerRef);
		
		% Was given a fork
		{PID, Ref, give_fork} ->												%% JAMES <- SOMEHOW THIS GETS THE WRONG PID?
			io:format("(~p) received 'give_fork' from (~p)~n", [node(), PID]), 
			% get rid of the fork and add it to avoid duplicates
			TempTokens = Tokens--[{PID, clean}],
			Temp2Tokens = TempTokens--[{PID, dirty}],
			FinalTokens = Temp2Tokens ++ [{PID, clean}],
			io:format("(~p) forks are now ~p~n", [node(), FinalTokens]),
			hungry(Neighbors, FinalTokens, EatRequests, Controller, ControllerRef);
		
		% Confirm neighbor is joining
		{PID, Ref, joining} ->
			io:format("(~p) Received request to join as neighbor, sending confirmation to ~p~n", [node(), PID]),
			PID ! {self(), Ref, confirmed},
			monitor(process, PID),
			handle_message(State, Neighbors++[PID], Tokens++[{PID, clean}], EatRequests, Controller, ControllerRef);
		
		% One of its friends is leaving
		{PID, Ref, gone} ->
			io:format("(~p) Received notice that ~p is leaving~n", [node(), PID]),
			handle_message(State, Neighbors--[PID], Tokens--[{PID, clean}]--[{PID, dirty}], EatRequests, Controller, ControllerRef);

		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("(~p) Received 'leave' message from ~p~n", [node(), PID]),
			leaving(Neighbors, PID, Ref);

		% a process died
		{'DOWN', Ref, process, PID, Reason} ->
			io:format("(~p) Received a DOWN message from process ~p because ~p~n", [node(), PID, Reason]),
			handle_message(State, Neighbors--[PID], Tokens--[{PID, dirty}, {PID, clean}], EatRequests--[PID], Controller, ControllerRef)
	end.

% Sends a message to a list of philosophers (recursively) determined by auxiliary functions
send_message([], Message) ->
	ok;
send_message(Receivers, Message) ->
	Ref = make_ref(), % make a ref so I know I got a valid response back
		if 
			Message == joining ->
				NodeName = list_to_atom(hd(Receivers)),
				io:format("(~p) sending message ~p to ~p~n", [node(), Message, NodeName]),
				{philosopher, NodeName} ! {self(), Ref, Message},
				% and send the rest recursively
				send_message(tl(Receivers), Message);
			true ->
				PID = hd(Receivers),
				io:format("(~p) sending message ~p to ~p~n", [node(), Message, PID]), 
				PID ! {self(), Ref, Message},				%% JAMES <- THIS SEEMS TO SEND AS IF IT WERE SOMEONE ELSE?
				% and send the rest recursively
				send_message(tl(Receivers), Message)
		end.
% send a message to a single process, the controller
send_message(ControllerPID, Ref, Message)->
	% here we do not make a reference but rather pass the old reference
	% so the controller is aware of the origin of the message
	% i.e get_hungry eventually eats and leaving is eventually gone
	if
		Message == eating ->
			io:format("(~p) sending message ~p to ~p~n", [node(), Message, ControllerPID]),
			ControllerPID ! {Ref, Message};
		Message == gone ->
			io:format("(~p) sending message ~p to ~p~n", [node(), Message, ControllerPID]),
			ControllerPID ! {Ref, Message}
	end.

% Wait for another join confirmation
joining_listener(State, Neighbors, ConfirmedNeighbors) ->
	receive
 		{PID, Ref, confirmed} ->
 			monitor(process, PID),
			joining(Neighbors, ConfirmedNeighbors++[PID])
	end.

% need some leaving auxiliary here


% current state of the philosopher; will be an atom
% [A|B] = List of Neighbors
% [Aa|Bb] = List of neighbors that have confirmed our presence
% [C|D] = List of forks where each item is a tuple: {neighbor_id, clean/dirty}
% [E|F] = List of hungry neighbors

% joining -> joining or thinking

% if the lists are equal, then we move to thinking
joining([A|B], [Aa|Bb]) ->
	Size1 = len([A|B]),
	Size2 = len([Aa|Bb]),
	if 
		Size1 == Size2 ->
			% we are now thinking; list of neighbors and empty list of forks and no fork requests
			io:format("(~p) Confirmed from all neighbors, move to thinking~n~n", [node()]),
			handle_message(thinking, [Aa|Bb], [], []);
		true -> 
			joining_listener(joining, [A|B], [Aa|Bb])
	end;

% we have no confirmed philosophers, so we send a message out asking for confirmation
joining([A|B], _) ->
	send_message([A|B], joining),
	% now we listen with an empty list of confirmations
	joining_listener(joining, [A|B], []);

% this must be the first philosopher or we have sent all of our messages
joining([], []) ->
	io:format("first philosopher!~n"),
	handle_message(thinking, [], [], []).

% thinking -> thinking or hungry or leaving

% the controller sets the next state from here
thinking(Neighbors, Forks) ->
	handle_message(thinking, Neighbors, Forks, []).


% hungry -> hungry or eating or leaving

% we have hungry neighbors and forks
hungry([A|B], [C|D], [E|F], ControllerPID, ControllerRef) ->
	io:format("(~p) Became hungry, with hungry neighbors~n", (node())),
	% give up our forks where others have priority and are hungry
	{ForksToGive, IDs} = priority([C|D], [E|F]),
	case ForksToGive of
		[A|B] ->
				send_message(IDs, give_fork),
				% remove the forks and ID's from our list
				handle_message(hungry, [A|B], [C|D]--ForksToGive, [E|F]--IDs, ControllerPID, ControllerRef)
	end,
	% the case where we do not have any forks to give; we make no changes
	handle_message(hungry, [A|B], [C|D], [E|F], ControllerPID, ControllerRef);

% we have no hungry neighbors
hungry(Neighbors, Forks, [], ControllerPID, ControllerRef) ->
	io:format("(~p) Became hungry, with no hungry neighbors~n", [node()]),
	SizeN = len(Neighbors),
	SizeF = len(Forks),

	if 
		SizeN == SizeF ->
			% we have all the forks so we eat and notify the controller
			send_message(ControllerPID, ControllerRef, eating),
			handle_message(eating, Neighbors, Forks, []);
		true -> handle_message(hungry, Neighbors, Forks, [], ControllerPID, ControllerRef)
			% we do not have all the forks so we wait arity 5
	end.

% eating -> eating or thinking or leaving
% the controller sets the next state from here
eating(Neighbors, Forks, HungryNeighbors) ->
	handle_message(eating, Neighbors, Forks, HungryNeighbors).


% leaving -> leaving or gone

% tell our neighbors and the controller that we are peacing
leaving(Neighbors, ControllerID, Ref) ->
	% arity 3 is for controller messages
	send_message(ControllerID, Ref, gone),
	% arity 2 is for neighbor messages; no reference required
	send_message(Neighbors, gone).

% checks if the two lists are of equal length
%equal([A|B], [C|D]) -> equal(B, D);
%equal(_, _) -> true;
%equal(List, List) -> false.

len([]) -> 0;
len([_|T]) -> 1 + len(T).

% returns a list of forks and IDs that have priority over us and are hungry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we don't have two non-empty lists so no forks need to be given away
priority([], _) -> {[], []};
priority(_, []) -> {[], []};

priority([C|D], [E|F]) ->
	% find all of the forks where we don't have priority
	DirtyForks = remove_clean([C|D], []),
	% return a list of dirty forks that are in our hungry list
	priority_helper_1(DirtyForks, [E|F], [], []).



% return forks where ID's are in our hungry list
priority_helper_1([C|D], [E|F], ReturnForks, ReturnIDs) ->
	case priority_helper_2(C, [E|F]) of
		% add the fork to the list
		{true, ID} -> priority_helper_1(D, [E|F], ReturnForks++[C], ReturnIDs++[ID]);
		% do not add the element to the list
		{false, _} -> priority_helper_1(D, [E|F], ReturnForks, ReturnIDs)
	end;
priority_helper_1(_, _, ReturnForks, ReturnIDs) -> 
	{ReturnForks, ReturnIDs}.

% checks if the this single fork is in our hungry list
priority_helper_2({ID, dirty}, [E|F]) ->
	case ID =:= E of
		false -> priority_helper_2({ID, dirty}, F);
		true -> {true, ID}
	end;

% we have finished iterating the list and know it does not exist
priority_helper_2({ID, dirty}, _) -> {false, 0}.

% removes all clean forks from our list
remove_clean([C|D], DirtyForks) ->
	case C of 
		{_, dirty} -> remove_clean(D, DirtyForks++[C]);
		{_, _} -> remove_clean(D, DirtyForks)
	end;

remove_clean(_, DirtyForks) -> DirtyForks.

% return a list of IDs from a list of forks
get_fork_ids([C|D]) ->
	% add the tuple element to our list
	get_fork_ids(D) ++[get_id(C)].

get_id({ID, _}) -> ID.

make_dirty([]) ->
	[];
make_dirty([{PID, Status}|D]) ->
	[{PID,dirty}] ++ make_dirty(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
