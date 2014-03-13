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
	receive
		% CONTROLLER METHOD %
		% Should only receive when thinking. Transition to hungry.
		{PID, Ref, become_hungry} ->
			io:format("(~p) Received 'become_hungry' check message from ~p~n", [node(), PID]),
			% first ask all neighbors for forks, and go to hungry
			send_message(Neighbors, eat_request),
			hungry(Neighbors, Tokens, EatRequests, PID);
		
		% CONTROLLER METHOD %
		% Should only receive when eating. Transition to thinking.
		{PID, Ref, stop_eating} ->
			io:format("(~p) Received 'stop_eating' check message from ~p~n", [node(), PID]),
			% send out forks to who asked for it, and go to thinking
			send_message(EatRequests, give_fork),
			thinking(Neighbors, Tokens -- EatRequests);
		
		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("(~p) Received 'leave' message from ~p~n", [node(), PID]),
			leaving(Neighbors, PID, Ref);
		
		% Confirm neighbor is joining
		{PID, Ref, joining} ->
			io:format("(~p) Received request to join as neighbor, sending confirmation to ~p~n", [node(), PID]),
			PID ! {self(), Ref, confirmed},
			handle_message(State, Neighbors++[PID], Tokens, EatRequests);
		
		% One of its friends is leaving
		{PID, Ref, gone} ->
			io:format("(~p) Received notice that ~p is leaving~n", [node(), PID]),
			handle_message(State, Neighbors--[PID], Tokens, EatRequests);
		
		% Received in thinking or eating
		{PID, Ref, eat_request} ->
			io:format("(~p) received 'eat_request' from (~p)~n", [node(), PID]),
			case State of
				% If I'm thinking, relinquish fork
				thinking  -> 
					% We don't know if fork is clean or dirty, so subtract both
					Tokens = Tokens -- [{self(), clean}], 
					Tokens = Tokens -- [{self(), dirty}],
					send_message([PID], give_fork),
					thinking(Neighbors, Tokens);
				% If I'm eating, stay eating and add to eat requests
				eating -> 
					eating(Neighbors, Tokens, EatRequests++[PID])
			end
	end.

% Handles a message sent to philosopher (**only called when hungry!)
handle_message(State, Neighbors, Tokens, EatRequests, Controller) ->
	io:format("(~p) is ~p, waiting for message (with Controller ID)...~n", [node(), State]),
	receive
		% Was asked for a fork (also called above)
		{PID, Ref, eat_request} ->
			io:format("(~p) received 'eat_request' from (~p)~n", [node(), PID]),
			hungry(Neighbors, Tokens, EatRequests++PID, Controller);
		
		% Was given a fork
		{PID, Ref, give_fork} ->
			io:format("(~p) received 'give_fork' from (~p)~n", [node(), PID]),
			hungry(Neighbors, Tokens++[{PID, clean}], EatRequests, Controller);
		
		% Confirm neighbor is joining
		{PID, Ref, joining} ->
			io:format("(~p) Received request to join as neighbor, sending confirmation to ~p~n", [node(), PID]),
			PID ! {self(), Ref, confirmed},
			handle_message(State, Neighbors++[PID], Tokens, EatRequests, Controller);
		
		% One of its friends is leaving
		{PID, Ref, gone} ->
			io:format("(~p) Received notice that ~p is leaving~n", [node(), PID]),
			handle_message(State, Neighbors--[PID], Tokens, EatRequests, Controller);

		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("(~p) Received 'leave' message from ~p~n", [PID]),
			leaving(Neighbors, PID, Ref)
	end.

% Sends a message to a list of philosophers (recursively) determined by auxiliary functions
send_message([], Message) ->
	ok;
send_message(Receivers, Message) ->
	Ref = make_ref(), % make a ref so I know I got a valid response back
		if 
			Message == eating ->
				io:format("(~p) sending message ~p to ~p~n", [node(), Message, hd(Receivers)]),
				hd(Receivers) ! {Ref, Message};
			%Message == gone ->
			%	io:format("(~p) sending message ~p to ~p~n", [node(), Message, hd(Receivers)]),
			%	hd(Receivers) ! {Ref, Message},
			%	send_message(tl(Receivers), Message);
			% at this point we still have nodenames
			Message == joining ->
				NodeName = list_to_atom(hd(Receivers)),
				io:format("(~p) sending message ~p to ~p~n", [node(), Message, NodeName]),
				{philosopher, NodeName} ! {self(), Ref, Message},
				% and send the rest recursively
				send_message(tl(Receivers), Message);
			true ->
				PID = hd(Receivers),
				io:format("(~p) sending message ~p to ~p~n", [node(), Message, PID]),
				PID ! {self(), Ref, Message},
				% and send the rest recursively
				send_message(tl(Receivers), Message)
		end.

% Wait for another join confirmation
joining_listener(State, Neighbors, ConfirmedNeighbors) ->
	receive
 		{PID, Ref, confirmed} ->
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
hungry([A|B], [C|D], [E|F], ControllerID) ->
	% give up our forks where others have priority and are hungry
	{ForksToGive, IDs} = priority([C|D], [E|F]),
	case ForksToGive of
		[A|B] ->
				send_message(IDs, fork),
				% remove the forks and ID's from our list
				handle_message(hungry, [A|B], [C|D]--ForksToGive, [E|F]--IDs, ControllerID)
	end,
	% the case where we do not have any forks to give; we make no changes
	handle_message(hungry, [A|B], [C|D], [E|F], ControllerID);

% we have no hungry neighbors
hungry(Neighbors, Forks, [], ControllerID) ->
	SizeN = len(Neighbors),
	SizeF = len(Forks),
	if 
		SizeN == SizeF ->
			% we have all the forks so we eat and notify the controller
			send_message([ControllerID], eating),
			handle_message(eating, Neighbors, Forks, []);
		true -> handle_message(hungry, Neighbors, Forks, [], ControllerID)
			% we do not have all the forks so we wait arity 5
	end.

% eating -> eating or thinking or leaving
% the controller sets the next state from here
eating(Neighbors, Forks, HungryNeighbors) ->
	handle_message(eating, Neighbors, Forks, HungryNeighbors).


% leaving -> leaving or gone

% tell our neighbors and the controller that we are peacing
leaving(Neighbors, ControllerID, Ref) ->
	io:format("(~p) sending leave message to controller~n",[node()]),
	ControllerID ! {Ref, gone},
	send_message(Neighbors, gone).

% checks if the two lists are of equal length
%equal([A|B], [C|D]) -> equal(B, D);
%equal(_, _) -> true;
%equal(List, List) -> false.

len([]) -> 0;
len([_|T]) -> 1 + len(T).

% returns a list of forks and IDs that have priority over us and are hungry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	io:fwrite("Priority 1 Terminating\n"),
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
