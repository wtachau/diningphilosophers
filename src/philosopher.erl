%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile philosopher
%% erl -noshell -run philosopher main <name> <neighbor1> <neighbor2> -run init stop -noshell
%% erl -noshell -run philosopher main <name> <neighbor1> -run init stop -noshell

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
	io:format("Registered under philosopher at node ~p, (~p)waiting for messages.~n", [node()]),

	% Initial join takes an empty list (confirmed neighbors), and Neighbors
	joining(Neighbors, []).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
% Two handle_message functions, since hungry needs to keep track of its controller

% Handles a message sent to philosopher (every state except hungry)
handle_message(State, Neighbors, Tokens, EatRequests) ->
	io:format("Waiting for message"),
	receive
		% CONTROLLER METHOD %
		% Should only receive when thinking. Transition to hungry.
		{PID, Ref, become_hungry} ->
			io:format("Received 'become_hungry' check message from ~p~n", [PID]),
			% first ask all neighbors for forks, and go to hungry
			send_message(Neighbors, eat_request),
			hungry(Neighbors, Tokens, EatRequests, PID); %fixme: shouldn't this be []?
		
		% CONTROLLER METHOD %
		% Should only receive when eating. Transition to thinking.
		{PID, Ref, stop_eating} ->
			io:format("Received 'stop_eating' check message from ~p~n", [PID]),
			% send out forks to who asked for it, and go to thinking
			send_message(EatRequests, give_fork),
			thinking(Neighbors, Tokens -- EatRequests);
		
		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("Received 'leave' message from ~p~n", [PID]),
			leaving(Neighbors, PID);
		
		% Received in thinking or eating
		{PID, Ref, eat_request} ->
			case State of
				% If I'm thinking, relinquish fork
				thinking  -> 
					% We don't know if fork is clean or dirty, so subtract both
					Tokens = Tokens -- [{node(), clean}],
					Tokens = Tokens -- [{node(), dirty}],
					thinking(Neighbors, Tokens);
				% If I'm eating, stay eating and add to eat requests
				eating -> 
					eating(Neighbors, Tokens, EatRequests++PID)
			end
	end.

% Handles a message sent to philosopher (**only called when hungry!)
handle_message(State, Neighbors, Tokens, EatRequests, Controller) ->
	io:format("Waiting for message (with Controller ID)"),
	receive
		% Was asked for a fork (also called above)
		{PID, Ref, eat_request} ->
			hungry(Neighbors, Tokens, EatRequests++PID, Controller);
		
		% Was given a fork
		{PID, Ref, give_fork} ->
			hungry(Neighbors, Tokens++{PID, clean}, EatRequests, Controller);

		% CONTROLLER METHOD %
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			io:format("Received 'leave' message from ~p~n", [PID]),
			leaving(Neighbors, PID)
	end.

% Sends a message to a list of philosophers (recursively) determined by auxiliary functions
send_message([], Message) ->
	halt();
send_message(Receivers, Message) ->
	Ref = make_ref(), % make a ref so I know I got a valid response back
		if 
			Message == eating ->
				{philosopher, hd(Receivers)} ! {Ref, Message};
			Message == gone ->
				{philosopher, hd(Receivers)} ! {Ref, Message};
			true ->
				NodeName = hd(Receivers),
				io:format("Process ~p sending message [~p] to ~p~n", [self(), Message, NodeName]),
				{philosopher, NodeName} ! {self(), Ref, Message},
				% and send the rest
				send_message(tl(Receivers), Message)
		end.

% Wait for another join confirmation
joining_listener(State, Neighbors, ConfirmedNeighbors) ->
	receive
		{PID, Ref, confirmed} ->
			joining(Neighbors, ConfirmedNeighbors++PID)
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
	case equal([A|B], [Aa|Bb]) of
		% we are now thinking; list of neighbors and empty list of forks and no fork requests
		true -> handle_message(thinking, [A|B], [], []);
		false -> joining_listener(joining, [A|B], [Aa|Bb])
	end;

% we have no confirmed philosophers, so we send a message out asking for confirmation
joining([A|B], _) ->
	send_message([A|B], joining),
	% now we listen with an empty list of confirmations
	joining_listener(joining, [A|B], []);

% this must be the first philosopher or we have sent all of our messages
joining([], []) ->
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
	end;

% we have no hungry neighbors
hungry(Neighbors, Forks, [], ControllerID) ->
	
	case equal(Neighbors, Forks) of
		% we have all the forks so we eat and notify the controller
		true -> handle_message(eating, Neighbors, Forks, []),
				send_message([ControllerID], eating);
		% we do not have all the forks so we wait arity 5
		false -> handle_message(hungry, Neighbors, Forks, [], ControllerID)
	end.

% eating -> eating or thinking or leaving
% the controller sets the next state from here
eating(Neighbors, Forks, HungryNeighbors) ->
	handle_message(eating, Neighbors, Forks, HungryNeighbors).


% leaving -> leaving or gone

% tell our neighbors and the controller that we are peacing
leaving(Neighbors, ControllerID) ->
	send_message(Neighbors++[ControllerID], leaving).

% checks if the two lists are of equal length
equal([A|B], [C|D]) -> equal(B, D);

equal([A|B], _) -> false;

equal(_, _) -> true.

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
