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
	io:format("Registered under philosopher at node ~p, waiting for messages.~n", [node()]),

	% Initial join takes an empty list (confirmed neighbors), and Neighbors
	joining(Neighbors, []).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	

% Handles a message sent to philosopher
handle_message(State, Neighbors, Tokens, EatRequests) ->
	io:format("Waiting for message"),
	receive
		% CONTROLLER METHOD %
		% Should only receive when thinking. Transition to hungry.
		{PID, Ref, become_hungry} ->
			send_message(Neighbors, eat_request),
			io:format("Received 'become_hungry' check message from ~p~n", [PID]),
			hungry(Neighbors, Tokens, EatRequests);
		
		% CONTROLLER METHOD %
		% Should only receive when eating. Transition to eating.
		{PID, Ref, stop_eating} ->
			io:format("Received 'stop_eating' check message from ~p~n", [PID]),
			% send out forks to who asked for it
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
					thinking(Neighbors, Tokens); %fixme
				% If I'm eating, add to eat requests
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
			hungry(Neighbors, Tokens, EatRequests++PID);
		
		% Was given a fork
		{PID, Ref, give_fork} ->
			hungry(Neighbors, Tokens++{PID, clean}, EatRequests);

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
		NodeName = hd(Receivers),
		io:format("Process ~p sending message [~p] to ~p~n", [self(), Message, NodeName]),
		Ref = make_ref(), % make a ref so I know I got a valid response back
		{philosopher, NodeName} ! {self(), Ref, Message},
		% and send the rest
		send_message(tl(Receivers), Message).

% Wait for another join confirmation
joining_listener(State, Neighbors, ConfirmedNeighbors) ->
	receive
		{PID, Ref, confirmed} ->
			joining(Neighbors, ConfirmedNeighbors++PID)
	end.



%%%% JAMES IMPLEMENTATION %%%

% current state of the philosopher; will be an atom
% [A|B] = List of Neighbors
% [Aa|Bb] = List of neighbors that have confirmed our presence
% [C|D] = List of Tokens where each item is a tuple: {neighbor_id, clean/dirty}
% [E|F] = List of Requests tuples {neighbor_id} where requests that are pending will only be hungry requests

% Joining
% Two non-empty lists: If not equal, then listen for next confirmation.
% If equal, go to thinking.
joining([A|B], [Aa|Bb]) -> [];
% we have no confirmed philosophers, so we send a message out asking for confirmation
joining([A|B], _) -> [];
% this must be the first philosopher, so we enter thinking
joining(_, _) -> [].

% hungry -> hungry or eating or leaving
hungry([A|B], [C|D], [E|F]) -> [];
hungry([A|B], _, _) -> [];
hungry(_, _, _) -> [].

% eating -> eating or thinking or leaving
eating([A|B], [C|D], [E|F]) -> [];
eating([A|B], _, _) -> [];
eating(_, _, _) -> []. 

% call handle message with same parameters
thinking([A|B], [C|D]) -> [];
thinking([A|B], _) -> [];
thinking(_, _) -> [].

leaving(_, _) -> []. 
