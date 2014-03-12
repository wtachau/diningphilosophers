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
	Neighbors = tl(Params),
	Tokens = [],
	
	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	register(spelling, self()),
	io:format("Registered under ~p at node ~p, waiting for messages.~n", ["philosopher", node()]),
	
	%% Figure out how many neighbors
	if 
		length(Neighbors) == 1 ->
			io:format("Philosopher ~p started with Neighbor~p~n", [Name, hd(Neighbors)]);
		true ->
			io:format("Philosopher ~p started with Neighbors ~p and ~p~n", [Name, hd(Neighbors), hd(tl(Neighbors))])
	end,
	
	handle_message(Neighbors, Tokens).


%% ====================================================================
%% Internal functions
%% ===========================`=========================================
% Handles a message using the specified reply
handle_message(Neighbors, Tokens) ->
	io:format("Waiting for message"),
	receive
		% Should only receive when hungry. Transition to hungry.
		{PID, Ref, become_hungry} ->
			io:format("Received 'become_hungry' check message from ~p~n", [PID]),
			handle_message(Neighbors, Tokens);
		% Should only receive when eating. Transition to eating.
		{PID, Ref, stop_eating} ->
			io:format("Received 'become_hungry' check message from ~p`~n", [PID]),
			handle_message(Neighbors, Tokens);
		% Can receive in anything other than joining. Leave ASAP.
		{PID, Ref, leave} ->
			handle_message(Neighbors, Tokens)
	end.