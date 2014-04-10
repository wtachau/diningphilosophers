%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile key_value_node
%% erl -noshell -run key_value_node main 10 node1 -run init stop -noshell
%% erl -noshell -run key_value_node main 10 node2 node1@host1 -run init stop -noshell
%%
%% To run tester:
%% erl -noshell -run philosopherTester main -run init stop -noshell
%% >>> become_hungry <name>@<host>

-module(key_value_node).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main(Params) ->

	% First, value that determines the number of storage processes in system
	M = hd(Params),
	% Next, the name to register with
	Name = hd(tl(Params)),
	% The next parameter, if there, is a node to get access to global set
	% of registered processes
	Neighbor = tl(tl(Params)),
	
	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	% register(philosopher, self()), necessary?
	io:format("~p Registered as node ~p.~n", [timestamp(), node()]),

	join_system(M, Neighbor).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).


join_system(M, []) ->
	io:format("no neighbors~n");
join_system(M, [Neighbor]) ->
	io:format("one neighbor: ~p~n", [Neighbor]).
