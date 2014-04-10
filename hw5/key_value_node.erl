%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile key_value_node
%% erl -noshell -run key_value_node main 10 node1 -run init stop -noshell
%% erl -noshell -run key_value_node main 10 node2 node1@<host> -run init stop -noshell
%%

-module(key_value_node).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1, storage/1, spawn_proc/1]).

main(Params) ->

	% Value that determines the number of storage processes in system
	M = hd(Params),
	% The name to register with
	Name = hd(tl(Params)),
	% The next parameter, if there, is a node to get access to global set
	% of registered processes
	Neighbor = tl(tl(Params)),
	
	%% IMPORTANT: Start the empd daemon!
	os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	io:format("~p Registered as node ~p, with ~p~n", [timestamp(), node(), nodes()]),
	
	% Depending on whether Neighbor is empty
	join_system(Name, M, Neighbor).

storage(_) ->
	io:format("process started~n").

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

spawn_proc(0.0) ->
	ok;
spawn_proc(N) ->
	spawn(?MODULE, storage, [args]),
	spawn_proc(N-1).


% If we are the first node in the system
join_system(Name, M, []) ->

	% Register myself
	global:register_name(list_to_atom("Node0"), self()),

	% To start, spawn all the processes
	spawn_proc(math:pow(2,list_to_integer(M))),

	% sit and wait (testing)
	receive
		{_} ->
			io:format("...")
	end;

% If there are >0 other nodes in the system
join_system(Name, M, [N]) ->

	% connect to existing node
	Neighbor = list_to_atom(N),
	Result = net_kernel:connect_node(Neighbor),
	io:format("Connecting to ~p ... ~p ~n", [Neighbor, Result]),

	% Connect to other nodes
	timer:sleep(1000), % necessary.
	AllNames = global:registered_names(),
	io:format("All names in registry... ~p~n", [AllNames]),
	io:format("Connected nodes... ~p~n", [nodes()]),

	% Pick a new node number, take its share of the processes
	NewNodeNum = get_node_num(M, AllNames),
	io:format("I'm now Node~p~n",[NewNodeNum]),

	global:register_name(list_to_atom("Node"++integer_to_list(NewNodeNum)), self()).


% Based off names in the registry, find a new node number
get_node_num(M, Names) ->
	PossibleNums = possible_nums(round(math:pow(list_to_integer(M),2))),
	TakenNums = get_all_nums(Names),
	NumsLeft = lists:subtract(PossibleNums, TakenNums),
	random:seed(now()),
	lists:nth(random:uniform(len(NumsLeft)), NumsLeft).

possible_nums(0) ->
	[];
possible_nums(M) ->
	possible_nums(M-1) ++ [M-1].
	
get_all_nums([]) ->
	[];
get_all_nums(Names) ->
	Name = atom_to_list(hd(Names)),
	FirstPartNode = string:equal(string:substr(Name, 1, 4), "Node"),
	if
		FirstPartNode ->
			get_all_nums(tl(Names)) ++ [list_to_integer(string:substr(Name, 5, 1))];
		true ->
			get_all_nums(tl(Names))
	end.

len([]) -> 0;
len([_|T]) -> 1 + len(T).







