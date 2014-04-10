%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile key_value_node
%% erl -noshell -run key_value_node main 3 node1 -run init stop -noshell
%% erl -noshell -run key_value_node main 3 node2 node1@<host> -run init stop -noshell
%%

-module(key_value_node).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1, storage_start/2, spawn_proc/2]).

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

% Storage processes start here
storage_start(Process_Number, M) ->
	global:register_name(list_to_atom("StorageProcess"++integer_to_list(Process_Number)), self()),
	io:format("process ~p started  at ~p~n", [Process_Number, self()]),
	storage_process([], Process_Number, M).

% Continuous listening method for storage processes
storage_process(List, Process_Number, M)->
	M2 = round(math:pow(2,list_to_integer(M))),
	receive
		{Name, takeover} ->
			% find the non-storage node before the one who sent takeover
			PrevNode = get_prev_node(Name),
			NextNodeNum = get_next_node_num(Name), % this, minus 1, will be our last process
			io:format("I am proc ~p, was owned by ~p (~p) ~n",[Process_Number, PrevNode, NextNodeNum]),
			% send that node my Dict
			global:send(PrevNode, {imdying, List, Process_Number, Name}),

			% If i'm the last, send special message
			if 
				NextNodeNum == (Process_Number + 1) rem M2 ->

					% Find the node who was storing backup data, and tell them not to store my data
					BackupNode = get_prev_node(PrevNode),
					global:send(PrevNode, {dump, List, Process_Number, Name, BackupNode});
				true ->
					ok
			end,
			io:format("Process #~p on node ~p has been taken over from ~p! Dying now...~n", [Process_Number, node(), Name])
	end.

% Non-storage processes listen here
non_storage_process(BackupDict, NewBackupDict, M) ->
	receive
		% From a storage process "going out of business"
		{imdying, List, Process_Number, NewNode} ->
			
			io:format("time to delete ~p~n", [Process_Number]),

			% send new node dicts to populate storage processes
			global:send(NewNode, {makeproc, List, Process_Number}),

			% Accumulate Dicts from dying processes - these will be the new backup
			non_storage_process(BackupDict, NewBackupDict++List, M);
			
		% Last process of a group that are leaving
		{dump, List, Process_Number, NewNode, BackupNode} ->
			% send new node its backup data
			global:send(NewNode, {sendbackup, BackupDict}),
			% Find the node who was storing backup data, and tell them not to store my data
			global:send(BackupNode, {erasebackup, NewBackupDict}),
			% its new backup is whatever processes are dying
			non_storage_process(NewBackupDict, [], M);

		% Given a new backup dict
		{sendbackup, Dict} ->
			non_storage_process(Dict, [], M);

		% Start new processes, give them appropriate info
		{makeproc, List, Process_Number} ->
			% start that process as your own
			spawn(?MODULE, storage_start, [Process_Number, M]),
			% do something with List, which is proc's info
			non_storage_process(BackupDict, [], M);

		% Delete certain key/values from your backup data
		{erasebackup, List} ->
			non_storage_process(BackupDict--List, NewBackupDict, M);

		{Ref, {Key, Value}, backup} ->
			io:format("backing up...")
	end.

% get the node before this one
get_prev_node(NodeName) ->
	AllNames = global:registered_names(),
	AllNodeNums = get_all_nums(AllNames),
	CurIndex = [list_to_integer(string:substr(atom_to_list(NodeName), 5, 1))],
	Length = len(AllNodeNums),
	PrevIndex = (string:str(AllNodeNums, [CurIndex]) - 1),
	PrevIndexMod = (PrevIndex + Length) rem Length, % in case negative
	PrevNum = lists:nth(PrevIndexMod, AllNodeNums),
	list_to_atom("Node"++integer_to_list(PrevNum)).

% get the node after this one
get_next_node_num(NodeName) ->
	AllNames = global:registered_names(),
	AllNodeNums = get_all_nums(AllNames),
	CurIndex = [list_to_integer(string:substr(atom_to_list(NodeName), 5, 1))],
	Length = len(AllNodeNums),
	NextIndex = (string:str(AllNodeNums, [CurIndex]) + 1) rem Length,
	lists:nth(NextIndex, AllNodeNums).


%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

spawn_proc(0, _) ->
	timer:sleep(1000), % necessary.
	AllNames = global:registered_names(),
	io:format("All names in registry... ~p~n", [AllNames]);
spawn_proc(N, M) ->
	spawn(?MODULE, storage_start, [N, M]),
	spawn_proc(N-1, M).



% If we are the first node in the system
join_system(Name, M, []) ->

	% Register myself
	global:register_name(list_to_atom("Node0"), self()),

	% To start, spawn all the processes
	spawn_proc(round(math:pow(2,list_to_integer(M))), M),

	% Now behave as non-storage process
	non_storage_process([], [], M);

% If there are >0 other nodes in the system
join_system(Name, M, [N]) ->

	M2 = round(math:pow(2,list_to_integer(M))),

	% connect to existing node
	Neighbor = list_to_atom(N),
	Result = net_kernel:connect_node(Neighbor),
	io:format("Connecting to ~p ... ~p ~n", [Neighbor, Result]),

	% Connect to other nodes
	timer:sleep(1000), % necessary.
	AllNames = global:registered_names(),

	% Pick a new node number, take its share of the processes
	NewNodeNum = get_node_num(M2, AllNames),
	io:format("I'm now Node~p~n",[NewNodeNum]),

	MyName = list_to_atom("Node"++integer_to_list(NewNodeNum)),
	global:register_name(MyName, self()),
	io:format("All names in registry... ~p~n", [global:registered_names()]),

	% Take over the appropriate processes
	TakenNodes = get_all_nums(AllNames),
	NextNum = lists:nth(((string:str(TakenNodes, [NewNodeNum]) + 1) rem M2), TakenNodes),
	ProcessesToTake = get_processes_to_take(NewNodeNum, NextNum, M2),
	take_processes(ProcessesToTake, MyName),
	% And behave as non storage node
	non_storage_process([], [], M).

% Calculate which processes to take
get_processes_to_take(Start, Finish, Total) ->
	if
		(Start == Finish) ->
			[];
		true ->
			[Start] ++ get_processes_to_take(((Start+1) rem Total), Finish, Total)
	end.

% Based off names in the registry, find a new node number
get_node_num(M2, Names) ->
	PossibleNums = possible_nums(M2),
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

% "Take" processes by sending them a message
take_processes([], _) ->
	ok;
take_processes(Processes, MyName) ->
	Name = list_to_atom("StorageProcess"++integer_to_list(hd(Processes))),
	io:format("sending message takeover to ~p (~p) ~n", [Name, global:whereis_name(Name)]),
	global:send(Name, {MyName, takeover}),
	take_processes(tl(Processes), MyName).






