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
-export([main/1, storage_start/3, spawn_proc/2]).

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
storage_start(List, Process_Number, M) ->
	global:register_name(list_to_atom("StorageProcess"++integer_to_list(Process_Number)), self()),
	io:format("process ~p started at ~p with List ~p~n", [Process_Number, self(), List]),
	storage_process(List, Process_Number, M).

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
			io:format("Process #~p on node ~p has been taken over from ~p! Dying now...~n", [Process_Number, node(), Name]);

		% Receive a message after being spawned with dict values
		{store, NewList} ->
			io:format("Process ~p's new list is ~p~n", [Process_Number, NewList]),
			storage_process(NewList, Process_Number, M);

		% store a value for our key, msg the controller,
		{PID, Ref, store, Key, Value}->
			Send_To = hash(Key, 0, M),
			if 
				Send_To == Process_Number->
					% send the old value to the controller
					Old_Value = get_value(List, Key),
					PID ! {Ref, stored, Old_Value},
					% find our backup node and send a message to that process to backup our data
					% finds intermediate node to pass message to if we can't make it in one hop
					Process_ID = recipient(Process_Number, get_backup_node(Process_Number, M), 0, M),
					Msg = {self(), make_ref(), backup, Key, Value},
					send(Msg, Process_ID, storage_process),
					% update the storage process
					storage_process(List--[{Key, Old_Value}]++[{Key, Value}], Process_Number, M);

				% the storage process does not have the key
				true ->
					% our next process which receives a message
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					Msg = {PID, Ref, store, Key, Value},
					send(Msg, Process_ID, storage_process),
					storage_process(List, Process_Number, M)
			end;

		% a dictionary value meant for backup on the correct node
		{PID, Ref, backup, Key, Value}->
			Send_To = hash(Key, 0, M),
			Node_ID = get_node(Process_Number, M),
			Msg = {self(), Ref, backup, Key, Value},
			if  % this process is in the same node so we send to our non-storage process
				Send_To == Node_ID ->
					send(Msg, Node_ID, node);
				% the backup node is not on the node of the process so we forward
				true ->
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					send(Msg, Process_ID, storage_proess)
			end,
			storage_process(List, Process_Number, M);

		% retrieve the value of a given key
		{PID, Ref, retrieve, Key}-> 
			Send_To = hash(Key, 0, M),
			if  % the key we want to retrieve is on this process
				Send_To == Process_Number ->
					Return_Value = get_value(List, Key),
					PID ! {Ref, Return_Value, result};
				true ->
					% the key is not on this process
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					send({PID, Ref, retrieve, Key}, Process_ID, storage_process)
			end,
			storage_process(List, Process_Number, M);


		% find the last key in lexicographic order
		{PID, Ref, first_key} ->
			% creates one large dictionary from all storage processes
			Snapshot = gather_snapshot(List, [], Process_Number, M, 0),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = hd(Snapshot_Sorted),
			PID ! {Ref, Key, result},
			storage_process(List, Process_Number, M);

		% find the last key in lexicographic order
		{PID, Ref, last_key}->
			% creates one large dictionary from all storage processes
			Snapshot = gather_snapshot(List, [], Process_Number, M, 0),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = lists:last(Snapshot_Sorted),
			PID ! {Ref, Key, result},
			storage_process(List, Process_Number, M);

		% number of keys currently stored in the system
		{PID, Ref, num_keys}->
			% creates one large dictionary from all storage processes
			Snapshot_Size = lists:length(gather_snapshot(List, [], Process_Number, M, 0)),
			PID ! {Ref, Snapshot_Size, result},
			storage_process(List, Process_Number, M);

		% list of node numbers currently in the system
		{PID, Ref, node_list}->
			nothing;

		% sent from the controller to leave the system
		{PID, Ref, leave}->
			nothing;

		% send the dictionary to the collector
		{PID, Ref, Collector_Number, snapshot} ->
			Msg = {PID, Ref, Collector_Number, snapshot},
			Total_Storage_Processes = round(math:pow(2, M)),
			Recipient = Process_Number + 1 rem Total_Storage_Processes,
			send(Msg, Recipient, storage_process),
			send_dictionary(List, Collector_Number)
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
			spawn(?MODULE, storage_start, [List, Process_Number, M]),
			% continue receiving messages
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
	spawn(?MODULE, storage_start, [[], N, M]),
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


% return a mod of our hash value
hash([], Total, M) -> 
	Return = Total rem round(math:pow(2, list_to_integer(M)) - 1),
	Return;

% returns the hash value based on our global M
hash(String, Total, M) ->
	Arith = hd(String) + round(math:pow(2, 6)) + round(math:pow(2, 16)) - Total,
	hash(tl(String), Arith + Total, M).

get_value([], Key) -> no_value;

get_value(Dictionary, Key) -> no_value.

get_smallest([]) -> no_value;

get_smallest(Dictionary) -> 0. 

% returns the node number that a storage process should send the message to for greatest efficiency
% Start Node: the node sending the message
% End Node: the node receiving the message
% K: our current add value 2^k, which starts at 0
% M: the exponent M for how many nodes in the system
recipient(Start_Node, End_Node, K, M) ->
	% if we are less than the start and greater than the end, we have gone too far!
	Arith = round(math:pow(2, K)),
	Distance = abs(Start_Node - End_Node),
	io:format("Arithetic: ~p, Distance: ~p~n", [Arith, Distance]),
	% if our added exponent is greater than the distance, we have gone too far!
	Test = Arith > Distance,

	if
		Test ->
				% our previous answer was the correct one
				(Start_Node + round(math:pow(2, K - 1)) rem round(math:pow(2, list_to_integer(M))));
		true->  
				% we keep searching
				recipient(Start_Node, End_Node, K + 1, M)
	end.


% finds the index that the process belongs to
get_node(Process_Number, Mstr) ->
	M = list_to_integer(Mstr),
	Arith = (Process_Number + round(math:pow(2,M))) rem round(math:pow(2,M)),
	io:format("Arith String: ~p~n", [Arith]),
	Name = list_to_atom("Node"++integer_to_list(Arith)),
	case global:whereis_name(Name) of
		undefined ->
			get_node(Process_Number - 1, M);
		PID ->
			Process_Number
	end.

% finds the node that is backing up a processes' information
get_backup_node(Process_Number, Mstr)->
	M = list_to_integer(Mstr),
	Node_Number = get_node(Process_Number, M),
	Arith = (Node_Number - 1 + round(math:pow(2,M))) rem round(math:pow(2,M)),
	get_node(Arith, M).


% sends a message using our global table
send(Msg, Number, node)->
	Name = list_to_atom("Node"++integer_to_list(Number)),
	global:send(Name, Msg);

send(Msg, Number, storage_process)->
	Name = list_to_atom("StorageProcess"++integer_to_list(Number)),
	global:send(Name, Msg).

gather_snapshot(Dictionary, Snapshot, Process_Number, M, Total_Received)->
	Total_Storage_Processes = round(math:pow(2, M)),

	if
		Total_Received == Total_Storage_Processes ->
			Snapshot++Dictionary;
		true-> ok
	end,
	receive
		{Item, dict_item} ->
			gather_snapshot(Dictionary, Snapshot++[Item], Process_Number, M, Total_Received);
		{complete} ->
			gather_snapshot(Dictionary, Snapshot, Process_Number, M, Total_Received + 1)
	end.

% tell the collector that we have finished sending our dictionary
send_dictionary([], Collector_Number) -> 
	send({complete}, Collector_Number, storage_process);

% send dictionary items one by one to the collector
send_dictionary(Dictionary, Collector_Number) ->
	send({hd(Dictionary), dict_item}, Collector_Number, storage_process),
	send_dictionary(tl(Dictionary), Collector_Number).





