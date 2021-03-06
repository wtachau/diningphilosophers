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
	io:format("~p Process ~p started at ~p ~n", [timestamp(), Process_Number, self()]),
	timer:sleep(1000), % necessary.
	storage_process(List, Process_Number, M).

% Continuous listening method for storage processes
storage_process(List, Process_Number, M)->
	M2 = round(math:pow(2,list_to_integer(M))),
	receive
		{Name, takeover} ->
			% find the non-storage node before the one who sent takeover
			PrevNode = get_prev_node(Name),
			NextNodeNum = get_next_node_num(Name), % this, minus 1, will be our last process
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
			io:format("~p Process ~p on ~p taken over from ~p! About to die...~n", [timestamp(), Process_Number, node(), Name]);

		% Receive a message after being spawned with dict values
		{store, NewList} ->
			storage_process(NewList, Process_Number, M);

		% store a value for our key, msg the controller,
		{PID, Ref, store, Key, Value}->
			io:format("~p Process ~p got message 'store'~n", [timestamp(), Process_Number]),
			Send_To = hash(Key, 0, M),
			if 
				Send_To == Process_Number->
					% send the old value to the controller
					Old_Value = get_value(List, Key),
					PID ! {Ref, stored, Old_Value},
					io:format("~p Process ~p storing value '~p' for key '~p'~n", [timestamp(), Process_Number, Value, Key]),
					% find our backup node and send a message to that process to backup our data
					% finds intermediate node to pass message to if we can't make it in one hop
					Backup_ID = get_backup_node(Process_Number, M),
					Process_ID = recipient(Process_Number, Backup_ID, 0, M),
					Msg = {self(), make_ref(), backup, Key, Value, Backup_ID},
					send(Msg, Process_ID, storage_process),
					% update the storage process
					NewList = List--[{Key, Old_Value}],
					NewList2 = NewList++[{Key, Value}],
					storage_process(NewList2, Process_Number, M);

				% the storage process does not have the key
				true ->
					% our next process which receives a message
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					Msg = {PID, Ref, store, Key, Value},
					io:format("~p Passing on message to process ~p~n", [timestamp(), Process_ID]),

					Name = list_to_atom("StorageProcess"++integer_to_list(Process_ID)),
					global:send(Name, Msg),
					storage_process(List, Process_Number, M)
			end;

		% a dictionary value meant for backup on the correct node
		{__, Ref, backup, Key, Value, Destination}->
			io:format("~p Process ~p got message 'backup'~n", [timestamp(), Process_Number]),
			Node_ID = get_node(Process_Number, M),
			
			if  % this process is in the same node so we send to our non-storage process
				Destination == Node_ID ->
					Msg = {self(), Ref, backup, Key, Value},
					io:format("~p Backing up data on node ~p~n", [timestamp(), Destination]),
					Name = list_to_atom("Node"++integer_to_list(Node_ID)),
					global:send(Name, Msg);
				% the backup node is not on the node of the process so we forward
				true ->
					Msg = {self(), Ref, backup, Key, Value, Destination},
					Process_ID = recipient(Process_Number, Destination, 0, M),
					Name = list_to_atom("StorageProcess"++integer_to_list(Process_ID)),
					global:send(Name, Msg)
			end,
			storage_process(List, Process_Number, M);

		% retrieve the value of a given key
		{PID, Ref, retrieve, Key}-> 
			io:format("~p Process ~p got message 'retrieve'~n", [timestamp(), Process_Number]),
			Send_To = hash(Key, 0, M),
			if  % the key we want to retrieve is on this process
				Send_To == Process_Number ->
					Return_Value = get_value(List, Key),
					io:format("~p Process ~p retrieved value ~p from Key '~p'~n", [timestamp(), Process_Number, Return_Value, Key]),
					PID ! {Ref, retrieved, Return_Value};
				true ->
					% the key is not on this process
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					Name = list_to_atom("StorageProcess"++integer_to_list(Process_ID)),
					Msg = {PID, Ref, retrieve, Key},
					io:format("~p~n", [Msg]),
					global:send(Name, Msg)
			end,
			storage_process(List, Process_Number, M);


		% find the last key in lexicographic order
		{PID, Ref, first_key} ->
			io:format("~p Process ~p got message 'first_key'~n", [timestamp(), Process_Number]),

			Msg = {PID, Ref, Process_Number, snapshot},

			% generate the first message
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,
			RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
			
			global:send(RecipientName, Msg),

			% start listening for snapshots before continuing
			Snapshot = gather_snapshot(List, [], M, 1),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = hd(Snapshot_Sorted),
			PID ! {Ref, result, Key},
			% once you're done, keep listening
			storage_process(List, Process_Number, M);

		% find the last key in lexicographic order
		{PID, Ref, last_key}->
			io:format("~p Process ~p got message 'last_key'~n", [timestamp(), Process_Number]),
			% creates one large dictionary from all storage processes

			Msg = {PID, Ref, Process_Number, snapshot},

			% generate the first message
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,
			RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
			
			global:send(RecipientName, Msg),

			% start listening for snapshots before continuing
			Snapshot = gather_snapshot(List, [], M, 1),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = lists:last(Snapshot_Sorted),
			PID ! {Ref, result, Key},
			storage_process(List, Process_Number, M);

		% number of keys currently stored in the system
		{PID, Ref, num_keys}->
			io:format("~p Process ~p got message 'num_keys'~n", [timestamp(), Process_Number]),
			% creates one large dictionary from all storage processes

			Msg = {PID, Ref, Process_Number, snapshot},

			% generate the first message
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,
			RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
			
			global:send(RecipientName, Msg),

			% start listening for snapshots before continuing
			Snapshot = gather_snapshot(List, [], M, 1),
			Snapshot_Size = length(Snapshot),
			PID ! {Ref, result, Snapshot_Size},
			storage_process(List, Process_Number, M);

		% list of node numbers currently in the system
		{_, _, node_list}->
			storage_process(List, Process_Number, M);

		% sent from the controller to leave the system
		{_, _, leave}->
			storage_process(List, Process_Number, M);

		% first collector - send out snapshot messages
		{PID, Ref, snapshot} ->
			io:format("~p Process ~p got message 'snapshot'~n", [timestamp(), Process_Number]),
			
			Msg = {PID, Ref, Process_Number, snapshot},

			% generate the first message
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,
			RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
			
			global:send(RecipientName, Msg),

			% start listening for snapshots before continuing
			Snapshot = gather_snapshot(List, [], M, 1),
			PID ! {Ref, result, Snapshot},
			% once you're done, keep listening
			storage_process(List, Process_Number, M);

		% send the dictionary to the collector
		{PID, Ref, Collector_Number, snapshot} ->
			
			Collector_Name = list_to_atom("StorageProcess"++integer_to_list(Collector_Number)),

			% First send dict
			send_dictionary(List, Collector_Name),

			% now pass on message
			Msg = {PID, Ref, Collector_Number, snapshot},
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,

			if 
				Collector_Number == Recipient ->
					ok;
				true ->
					RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
					global:send(RecipientName, Msg)
			end,
			% and keep listening
			storage_process(List, Process_Number, M)
	end.

% Non-storage processes listen here
non_storage_process(BackupDict, NewBackupDict, M, Name) ->
	receive
		% From a storage process "going out of business"
		{imdying, List, Process_Number, NewNode} ->

			% send new node dicts to populate storage processes
			global:send(NewNode, {makeproc, List, Process_Number}),

			% Accumulate Dicts from dying processes - these will be the new backup
			non_storage_process(BackupDict, NewBackupDict++List, M, Name);
			
		% Last process of a group that are leaving
		{dump, __, __, NewNode, BackupNode} ->
			% send new node its backup data
			global:send(NewNode, {sendbackup, BackupDict}),
			% Find the node who was storing backup data, and tell them not to store my data
			global:send(BackupNode, {erasebackup, NewBackupDict}),
			% its new backup is whatever processes are dying
			non_storage_process(NewBackupDict, [], M, Name);

		% Given a new backup dict
		{sendbackup, Dict} ->
			non_storage_process(Dict, [], M, Name);

		% Start new processes, give them appropriate info
		{makeproc, List, Process_Number} ->
			% start that process as your own
			spawn(?MODULE, storage_start, [List, Process_Number, M]),
			% continue receiving messages
			non_storage_process(BackupDict, [], M, Name);

		% Delete certain key/values from your backup data
		{erasebackup, List} ->
			non_storage_process(BackupDict--List, NewBackupDict, M, Name);

		% create new processes and give them their data
		{'DOWN', _, process, PID, _} ->


			MonitorNode = get_next_node_num(Name),
			MonitorNodeName = list_to_atom("Node"++integer_to_list(MonitorNode)),
			PrevNode = get_prev_node(Name),
			OurNumber = get_next_node_num(PrevNode),
			PID = global:whereis_name(MonitorNodeName),
			monitor(process, PID),
			% grab the fallen node's processes
			ProcessesToTake = get_processes_to_take(OurNumber, MonitorNode, M),
			spawn_proc_list(ProcessesToTake, M),
			timer:sleep(1000),
			store_dictionary(BackupDict, hd(ProcessesToTake)),
			non_storage_process(BackupDict, NewBackupDict, M, Name);

		{_, _, backup, Key, Value}->
			non_storage_process(BackupDict++[{Key,Value}], NewBackupDict, M, Name)
	end.


gather_snapshot(Dictionary, Snapshot, M, Total_Received)->
	Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
	if
		Total_Received == Total_Storage_Processes ->
			io:format("Found dictionaries: ~p ~n", [Snapshot++Dictionary]),
			Snapshot++Dictionary;
		true-> 
			receive
				{dict_item, Item} ->
					gather_snapshot(Dictionary, Snapshot++[Item], M, Total_Received);
				{complete} ->
					gather_snapshot(Dictionary, Snapshot, M, Total_Received + 1)
			end
	end.
	

% tell the collector that we have finished sending our dictionary
send_dictionary([], Collector) -> 
	global:send(Collector, {complete});

% send dictionary items one by one to the collector
send_dictionary(Dictionary, Collector) ->
	global:send(Collector, {dict_item, hd(Dictionary)}),
	send_dictionary(tl(Dictionary), Collector).

% send dictionary piece by piece to a process to store
store_dictionary([], _) -> ok;
store_dictionary(Dictionary, Name)->
	{Key, Value} = hd(Dictionary),
	Msg = {self(), make_ref(), store, Key, Value},
	send(Msg, Name, storage_process),
	store_dictionary(tl(Dictionary), Name).



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

spawn_proc(-1, _) ->
	timer:sleep(1000); % necessary.

spawn_proc(N, M) ->
	spawn(?MODULE, storage_start, [[], N, M]),
	spawn_proc(N-1, M).

spawn_proc_list([], _) ->
	timer:sleep(1000);

spawn_proc_list(Number_List, M) ->
	spawn(?MODULE, storage_start, [[], hd(Number_List), M]),
	spawn_proc_list(tl(Number_List), M).


% If we are the first node in the system
join_system(_, M, []) ->

	% Register myself
	global:register_name(list_to_atom("Node0"), self()),

	% To start, spawn all the processes
	spawn_proc(round(math:pow(2,list_to_integer(M))-1), M),

	% Now behave as non-storage process
	non_storage_process([], [], M, "Node0");

% If there are >0 other nodes in the system
join_system(_, M, [N]) ->

	M2 = round(math:pow(2,list_to_integer(M))),

	% connect to existing node
	Neighbor = list_to_atom(N),
	Result = net_kernel:connect_node(Neighbor),
	io:format("~p Connecting to ~p ... ~p ~n", [timestamp(), Neighbor, Result]),

	% Connect to other nodes
	timer:sleep(1000), % necessary.
	AllNames = global:registered_names(),

	% Pick a new node number, take its share of the processes
	NewNodeNum = get_node_num(M2, AllNames),
	io:format("~p Joining as node~p~n",[timestamp(), NewNodeNum]),

	MyName = list_to_atom("Node"++integer_to_list(NewNodeNum)),
	global:register_name(MyName, self()),

	% find the node we want to follow
	NumToFollow = get_next_node_num(MyName),
	NodeName = list_to_atom("Node"++integer_to_list(NumToFollow)),
	PID = global:whereis_name(NodeName),
	monitor(process, PID),


	% Take over the appropriate processes
	TakenNodes = get_all_nums(AllNames),
	NextNum = lists:nth(((string:str(TakenNodes, [NewNodeNum]) + 1) rem M2), TakenNodes),
	ProcessesToTake = get_processes_to_take(NewNodeNum, NextNum, M2),
	take_processes(ProcessesToTake, MyName),
	% And behave as non storage node
	non_storage_process([], [], M, MyName).

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

get_value([], _) -> 
	no_value;
get_value(Dictionary, Key) -> 
	{Key1, Value1} = hd(Dictionary),
	if 
		Key1 == Key ->
			Value1;
		true ->
			get_value(tl(Dictionary), Key)
	end.



% returns the node number that a storage process should send the message to for greatest efficiency
% Start Node: the node sending the message
% End Node: the node receiving the message
% K: our current add value 2^k, which starts at 0
% M: the exponent M for how many nodes in the system
recipient(Start_Node, End_Node, K, M) ->
	M2 = round(math:pow(2,list_to_integer(M))),
	% if we are less than the start and greater than the end, we have gone too far!
	Arith = round(math:pow(2, K)),
	Distance1 = End_Node - Start_Node,
	%io:format("Distance:~p m2:~p, ~n", [Distance1, M2]),
	if 
		Distance1 < 0 ->
			Distance = (End_Node+M2 - Start_Node);
		true ->
			Distance = Distance1
	end,

	% if our added exponent is greater than the distance, we have gone too far!
	Test = Arith > Distance,
	if
		Test ->
				% our previous answer was the correct one
				(Start_Node + round(math:pow(2, K - 1))) rem M2;
		true->  
				% we keep searching
				recipient(Start_Node, End_Node, K + 1, M)
	end.


% finds the index that the process belongs to
get_node(Process_Number, M) ->
	Arith = (Process_Number + round(math:pow(2, list_to_integer(M)))) rem round(math:pow(2, list_to_integer(M))),
	Name = list_to_atom("Node"++integer_to_list(Arith)),
	case global:whereis_name(Name) of
		undefined ->
			get_node(Process_Number - 1, M);
		_ ->
			Process_Number
	end.

% finds the node that is backing up a processes' information
get_backup_node(Process_Number, M)->
	Node_Number = get_node(Process_Number, M),
	Arith = (Node_Number - 1 + round(math:pow(2, list_to_integer(M)))) rem round(math:pow(2, list_to_integer(M))),
	BackupNode = get_node(Arith, M),
	BackupNode.


% sends a message using our global table
send(Msg, Number, node)->
	Name = list_to_atom("Node"++integer_to_list(Number)),
	global:send(Name, Msg);

send(Msg, Number, storage_process)->
	Name = list_to_atom("StorageProcess"++integer_to_list(Number)),
	global:send(Name, Msg).





