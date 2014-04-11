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
	timer:sleep(1000), % necessary.
	AllNames = global:registered_names(),
	%io:format("All names in registry... ~p~n", [AllNames]),
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
			io:format("hash says send to ~p~n", [Send_To]),
			if 
				Send_To == Process_Number->
					% send the old value to the controller
					Old_Value = get_value(List, Key),
					PID ! {Ref, stored, Old_Value},
					% find our backup node and send a message to that process to backup our data
					% finds intermediate node to pass message to if we can't make it in one hop
					io:format("STORED... sending backup ...~n"),
					Backup_ID = get_backup_node(Process_Number, M),
					Process_ID = recipient(Process_Number, Backup_ID, 0, M),
					io:format("backup Process_ID: ~p~n", [Process_ID]),
					Msg = {self(), make_ref(), backup, Key, Value, Backup_ID},
					io:format("**** want to back up on node ~p ****~n", [Backup_ID]),
					send(Msg, Process_ID, storage_process),
					% update the storage process
					NewList = List--[{Key, Old_Value}],
					NewList2 = NewList++[{Key, Value}],
					io:format("New list: ~p~n", [NewList2]),
					storage_process(NewList2, Process_Number, M);

				% the storage process does not have the key
				true ->
					% our next process which receives a message
					io:format("looking to pass on...~n"),
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					Msg = {PID, Ref, store, Key, Value},
					io:format("next recipient: ~p~n", [Process_ID]),

					Name = list_to_atom("StorageProcess"++integer_to_list(Process_ID)),
					io:format("sending ~p to ~p~n", [Msg, Name]),
					global:send(Name, Msg),
					storage_process(List, Process_Number, M)
			end;

		% a dictionary value meant for backup on the correct node
		{PID, Ref, backup, Key, Value, Destination}->
			io:format("Got backup message~n"),
			%Send_To = hash(Key, 0, M),
			Node_ID = get_node(Process_Number, M),
			io:format("Node_ID:~p Want to go to:~p~n", [Node_ID, Destination]),
			
			if  % this process is in the same node so we send to our non-storage process
				Destination == Node_ID ->
					Msg = {self(), Ref, backup, Key, Value},
					io:format("*Time to store on node~n"),
					Name = list_to_atom("Node"++integer_to_list(Node_ID)),
					io:format("going to send node message ~p to ~p~n", [Msg, Name]),
					global:send(Name, Msg);
				% the backup node is not on the node of the process so we forward
				true ->
					Msg = {self(), Ref, backup, Key, Value, Destination},
					Process_ID = recipient(Process_Number, Destination, 0, M),
					io:format("Process_ID for next backup attempt:~p~n",[Process_ID]),
					Name = list_to_atom("StorageProcess"++integer_to_list(Process_ID)),
					io:format("sending ~p to ~p~n", [Msg, Name]),
					global:send(Name, Msg)
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
			Snapshot = gather_snapshot(List, [], M, 0),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = hd(Snapshot_Sorted),
			PID ! {Ref, Key, result},
			storage_process(List, Process_Number, M);

		% find the last key in lexicographic order
		{PID, Ref, last_key}->
			% creates one large dictionary from all storage processes
			Snapshot = gather_snapshot(List, [], M, 0),
			Snapshot_Sorted = lists:keysort(1, Snapshot),
			{Key, _} = lists:last(Snapshot_Sorted),
			PID ! {Ref, Key, result},
			storage_process(List, Process_Number, M);

		% number of keys currently stored in the system
		{PID, Ref, num_keys}->
			% creates one large dictionary from all storage processes
			Snapshot_Size = lists:length(gather_snapshot(List, [], M, 0)),
			PID ! {Ref, Snapshot_Size, result},
			storage_process(List, Process_Number, M);

		% list of node numbers currently in the system
		{PID, Ref, node_list}->
			storage_process(List, Process_Number, M);

		% sent from the controller to leave the system
		{PID, Ref, leave}->
			storage_process(List, Process_Number, M);

		% first collector - send out snapshot messages
		{PID, Ref, snapshot} ->
			
			Msg = {PID, Ref, Process_Number, snapshot},

			% generate the first message
			Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
			Recipient = (Process_Number + 1) rem Total_Storage_Processes,
			RecipientName = list_to_atom("StorageProcess"++integer_to_list(Recipient)),
			
			global:send(RecipientName, Msg),

			% start listening for snapshots before continuing
			gather_snapshot(List, [], M, 1),
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
non_storage_process(BackupDict, NewBackupDict, M) ->
	receive
		% From a storage process "going out of business"
		{imdying, List, Process_Number, NewNode} ->

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

		{PID, Ref, backup, Key, Value}->
			non_storage_process(BackupDict++[{Key,Value}], NewBackupDict, M)
	end.


gather_snapshot(Dictionary, Snapshot, M, Total_Received)->
	Total_Storage_Processes = round(math:pow(2, list_to_integer(M))),
	io:format("in gather snapshot (total:~p, received:~p)~n", [Total_Storage_Processes, Total_Received]),
	if
		Total_Received == Total_Storage_Processes ->
			io:format("found all dictionaries! ~p ~n", [Snapshot++Dictionary]),
			Snapshot++Dictionary;
		true-> 
			receive
				{dict_item, Item} ->
					io:format("received dict_item ~p~n", [Item]),
					gather_snapshot(Dictionary, Snapshot++[Item], M, Total_Received);
				{complete} ->
					io:format("received complete!~n"),
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
	spawn_proc(round(math:pow(2,list_to_integer(M))-1), M),

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
	io:format("hash returned ~p~n", [Return]),
	Return;

% returns the hash value based on our global M
hash(String, Total, M) ->
	Arith = hd(String) + round(math:pow(2, 6)) + round(math:pow(2, 16)) - Total,
	hash(tl(String), Arith + Total, M).

get_value([], Key) -> 
	no_value;
get_value(Dictionary, Key) -> 
	{Key1, Value1} = hd(Dictionary),
	io:format("dict stuff...~p:~p~n",[ Key1, Value1] ),
	if 
		Key1 == Key ->
			Value1;
		true ->
			get_value(tl(Dictionary), Key)
	end.

get_smallest([]) -> no_value;

get_smallest(Dictionary) -> 0. 

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

	%io:format("Start: ~p, End: ~p", [Start_Node, End_Node]),
	%io:format("Arithetic: ~p, Distance: ~p~n", [Arith, Distance]),
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
	%io:format("Arith String: ~p~n", [Arith]),
	Name = list_to_atom("Node"++integer_to_list(Arith)),
	case global:whereis_name(Name) of
		undefined ->
			get_node(Process_Number - 1, M);
		PID ->
			Process_Number
	end.

% finds the node that is backing up a processes' information
get_backup_node(Process_Number, M)->
	io:format("in get_backup~n"),
	Node_Number = get_node(Process_Number, M),
	io:format("smalleset node#...~p~n", [Node_Number]),
	Arith = (Node_Number - 1 + round(math:pow(2, list_to_integer(M)))) rem round(math:pow(2, list_to_integer(M))),
	BackupNode = get_node(Arith, M),
	io:format("Backupnode: ~p~n", [BackupNode]),
	BackupNode.


% sends a message using our global table
send(Msg, Number, node)->
	Name = list_to_atom("Node"++integer_to_list(Number)),
	io:format("going to send node message ~p to ~p~n", [Msg, Name]),
	global:send(Name, Msg);

send(Msg, Number, storage_process)->
	io:format("going to send storage message ~p~n", [Msg]),
	Name = list_to_atom("StorageProcess"++integer_to_list(Number)),
	io:format("sending ~p to ~p~n", [Msg, Name]),
	global:send(Name, Msg).





