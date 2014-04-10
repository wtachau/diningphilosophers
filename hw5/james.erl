%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile james
%% erl -noshell -run james main <Start_Node> <End_Node> <M> -run init stop -noshell
%%


-module(james).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/0]).

main() ->
	Return_Value = hash("Another God Damn Test", 0, 6),
	io:format("Hash Value ~p~n", [Return_Value]).
% storage process
% List -- list of tuples; our dictionary
% Number -- the process number
% Node -- node address the process is currently on
% M -- our constant value m
% global:whereis_name(Name) will return the PID that we need to send a message to
storage_process(Dictionary, Process_Number, M)-> 
	
	receive

		% store a value for our key, msg the controller,
		{PID, Ref, store, Key, Value}->
			Send_To = hash(Key, 0, M),
			if 
				Send_To == Process_Number->
					% send the old value to the controller
					Old_Value = get_value(Dictionary, Key),
					PID ! {Ref, stored, Old_Value},
					% find our backup node and send a message to that process to backup our data
					% finds intermediate node to pass message to if we can't make it in one hop
					Process_ID = recipient(Process_Number, get_backup_node(Process_Number, M), 0, M),
					Msg = {self(), make_ref(), backup, Key, Value},
					send(Msg, Process_ID, storage_process),
					% update the storage process
					storage_process(Dictionary--[{Key, Old_Value}]++[{Key, Value}], Process_Number, M);

				% the storage process does not have the key
				true ->
					% our next process which receives a message
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					Msg = {PID, Ref, store, Key, Value},
					send(Msg, Process_ID, storage_process)
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
			end;

		% retrieve the value of a given key
		{PID, Ref, retrieve, Key}-> 
			Send_To = hash(Key, 0, M),
			if  % the key we want to retrieve is on this process
				Send_To == Process_Number ->
					Return_Value = get_value(Dictionary, Key),
					PID ! {Ref, Return_Value, result};
				true ->
					% the key is not on this process
					Process_ID = recipient(Process_Number, Send_To, 0, M),
					send({PID, Ref, retrieve, Key}, Process_ID, storage_process)
			end;


		% find the last key in lexicographic order
		{PID, Ref, first_key}->
			if
				Process_Number == 0 ->
					Return_Value = get_smallest;
				true -> nothing
			end;

		% find the last key in lexicographic order
		{PID, Ref, last_key}->
			nothing;

		% number of keys currently stored in the system
		{PID, Ref, num_keys}->
			nothing;

		% list of node numbers currently in the system
		{PID, Ref, node_list}->
			nothing;

		% sent from the controller to leave the system
		{PID, Ref, leave}->
			nothing;

		{PID, Ref, collect}->
			% calculate the number of the last node in the system
			Arith = round(math:pow(2, M) - 1),
			if
				Process_Number == Arith ->
					nothing;
				true ->
					nothing
			end
	end.




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
				(Start_Node + round(math:pow(2, K - 1)) rem round(math:pow(2, M)));
		true->  
				% we keep searching
				recipient(Start_Node, End_Node, K + 1, M)
	end.


% finds the index that the process belongs to
get_node(Process_Number, M) ->
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
get_backup_node(Process_Number, M)->
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



