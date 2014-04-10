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
	Node1 = 35,
	global:register_name(list_to_atom("Node5"), self()),
	global:register_name(list_to_atom("Node20"), self()),
	global:register_name(list_to_atom("Node30"), self()),
	global:register_name(list_to_atom("Node35"), self()),
	Backup = backup_node(Node1,6),
	io:format("Backup for ~p is ~p~n",[Node1, Backup]).

% storage process
% List -- list of tuples; our dictionary
% Number -- the process number
% Node -- node address the process is currently on
% M -- our constant value m
% global:whereis_name(Name) will return the PID that we need to send a message to
storage_process(List, Process_Number, M)-> 
	
	receive

		% store a value for our key, msg the controller,
		{PID, Ref, store, Key, Value}->
			Send_To = hash(M, Key),
			if 
				Send_To == Process_Number->
					% send the old value to the controller
					Old_Value = find_value_for_key(List, Key),
					PID ! {Ref, stored, Old_Value},
					% find our backup node and send a message to that process to backup our data
					Recipient_ID = recipeint()
					% update the storage process
					storage_process(List--[{Key, Old_Value}]++[{Key, Value}], Process_Number, M);

				% the storage process does not have the key
				true ->
					% our next process which receives a message
					Recipient_ID = recipient(Process_Number, Send_To, M),
					% find the PID of our recipient
					Recipient_PID = get_pid(Recipient_ID, storage_process),
					% send the message along
					Recipient_PID ! {PID, Ref, store, Key, Value}
			end;

		% retrieve the value of a given key
		{PID, Ref, retrieve, Key}->
			nothing;

		% find the last key in lexicographic order
		{PID, Ref, first_key}->
			nothing;

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
			nothing

	end.


hash([], Key) -> 0;

% returns the hash value based on our global m
hash(String, Key) -> 0.


find_value_for_key([], Key) -> no_value;

find_value_for_key(Dictionary, Key) -> no_value.

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


% finds the index that the node belongs to
get_node(Process_Number, M) ->
	Arith = (Process_Number + round(math:pow(2,M))) rem round(math:pow(2,M)),
	io:format("Arith String: ~p~n", [Arith_String]),
	case get_pid(Arith, node) of
		undefined ->
			backup_node(Process_Number - 1, M);
		PID ->
			Process_Number
	end.

% finds the node that is backing up a processes' information
get_backup_node(Process_Number, M)->
	Node_Number = get_node(Process_Number),
	Arith = (Node_Number - 1 + round(math:pow(2,M))) rem round(math:pow(2,M)),
	get_node(Arith).

get_pid(Number, node)->
	global:whereis_name(list_to_atom("Node"++integer_to_list(Number)));

get_pid(Number, storage_process)->
	global:whereis_name(list_to_atom("StorageProcess"++integer_to_list(Number))).




