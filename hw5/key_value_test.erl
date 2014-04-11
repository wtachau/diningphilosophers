%% @author wtachau
%% @doc @todo Add description to philosopherTester.


-module(key_value_test).
-define(TIMEOUT, 5000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main([Node]) ->

	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	
	% format microseconds of timestamp to get an 
	% effectively-unique node name
	{_, _, Micro} = os:timestamp(),
    net_kernel:start([list_to_atom("client" ++ integer_to_list(Micro)), 
        				  shortnames]),
    % connect to existing node
	Neighbor = list_to_atom(Node),
	net_kernel:connect_node(Neighbor),
	timer:sleep(1000),
	io:format("Connected to ~p~n",[global:registered_names()]),
	
	send_command().

%% ====================================================================
%% Internal functions
%% ====================================================================

% Simple IO. Prompt the user and return input
get_user_input( Prompt ) ->
	string:tokens(
 	 string:strip(   % remove spaces from front and back
    	string:strip( % remove line-feed from the end
      		io:get_line( Prompt), right, $\n)), " ").

send_command() -> 
	try 
		Input = get_user_input(">>>:"),
		Node = list_to_atom(hd(Input)),
		Command = list_to_atom(hd(tl(Input))),
		Key = hd(tl(tl(Input))),
		Value = hd(tl(tl(tl(Input)))),
	
		io:format("(~p) Sending message ~p to ~p~n", [node(), Command, Node]),
		Ref = make_ref(), % make a ref so I know I got a valid response back

		if 
			(Command == store) ->
				global:send(Node, {self(), Ref, store, Key, Value});
			(Command == retrieve) ->
				global:send(Node, {self(), Ref, retrieve, Key});
			true ->
				global:send(Node, {self(), Ref, Command})
		end,
		
		% wait for response
		receive
			{NewRef, stored, OldValue} ->
				io:format("Got message 'stored' with old value = ~p~n", [OldValue]);
			{NewRef, retrieved, Value} ->
				io:format("Got message 'retrieved' with value ~p~n", [Value]);
			{NewRef, result, Result} ->
				io:format("Got message 'result' with Result = ~p~n", [Result])
        after 
        	?TIMEOUT -> io:format("Timed out waiting for reply!~n")
		end,

		send_command()
	catch
		_:_ -> io:format("Error parsing command line parameters or resolving node name.~n")
	end.


