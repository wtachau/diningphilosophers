%% @author wtachau
%% @doc @todo Add description to philosopherTester.


-module(philosopherTester).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main(Params) ->
	% get command line arguments
	NodeName = list_to_atom(hd(Params)), % node name of server ("something@host")
	ProcName = list_to_atom("philosopher"), % registry name

	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	
	% format microseconds of timestamp to get an 
	% effectively-unique node name
	{_, _, Micro} = os:timestamp(),
    net_kernel:start([list_to_atom("client" ++ integer_to_list(Micro)), 
        				  shortnames]),
	
	send_words(NodeName).

%% ====================================================================
%% Internal functions
%% ====================================================================
send_words(NodeName) -> 
	try 
		Message = become_hungry,
		io:format("Process ~p at node ~p sending message ~p to ~p at ~p~n", 
				  [self(), node(), Message, "philosopher", NodeName]),
		Ref = make_ref(), % make a ref so I know I got a valid response back
		{"philosopher", NodeName} ! {self(), Ref, Message}
		%%receive
		%%	{Ref, become_hungry} ->
		%%		io:format("Got message from server: Correct!~n");
		%%	{Ref, incorrect, Suggestions} ->
		%%		io:format("Got message from server: Incorrect.~n"),
		%%		io:format("Suggestions: "),
		%%		lists:map(fun(L)-> io:format(L), io:format(", ") end, Suggestions),
		%%		io:format("~n");
		%%	{Ref, add} ->
		%%		io:format("Got message from server: Added word~n");
		%%	{Ref, remove} ->
		%%		io:format("Got message from server: Removed word~n")
        %%after ?TIMEOUT -> io:format("Timed out waiting for reply!")
		%%end,
		%%send_words(ProcName, NodeName, tl(Words))
	catch
		_:_ -> io:format("Error parsing command line parameters or resolving node name.~n")
	end.

