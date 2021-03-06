%% @author wtachau
%% @doc @todo Add description to philosopherTester.


-module(philosopherTester).
-define(TIMEOUT, 5000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([main/0]).

main() ->

	%% IMPORTANT: Start the empd daemon!
	_ = os:cmd("epmd -daemon"),
	
	% format microseconds of timestamp to get an 
	% effectively-unique node name
	{_, _, Micro} = os:timestamp(),
    net_kernel:start([list_to_atom("client" ++ integer_to_list(Micro)), 
        				  shortnames]),
	
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
		Command = list_to_atom(hd(Input)),
		Philosopher = list_to_atom(hd(tl(Input))),
	
		io:format("(~p) Sending message ~p to ~p~n", [node(), Command, Philosopher]),
		Ref = make_ref(), % make a ref so I know I got a valid response back
		{philosopher, Philosopher} ! {self(), Ref, Command},
		
		% wait for response
		receive
			{NewRef, eating} ->
				io:format("Got message from philosopher ~p: Eating!~n", [NewRef]);
			{NewRef, gone} ->
				io:format("Got message from philosopher ~p: Left!~n", [NewRef])
        after ?TIMEOUT -> io:format("Timed out waiting for reply!~n")
		end,
	
		send_command()
	catch
		_:_ -> io:format("Error parsing command line parameters or resolving node name.~n")
	end.

