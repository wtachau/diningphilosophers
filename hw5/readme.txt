Team Members: James Reinke and Will Tachau

Reflection:
	We both found this assignment particuarly interesting.  The chord algorithm was nice and simple solution to minimizing message passing while maintaining huge lookup tables for which node to send the message to.  We could simply send the message and our simple rule would ensure it reached the correct recipient in log time based on the number of nodes.  The assignment was difficult to debug because of the inconsistent behavior of a distributed system using erlang message passing.  We could not ensure many things about the messages, but we were still able to produce robust enough code.

The code has many auxilliary functions but one can simply run it through the main method using the specifications in the homework assignment paper.  With duplicate keys, overwrite the previous key to ensure a non-repeating set.  

For testing, we recommend running the key_value_test module like so:
1. Start a number of nodes running:
	* erl -noshell -run key_value_node main <n> node1 -run init stop -noshell
	* erl -noshell -run key_value_node main <n> node2 node1@<host> -run init stop -noshell
	...
	* erl -noshell -run key_value_node main <n> node_m node1@<host> -run init stop -noshell
2. Start the test module running and connect it to the running nodes:
	* erl -noshell -run key_value_test main node1@<host> -run init stop -noshell
3. This will bring up a shell, where you can specify a name in the registry and send it commands.
	(It always requires two arguments, or it will fail. Commands like num_keys which don't need
	more arguments can just take empty characters like _).
4. Sample output for the test shell can be found in test_shell.txt, and the corresponding output 
	from the programs running can be found in nodes1, 2, and 3.txt.

	