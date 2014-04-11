Team Members: James Reinke and Will Tachau

Reflection:
	We both found this assignment particuarly interesting.  The chord algorithm was nice and simple solution to minimizing message passing while maintaining huge lookup tables for which node to send the message to.  We could simply send the message and our simple rule would ensure it reached the correct recipient in log time based on the number of nodes.  The assignment was difficult to debug because of the inconsistent behavior of a distributed system using erlang message passing.  We could not ensure many things about the messages, but we were still able to produce robust enough code.

The code has many auxilliary functions but one can simply run it through the main method using the specifications in the homework assignment paper.  With duplicate keys, overwrite the previous key to ensure a non-repeating set.  