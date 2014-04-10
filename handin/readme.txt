Homework 4
March 13, 2014
Will Tachau, James Reinke

Our code was implemented so as to conform to the messages specified by the professor in the assignment. To receive these messages, we have two functions named ‘handle_message.’ (Two because for all states except hungry, we need to pass around State, Neighbors, Tokens, and ForkRequests. While hungry, we also need to remember the Ref of the controller to send back a message when that process begins eating). These functions handle messages sent by the controller (become_hungry, stop_eating, leave) as well as messages sent between processes (joining, give_fork, eat_request, etc.). Similarly, we pass a lot of messages via send_message().

To create new philosophers and add them to the “table,” we used the following command line structure:
   erl -noshell -run philosopher main <name> -run init stop -noshell
   erl -noshell -run philosopher main <name> <neighbor1> -run init stop -noshell
   erl -noshell -run philosopher main <name> <neighbor1> <neighbor2> -run init stop -noshell
   …etc

Regarding the experience of implementation, a lot of overhead was spent designing the algorithm and figuring out how it would work in a functional program like erlang. The two of us split the coding up so that one person handled a lot of the message passing, and the other handled what information was manipulated in state changes (this explains a lot of the auxiliary functions like thinking() or hungry() that correspond to states). Also integral in the experience of this assignment was being at Norm’s at 4 in the morning asking for continuous refills of coffee.

We also implemented the extra credit, where neighbors detect unexpected termination via the monitor function and take appropriate action.

We have also included our sample test program, philosopherTester, which could be used as such:
erl -noshell -run philosopherTester main -run init stop -noshell
>>>:<message> <nodeName>

We have included sample output for a series of commands in files Process1.txt, Process2.txt, and Process3.txt. Commands sent are in Messages.txt.
