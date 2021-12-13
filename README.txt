Running the program
1. Navigate to this directory in terminal.
2. Open SWI Prolog
	Note: for me this is just entering the command `swipl`in the terminal
3. Load the Prolog program using the following command
?- ['typeCheck.pl'].
4. Run a program by calling main. 
	Note: There are two ways to run the program, Run the program and let it tell you the security class of the program by letting the second argument be a variable or assign a security class to the program and see if it is valid. Below are examples
?- main("test_while.txt", SC).
?- main("test_while.txt", high).


Working Testfiles:
test_skip.txt
test_seq.txt
test_while.txt
test_assign.txt
test_if.txt
test1.txt
test2.txt
test3.txt
test4.txt
test5.txt
test6.txt


Notes: 
Tests 1, 2, 4, and 6 are expected to return false becuase they have illegal flows. I tried to implement a parser using lex and yacc but was inexperienced with the tools and was not able to successfully  parse code. You  can see examples of how to properly format the code so that prolog will understand it. It does suffer from ambiguity from a lack of braces of indentation so if and while statements dont have a defined end and I have not looked into the effects that that will have. I am running version 8.0.3, but that should have no effect on the program.

