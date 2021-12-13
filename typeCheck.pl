/************ VARIABLE DECLARATIONS *****************/
/* declare variables and their class here */
class(h1, high).
class(m1, med).
class(l1, low).
class(h2, high).
class(m2, med).
class(l2, low).
class(ss, superSecret).

/* all numbers are low class but can be cast upward */
class(NUM, low) :- integer(NUM).
class(NUM, low) :- atom_number(NUM, X), integer(X).

/* all true and false are low class but can be cast upward */
class(BOOL_CONSTANT, low) :- atom_string(BOOL_CONSTANT, X), X="true".
class(BOOL_CONSTANT, low) :- atom_string(BOOL_CONSTANT, X), X="false".

/* skip is low security class */
class(SKIP_CONSTANT, low) :- atom_string(SKIP_CONSTANT, X), X="SKIP".

/************ CLASS DECLARATIONS *****************/
/* define upward relationships */
relationshipUp(low, med).
relationshipUp(med, high). 

/* allow casting up (for now only two levels) */
castUp(X,Y) :- relationshipUp(X,Y).
castUp(X,Z) :- relationshipUp(X,Y), relationshipUp(Y,Z).

/* define downward relationships */
relationshipDown(high, med).
relationshipDown(med, low). 

/* allow casting down (for now only two levels) */
castDown(X,Y) :- relationshipDown(X,Y).
castDown(X,Z) :- relationshipDown(X,Y), relationshipUp(Y,Z).

/************ ARITHMETIC STATEMENTS *****************/
/* base case just returns when the list is empty */
arith([], _, _).

/* if head and tail are the same security class then allow*/
arith([Head|Tail], TypeTail, FinalType) :- 
	%write("\nARITH1"),
	class(Head, TypeHead), 
	%write(Head), write(" 0 has SC "), write(TypeHead), write("\n"),
	TypeHead = TypeTail, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHead. 

/* allow if tail can be cast up to head */ 
arith([Head|Tail], TypeTail, FinalType) :- 
	%write("\nARITH2"),
	class(Head, TypeHead), 
	%write(Head), write(" 1 has SC "), write(TypeHead), write("\n"),
	castUp(TypeTail, TypeTailUp), 
	TypeHead = TypeTailUp, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHead.  

/* allow if head can be cast up to tail */ 
arith([Head|Tail], TypeTail, FinalType) :- 
	%write("\nARITH3"),
	class(Head, TypeHead), 
	%write(Head), write(" 2 has SC "), write(TypeHead), write("\n"),
	castUp(TypeHead, TypeHeadUp), 
	TypeHeadUp = TypeTail, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHeadUp. 

splitExpressionArith(Expr, FinalType) :-                                    /* take an expression and return a type */
	split_string(Expr, "+-*", " ", Tokenized_Strings),                /* tokenize the expression */
	list_term_to_atom(Tokenized_Strings, Tokenized_Atoms),             /* convert from strings to atoms */
	arith(Tokenized_Atoms, _, FinalType),                              /*  assign a type to the expression */
	write("Arithmetic Expression Passes\n").

/* convert list of strings to list of atoms*/
list_term_to_atom([], []).                                             /* base case return empty list */
list_term_to_atom([Head|Tail], Output) :-                              /* split list into H and T and return Output */
	term_to_atom(NewHead, Head),                                       /* convert head from trem to atom */
	list_term_to_atom(Tail, NewTail),                                  /* do the recursive call */
	Output = [NewHead|NewTail].                                        /* append head and tail and assign to return */


/************ ARITHMETIC STATEMENTS *****************/
/* base case just returns when the list is empty */
bool([], _, _).

/* if head and tail are the same security class then allow*/
bool([Head|Tail], TypeTail, FinalType) :- 
	class(Head, TypeHead), 
	%write(Head), write(" 0 has SC "), write(TypeHead), write("\n"),
	TypeHead = TypeTail, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHead. 

/* allow if tail can be cast up to head */ 
bool([Head|Tail], TypeTail, FinalType) :- 
	class(Head, TypeHead), 
	%write(Head), write(" 1 has SC "), write(TypeHead), write("\n"),
	castUp(TypeTail, TypeTailUp), 
	TypeHead = TypeTailUp, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHead.  

/* allow if head can be cast up to tail */ 
bool([Head|Tail], TypeTail, FinalType) :- 
	class(Head, TypeHead), 
	%write(Head), write(" 2 has SC "), write(TypeHead), write("\n"),
	castUp(TypeHead, TypeHeadUp), 
	TypeHeadUp = TypeTail, 
	arith(Tail, TypeTail, FinalType), 
	FinalType = TypeHeadUp. 

splitExpressionBool(Expr, FinalType) :-							/* take an expression and return a type */
	split_string(Expr, "&|!", " ", Tokenized_Strings),			/* tokenize the expression */
	list_term_to_atom(Tokenized_Strings, Tokenized_Atoms),		/* convert from strings to atoms */
	bool(Tokenized_Atoms, _, FinalType),						/*  assign a type to the expression */
	write("Boolean Epression Passes\n").

/************ top level command processing *****************/
/* SKIP */
process_command(Stream, Token_Skip, Type_Out) :-
	atom_string(Token_Skip, Token_Skip_Str),
	Token_Skip_Str = "SKIP", 
	class(Token_Skip, Token_Type),
	match_skip(Token_Type, Type_Out).

/* IF */
process_command(Stream, Line_If, Type_Out) :-			/* Check IF */
	atom_string(Line_If, Line_If_Str),		
	Line_If_Str = "IF",      				
	readLine(Stream, Line_Bool),			/* Check BOOLEAN */
	atom_string(Line_Bool,  Line_Bool_Str),	
	splitExpressionBool(Line_Bool_Str,  Type_Bool), 
	readLine(Stream, Line_Then),			/* Check THEN */
	atom_string(Line_Then, Line_Then_Str),	
	Line_Then_Str = "THEN",
	readLine(Stream, Line_C1),			/* Check C1 */
	process_command(Stream, Line_C1, Type_C1),
	readLine(Stream, Line_Else),			/* Check ELSE */
	atom_string(Line_Else, Line_Else_Str),	
	Line_Else_Str = "ELSE",
	readLine(Stream, Line_C2),			/* Check C2 */
	process_command(Stream, Line_C2, Type_C2),
	match_if(Type_Bool, Type_C1, Type_C2, Type_Out).  
	

/* WHILE */
process_command(Stream, Line_While, Type_Out) :-	/* Check WHILE */
	atom_string(Line_While, Line_While_Str),		
	Line_While_Str = "WHILE",      				
	readLine(Stream, Line_Bool),				/* Check BOOLEAN */
	atom_string(Line_Bool,  Line_Bool_Str),	
	splitExpressionBool(Line_Bool_Str,  Type_Bool), 
	readLine(Stream, Line_Do),					/* Check DO */
	atom_string(Line_Do, Line_Do_Str),	
	Line_Do_Str = "DO",
	readLine(Stream, Line_C1),					/* Check C1 */
	process_command(Stream, Line_C1, Type_C1),
	match_while(Type_Bool, Type_C1, Type_Out).


/* ASSIGN */ 
process_command(Stream, Line_Assign, Type_Out) :-
	/* split to var and arith parts */
	split_string(Line_Assign, ":", " =", [Line_Assign_Left|Line_Assign_Right]),
	not(Line_Assign_Left= "IF"),
	not(Line_Assign_Left= "WHILE"),
	not(Line_Assign_Left= "SKIP"),
	term_to_atom(Line_Assign_Left_Atom, Line_Assign_Left),
	class(Line_Assign_Left_Atom, Type_Left),
	atomics_to_string(Line_Assign_Right, Line_Assign_Right_Str),
	splitExpressionArith(Line_Assign_Right_Str, Type_Right),
	match_assign(Type_Left, Type_Right, Type_Out).

/* SEQ */
%process_command(Stream, Line_C1, Type_Out) :-
%	write("testing sequence\n"),
%	write(Line_C1),
%	readLine(Stream, Line_C2),					
%	\+ at_end_of_stream(Stream),
%	process_command(Stream, Line_C2, Type_C2),
%	match_seq(Type_C1, Type_C2, Type_Out).


/************ Code for matching *****************/
match_skip(In,Out):-
	In = Out.

match_skip(In,Out):-
	castUp(In, InUp),
	InUp = Out.

/* IF */
match_if(Bool, C1, C2, Out) :- /* all same class */
	Bool = C1, Bool = C2, Bool = Out.

match_if(Bool, C1, C2, Out) :- /* cast up bool */
	castUp(Bool, BoolUp), 
	BoolUp = C1, BoolUp = C2, BoolUp = Out.

match_if(Bool, C1, C2, Out) :- /* cast up bool and c1 */
	castUp(Bool, BoolUp), 
	castUp(C1, C1Up), 
	BoolUp = C1Up, BoolUp = C2, BoolUp = Out.

match_if(Bool, C1, C2, Out) :- /* cast up bool and c2 */
	castUp(Bool, BoolUp), 
	castUp(C2, C2Up), 
	BoolUp = C1, BoolUp = C2Up, BoolUp = Out.

match_if(Bool, C1, C2, Out) :- /* cast up bool, c1, and c2 */
	castUp(Bool, BoolUp), 
	castUp(C1, C1Up), 
	castUp(C2, C2Up), 
	BoolUp = C1Up, BoolUp = C2Up, BoolUp = Out.

/* ASSIGN */
match_assign(Left, Right, Out) :-
	Left = Right, Left = Out.

match_assign(Left, Right, Out) :-
	castUp(Right, RightUp),
	Left = RightUp, Left = Out.

match_assign(Left, Right, Out) :-
	castUp(Left, LeftUp),
	castUp(Right, RightUp),
	LeftUp = RightUp, LeftUp = Out.

/* WHILE */
match_while(Bool, Body, Out) :-
	Bool = Body, Bool = Out.

match_while(Bool, Body, Out) :-
	castUp(Bool, BoolUp),
	BoolUp = Body, BoolUp = Out.

match_while(Bool, Body, Out) :-
	castUp(Bool, BoolUp),
	castUp(Body, BodyUp),
	BoolUp = BodyUp, BoolUp = Out.

/* SEQ */
match_seq(C1, C2, Out) :-
	C1 = C2, C1 = Out.

match_seq(C1, C2, Out) :-
	castUp(C1, C1Up),
	C1Up = C2, C1Up = Out.

match_seq(C1, C2, Out) :-
	castUp(C2, C2Up),
	C1 = C2Up, C1 = Out.

match_seq(C1, C2, Out) :-
	castUp(C1, C1Up),
	castUp(C2, C2Up),
	C1Up = C2Up, C1Up = Out.

/************ MAIN CODE *****************/
main(FileName, Out) :-
	open(FileName, read, Stream),
	readFile(Stream, List, Out),
	close(Stream),
	write("CODE PASSES\n").


/************ FILE READING CODE *****************/
readFile(Stream, [], Type) :- 
	at_end_of_stream(Stream).
readFile(Stream, [H|T], Type) :-
	\+ at_end_of_stream(Stream),
	readLine(Stream, H),
	process_command(Stream, H, Type1),
	write("command processes\n"),
	readFile(Stream, T, Type2),
	match_seq(Type1, Type2, Type).


/* Code to read line by line */
readLine(Stream,W) :-
	get_code(Stream,Char),
	checkCharAndReadRest(Char,Chars,Stream),
	atom_codes(W,Chars),
	write(W), write("\n").
   
/* keep reading characters until a new line or other terminator */
checkCharAndReadRest(10,[],_):-  !. /* new line */
checkCharAndReadRest(-1,[],_):-  !. /* end of stream */
checkCharAndReadRest(end_of_file,[],_):-  !. /* end of file */
checkCharAndReadRest(Char,[Char|Chars],Stream):- /* default case */
    get_code(Stream,NextChar),
    checkCharAndReadRest(NextChar,Chars,Stream).

clean_string(In, Out) :-
	split_string(In, "", "\s\t\n", Out).
