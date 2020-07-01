:- module(helpers).
:- export exitError/1.
:- export exitError/2.
:- export run_test/2.

exitError(Message) :-
    writeln(stderr,Message),
    throw(error).

exitError(Format,Args) :-
	sprintf(Message,Format,Args),
	exitError(Message).

run_test(Test, FileN) :-
   (call(Test)@eclipse -> C = "Ok!\n" ; C = "!!!!!!!!! Fail !!!!!!!\n"),
   split_string(FileN,""," ",[FileNSan]),
   open(FileNSan,write,F),
   write(F, C),
   close(F). 
