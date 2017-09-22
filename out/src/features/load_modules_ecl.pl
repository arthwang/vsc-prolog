% load_modules_ecl.pl
:- module(load_modules_ecl).
:- export load_modules_from_file/1,
          load_modules_from_text/1.
:- use_module(library(source_processor)).

load_modules_from_file(FileName) :-
	load_modules(FileName, file).

load_modules_from_text(Text) :-
	load_modules(Text, text).
load_modules(Source, Type) :-
	(
	    Type = text
	->
	    open(string(Source), read, RStream),
	    source_open(stream(RStream), [], SP0)
	;
	    source_open(Source, [], SP0)
	),
	(
	    fromto(start, _, Kind, end),
	    fromto(SP0, SP1, SP2, SPEnd)
	do
	    source_read(SP1, SP2, Kind, SourceTerm),
	    arg(term of source_term, SourceTerm, Term),
	    load_module(Term)
	),
	source_close(SPEnd, []),
	(
	    nonvar(RStream)
	->
	    close(RStream)
	;
	    true
	).

load_module(Term) :-
	Term = lib(Lib),
	lib(Lib),
	!.
load_module(Term) :-
	Term =.. [:-, use_module(Module)],
	use_module(Module),
	!.
load_module(Term) :-
	Term =.. [:-, import _ from Module],
	use_module(Module),
	!.
load_module(Term) :-
	Term =.. [:-, import Module],
	use_module(Module),
	!.
load_module(_).



  