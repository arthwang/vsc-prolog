% load_modules.ecl
:- module(load_modules).
:- export load_modules_from_file / 1, load_modules_from_text / 1.
:- use_module(library(source_processor)).
:- tool(load_modules_from_file / 1, load_modules_from_file / 2).
:- tool(load_modules_from_text / 1, load_modules_from_text / 2).

load_modules_from_file(FileName, Module) :-
	load_modules(FileName, file, Module).

load_modules_from_text(Text, Module) :-
	load_modules(Text, text, Module).

load_modules(Source, Type, Module) :-
	set_stream(error, null),
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
	    fromto(SP0, SP1, SP2, SPEnd),
	    param(Module)
	do
	    source_read(SP1, SP2, Kind, SourceTerm),
	    arg(term of source_term, SourceTerm, Term),
	    load_module(Term, Module)
	),
	source_close(SPEnd, []),
	(
	    nonvar(RStream)
	->
	    close(RStream)
	;
	    true
	),
	set_stream(error, stderr).

load_module(Term, Module) :-
	Term =.. [:-, lib(Lib)],
	lib(Lib) @ Module,
	!.
load_module(Term, Module) :-
	Term =.. [:-, use_module(Mod)],
	use_module(Mod) @ Module,
	!.
load_module(Term, Module) :-
	Term =.. [:-, import _ from Mod],
	use_module(Mod) @ Module,
	!.
load_module(Term, Module) :-
	Term =.. [:-, import Mod],
	use_module(Mod) @ Module,
	!.
load_module(_, _).



  