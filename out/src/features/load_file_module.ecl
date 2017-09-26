% load_file_module.ecl
:- module(load_file_module).
:- export load_file_module / 2.
:- use_module(library(regex)).
:- tool(load_file_module / 2, load_file_module / 3).

load_file_module(File, Module, CModule) :-
	open(File, read, Stream),
	read_string(Stream, end_of_file, _, String),
	(
	    matchsub("^\\s*:-\\s*module\\s*\\(\\s*(\\w+)\\s*\\)", String, [newline], [ModS])
	->
	    atom_string(Module, ModS),
	    use_module(File) @ CModule
	;
	    ensure_loaded(File) @ CModule,
	    Module = eclipse
	),
	close(Stream).