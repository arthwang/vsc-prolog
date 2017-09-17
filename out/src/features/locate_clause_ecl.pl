:- use_module(library(source_processor)).
:- import matchsub / 4 from regex.

source_location(DocFile, PI) :-
	locate_clause_in_file(DocFile, PI, File, Line),
	printf("File:%s;Line:%d%n", [File, Line]).

locate_clause_in_file(DocFile, Name / Arity, FullName, Line) :-
	(   FullName = DocFile
	;
	    get_flag(Name / Arity, source_file, FullName)
	;
	    get_flag(Name / Arity, definition_module, Module),
	    current_compiled_file(File, _, Module),
	    matchsub("(.+)\\.eco", File, [], [BaseName]),
	    (
		concat_strings(BaseName, ".ecl", FullName)
	    ;
		concat_strings(BaseName, ".pl", FullName)
	    )
	),
	exists(FullName),
	source_open(FullName, [keep_comments, ignore_conditionals, no_macro_expansion, no_clause_expansion], SP0),
	(
	    fromto(p / a / l, _, P / A / L, Name / Arity / Line),
	    fromto(SP0, SP1, SP2, SPEnd)
	do
	    source_read(SP1, SP2, Kind, SourceTerm),
	    Kind \= end,
	    arg(term of source_term, SourceTerm, Term),
	    (
		Term =.. [:-, Head|_]
	    ->
		functor(Head, P, A)
	    ;
		functor(Term, P, A)
	    ),
	    SP1 = source_position{line : L}
	),
	!,
	source_close(SPEnd, []).
	   