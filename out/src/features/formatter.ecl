% formatter.ecl
:- use_module(library(source_processor)).
:- use_module(load_modules).

:- local reference(non_clause_start, 0).

format_prolog_source(RangeTxt, DocTxt) :-
	load_modules_from_text(DocTxt),
	open(string(RangeTxt), read, RStream),
	source_open(stream(RStream), [
		keep_comments,
		include_comment_files,
		ignore_conditionals,
		no_macro_expansion,
		no_clause_expansion
	], SP0),
	(
	    fromto(begin, _, Class, end),
			fromto(SP0, SP1, SP2, SPend)
	do
		source_read(SP1, SP2, Class, SourceTerm),
		SP1 = source_position{offset:Offset},
		SP2 = source_position{offset:To},
		arg(term of source_term, SourceTerm, Term),
		(
		(Class = clause; Class = directive; Class = handled_directive)
		->
			arg(vars of source_term, SourceTerm, Vars),
			maplist(var_name, Vars, VarsNames),
			printf("TERMSEGMENTBEGIN:::%n", []),
			printf("TERMBEGIN:::%n", []),
			writeclause(Term),
			printf(":::TERMEND%n", []),
			write("VARIABLESBEGIN:::"),
			write_term(VarsNames, [depth(full)]),
			writeln(":::VARIABLESEND"),
			printf("TERMPOSBEGIN:::%d:::TERMPOSEND%n", [Offset]),
			printf("TERMENDBEGIN:::%d:::TERMENDEND%n", [To]),
			printf(":::TERMSEGMENTEND%n", []),
			setval(non_clause_start, To)
		;
		  true
		)
	),
	source_close(SPend, []),
	close(RStream),
	writeln('::::::ALLOVER').

var_name([Name|Var], VarName) :-
		term_string(Var, VarStr),
		VarName = VarStr:Name.
