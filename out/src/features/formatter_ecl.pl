% formatter_ecl.pl
:- use_module(library(source_processor)).
:- local reference(non_clause_start, 0).

format_prolog_source(RangeTxt) :-
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
			fromto(SP0, SP1, SP2, SPend),
			param(RStream)
	do
		source_read(SP1, SP2, Class, SourceTerm),
		SP1 = source_position{offset:Offset},
		SP2 = source_position{offset:To},
		arg(term of source_term, SourceTerm, Term),
		writeln(Class:term:Term:Offset-To),
		(
			Class \= comment
		->
			getval(non_clause_start, NonClauseStart),
			writeln(noncs:NonClauseStart:ofs:Offset),
		  (  NonClauseStart \= Offset
			-> seek(RStream, NonClauseStart),
				 StrLength is Offset - NonClauseStart,
				 writeln(strlen:StrLength),
				 read_string(RStream, end_of_file, StrLength, CommStr),
				 seek(RStream, To)
			;  CommStr = ""
			),
			arg(vars of source_term, SourceTerm, Vars),
			maplist(var_name, Vars, VarsNames),
			printf("TERMSEGMENTBEGIN:::%n", []),
			printf("TERMBEGIN:::%n", []),
			writeclause(Term),
			printf(":::TERMEND%n", []),
			(  CommStr = ""
			-> true
			;
				concat_atom(['COMMENTSBIGIN:::{"comments":[{"location":', NonClauseStart, ',"comment":"', CommStr, '"}]}:::COMMENTSEND'], CString),
				printf("%q%n", [CString])
			),
			printf("VARIABLESBEGIN:::%w:::VARIABLESEND%n", [VarsNames]),
			printf("TERMPOSBEGIN:::%d:::TERMPOSEND%n", [Offset]),
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
