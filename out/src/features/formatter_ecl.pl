% formatter_ecl.pl
:- use_module(library(source_processor)).

format_prolog_source(RangeTxt) :-
	open(string(RangeTxt), read, RStream),
	open(queue(""), update, commQueue),
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
		arg(term of source_term, SourceTerm, Term),
		writeln(Class:term:Term),
		(
			Class = comment
		->
			concat_atom(['COMMENTSBIGIN:::{"comments":[{"location":', Offset, ',"comment":"', Term, '"}]}:::COMMENTSEND'], CString),
			printf(commQueue, '%q', [CString])
		;
			arg(vars of source_term, SourceTerm, Vars),
			maplist(var_name, Vars, VarsNames),
			printf("TERMSEGMENTBEGIN:::%n", []),
			printf("TERMBEGIN:::%n", []),
			writeclause(Term),
			printf(":::TERMEND%n", []),
			(
				read_string(commQueue, end_of_file, _, Comms)
			-> 
				true
			;  
				Comms = 'COMMENTSBIGIN:::{"comments":[]}'
			),
			printf("%s%n", [Comms]),
			printf("VARIABLESBEGIN:::%w:::VARIABLESEND%n", [VarsNames]),
			printf("TERMPOSBEGIN:::%d:::TERMPOSEND%n", [Offset]),
			printf(":::TERMSEGMENTEND%n", [])
		)
	),
	source_close(SPend, []),
	close(RStream),
    writeln('::::::ALLOVER').

output_codes(SP1, SourceTerm) :-
	SP1 = source_position{offset:Offset},
	arg(term of source_term, SourceTerm, Term),
	(
		Class = comment
	->
		concat_atom(['COMMENTSBIGIN:::{"comments":[{"location":', Offset, ',"comment":"', Term, '"}]}:::COMMENTSEND'], CString),
		printf(commQueue, '%q', [CString])
	;
		arg(vars of source_term, SourceTerm, Vars),
		maplist(var_name, Vars, VarsNames),
		printf("TERMSEGMENTBEGIN:::%n", []),
		printf("TERMBEGIN:::%n", []),
		writeclause(Term),
		printf(":::TERMEND%n", []),
		(
			read_string(commQueue, end_of_file, _, Comms)
		-> 
			true
		;  
			Comms = 'COMMENTSBIGIN:::{"comments":[]}'
		),
		printf("%s%n", [Comms]),
		printf("VARIABLESBEGIN:::%w:::VARIABLESEND%n", [VarsNames]),
		printf("TERMPOSBEGIN:::%d:::TERMPOSEND%n", [Offset]),
		printf(":::TERMSEGMENTEND%n", [])
	).


var_name([Name|Var], VarName) :-
	term_string(Var, VarStr),
	VarName = VarStr:Name.
