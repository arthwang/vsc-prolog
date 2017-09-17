% formatter_ecl.pl
:- use_module(library(source_processor)).
:- local reference(non_clause_start, 0).

format_prolog_source(RangeTxt, DocTxt) :-
	load_modules(DocTxt),
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
			% param(RStream)
	do
		source_read(SP1, SP2, Class, SourceTerm),
		SP1 = source_position{offset:Offset},
		SP2 = source_position{offset:To},
		arg(term of source_term, SourceTerm, Term),
		(
			Class = clause
		->
			% getval(non_clause_start, NonClauseStart),
		  % (  NonClauseStart \= Offset
			% -> seek(RStream, NonClauseStart),
			% 	 StrLength is Offset - NonClauseStart,
			% 	 read_string(RStream, end_of_file, StrLength, CommStr),
			% 	 seek(RStream, To)
			% ;  CommStr = ""
			% ),
			arg(vars of source_term, SourceTerm, Vars),
			maplist(var_name, Vars, VarsNames),
			printf("TERMSEGMENTBEGIN:::%n", []),
			printf("TERMBEGIN:::%n", []),
			writeclause(Term),
			printf(":::TERMEND%n", []),
			% (  CommStr = ""
			% -> true
			% ;
			% 	concat_atom(['COMMENTSBIGIN:::{"comments":[{"location":', NonClauseStart, ',"comment":"', CommStr, '"}]}:::COMMENTSEND'], CString),
			% 	printf("%q%n", [CString])
			% ),
			printf("VARIABLESBEGIN:::%w:::VARIABLESEND%n", [VarsNames]),
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

load_modules(DocTxt) :-
	open(string(DocTxt), read, RStream),
	source_open(stream(RStream), [], SP0),
	(  fromto(start, _, Kind, end),
		 fromto(SP0, SP1, SP2, SPEnd)
	do
		 source_read(SP1, SP2, Kind, SourceTerm),
		 arg(term of source_term, SourceTerm, Term),
		 (  (   Term =.. [:- use_module(Module)]
				;   Term =.. [import, _ from Module]
				;   Term =.. [import, Module]
				) 
				->
					 writeln(moduleUsed:Module),
					 use_module(Module)
				;  true
		 )
	),
	source_close(SPEnd, []),
	close(RStream).

var_name([Name|Var], VarName) :-
		term_string(Var, VarStr),
		VarName = VarStr:Name.
