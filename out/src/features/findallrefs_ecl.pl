% findallrefs_ecl.pl
:- use_module(library(source_processor)).
:- use_module(library(regex)).
:- local variable(locations), variable(pred_imported).

digout_predicate(File, Module : Pred / Arity) :-
	open(File, read, Stream),
	pre_process(File, Module : Pred / Arity, Stream),
	setval(locations, []),
	repeat,
	read_annotated(Stream, AnnTerm),
	dig_predicate(Module : Pred / Arity, AnnTerm),
	AnnTerm = annotated_term{type : end_of_file},
	getval(locations, Locations1),
	reverse(Locations1, Locations),
	writeln(references : Locations),
	close(Stream),
	!.
pre_process(File, Module : Pred / Arity, Stream) :-
	read_string(Stream, end_of_file, _, String),
	(
	    matchsub("^\\s*:-\\s*module\\s*\\(\\s*(\\w+)\\s*\\)", String, [newline], [ModS])
	->
	    atom_string(ModA, ModS),
	    use_module(File),
	    CurrMod = ModA
	;
	    ensure_loaded(File),
	    CurrMod = eclipse
	),
	(
	    get_flag(Pred / Arity, definition_module, Module) @ CurrMod
	->
	    setval(pred_imported, true)
	;
	    setval(pred_imported, false)
	),
	seek(Stream, 0).


dig_predicate(_, AnnTerm) :-
	\+ is_list(AnnTerm),
	functor(AnnTerm, Func, _),
	Func \= annotated_term,
	!.
dig_predicate(Pred / Arity, annotated_term{term : AT1 / AT2, type : compound}) :-
	(
	    AT1 = annotated_term{term : Pred, (from) : From},
	    AT2 = annotated_term{term : Arity},
	    getval(pred_imported, true),
	    update_locs(From)
	;
	    true
	).

dig_predicate(Pred / Arity, AnnTerm) :-
	AnnTerm = annotated_term{term : Term, type : Type, (from) : From},
	(
	    nonvar(Term),
	    functor(Term, Pred, Arity)
	->
	    update_locs(From)
	;
	    Type \= compound
	).
dig_predicate(_, annotated_term{type : end_of_file}).

dig_predicate(Pred / Arity, annotated_term{term : Term, type : compound}) :-
	Term =.. AnnList,
	dig_predicate(Pred / Arity, AnnList).
dig_predicate(Pred / Arity, [H|T]) :-
	dig_predicate(Pred / Arity, H),
	!,
	dig_predicate(Pred / Arity, T).
dig_predicate(_, []) :-
	!.
    
update_locs(NewData) :-
	getval(locations, Value),
	setval(locations, [NewData|Value]).
	
	

