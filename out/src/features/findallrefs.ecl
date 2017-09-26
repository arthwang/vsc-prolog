% findallrefs.ecl
:- use_module(library(source_processor)).
:- use_module(library(regex)).
:- use_module(load_file_module).

:- local variable(locations), 
		 variable(pred_imported),
		 variable(exported_here),
		 variable(this_module).

digout_predicate(File, Module : Pred / Arity) :-
	pre_process(File, Module : Pred / Arity),
	open(File, read, Stream),
	setval(locations, []),
	repeat,
	read_annotated(Stream, Term, AnnTerm),
	dig_predicate(Module : Pred / Arity, Term, AnnTerm),
	AnnTerm = annotated_term{type : end_of_file},
	getval(locations, Locations1),
	reverse(Locations1, Locations),
	printf("references:%w%n", [Locations]),
	close(Stream),
	!.
pre_process(File, Module : Pred / Arity) :-
	load_file_module(File, CurrMod),
	(
	    Module == CurrMod
	->
	    setval(exported_here, true)
	;
	    setval(exported_here, false)
	),
	setval(this_module, CurrMod),
	(
	    get_flag(Pred / Arity, definition_module, Module) @ CurrMod
	->
	    setval(pred_imported, true)
	;
	    setval(pred_imported, false)
	).


dig_predicate(_ : Pred / _, Term, _) :-
	term_string(Term, String),
	concat_string(["\\b", Pred, "\\b"], Pattern),
	\+ match(Pattern, String),
	!.
	
dig_predicate(_ : _ / Arity, Term, AnnTerm) :-
	\+ is_list(AnnTerm),
	Arity > 0,
	\+ compound(Term),
	!.
dig_predicate(_ : Pred / 0, Term, AnnTerm) :-
	\+ is_list(AnnTerm),
	Pred \= Term,
	\+ compound(Term),
	!.
	
dig_predicate(_, :- use_module(_), _) :-
	!.
dig_predicate(_, :- module(_), _) :-
	!.
dig_predicate(Module : Pred / Arity, :- import _ from Module, annotated_term{term : AnnTerm}) :-
	(
	    AnnTerm = (:- annotated_term{term : AnnTerm1}),
	    AnnTerm1 = (import annotated_term{term : AnnTerm2}),
	    AnnTerm2 = (annotated_term{term : Ps} from annotated_term{term : Module}),
	    locate_pred(Ps, Pred / Arity, From)
	->
	    update_locs(From)
	;
	    true
	),
	!.
dig_predicate(_ : Pred / _, :- import _ from Pred, _) :-
	!.
dig_predicate(_ : Pred / Arity, :- export _, annotated_term{term : (:- annotated_term{term : (export Ps1)})}) :-
	(
	    getval(exported_here, true),
	    Ps1 = annotated_term{term : Ps},
	    locate_pred(Ps, Pred / Arity, From)
	->
	    update_locs(From)
	;
	    true
	),
	!.

dig_predicate(Module : Pred / Arity, :- tool(Pred / Arity, _), annotated_term{term : (:- annotated_term{term : AnnTerm})}) :-
	tool_body(Pred / Arity, _, Module) @ Module,
	AnnTerm = tool(ATI, _),
	locate_pred(ATI, Pred / Arity, From),
	update_locs(From).
dig_predicate(Module : Pred / Arity, :- tool(_, Pred / Arity), annotated_term{term : (:- annotated_term{term : AnnTerm})}) :-
	getval(this_module, Module),
	AnnTerm = tool(_, ATB),
	locate_pred(ATB, Pred / Arity, From),
	update_locs(From).
dig_predicate(_, :- tool(_, _), _) :-
	!.
	

dig_predicate(Module : PredName / Arity, Module : Pred, annotated_term{term : annotated_term{term : Module} : annotated_term{(from) : From}}) :-
	functor(Pred, PredName, Arity),
	update_locs(From),
	!.
dig_predicate(_ : PredName / _, PredName : _, _) :-
	!.

dig_predicate(_ : PredName / Arity, Pred, annotated_term{type : Type, (from) : From}) :-
	(
	    nonvar(Pred),
	    functor(Pred, PredName, Arity),
	    getval(pred_imported, true)
	->
	    update_locs(From)
	;
	    Type \= compound
	),
	!.
dig_predicate(_, _, annotated_term{type : end_of_file}).

dig_predicate(Module : Pred / Arity, Term, annotated_term{term : AnnTerm, type : compound}) :-
	Term =.. TermList,
	AnnTerm =.. AnnList,
	dig_predicate(Module : Pred / Arity, TermList, AnnList),
	!.
dig_predicate(Module : Pred / Arity, [TH|TT], [AH|AT]) :-
	dig_predicate(Module : Pred / Arity, TH, AH),
	!,
	dig_predicate(Module : Pred / Arity, TT, AT).
dig_predicate(_, _, []) :-
	!.
    
update_locs(NewData) :-
	getval(locations, Value),
	setval(locations, [NewData|Value]).
	
locate_pred(PI, Pred / Arity, From) :-
	unified(PI, Pred / Arity, From),
	!.
locate_pred(PIs, Pred / Arity, From) :-
	PIs =.. [',', PI, NPIs],
	(
	    unified(PI, Pred / Arity, From),
	    !
	;
	    locate_pred(NPIs, Pred / Arity, From)
	).

unified(PI, Pred / Arity, From) :-
	PI = annotated_term{term : annotated_term{term : Pred, (from) : From} / annotated_term{term : Arity}},
	!.

unified(PI, Pred / Arity, From) :-
	PI = annotated_term{term : Pred, (from) : From} / annotated_term{term : Arity}.