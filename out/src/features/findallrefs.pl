%% findallrefs.pl
:- module(findrefs,[]).


:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(prolog_codewalk)).

:- meta_predicate findallrefs(:, +), findrefs(:, +).
:- (thread_local hit/1).
all_source_files_under_cwd(Files) :-
    working_directory(D, D),
    directory_source_files(D, Files, [recursive(true), if(false)]).
    

findallrefs(To, IncludingDefLoc) :-
    all_source_files_under_cwd(Files),
    forall(member(File, Files),
           setup_call_cleanup(load_files(File, [if(changed), silent(true)]),
                              findrefs(To, IncludingDefLoc),
                              unload_file(File))).

findrefs(To, IncludingDefLoc) :-
    resolve(To, Target),
    (   IncludingDefLoc==true
    ->  output_definition_locations(To)
    ;   true
    ),
    prolog_walk_code(
                     [ module_class([user]),
                       infer_meta_predicates(true),
                       trace_reference(Target),
                       on_trace(hit)
                     ]).

output_definition_locations(To) :-
    resolve(To, Target),
    output_definition_locations1(Target).

output_definition_locations1(Target) :-
    (   predicate_property(Target, built_in)
    ;   predicate_property(Target, foreign)
    ),
    writeln('{"reference":"built_in or foreign"}'), !.
output_definition_locations1(Target) :-
    nb_setval(export_located, null),
    clause(Target, _, CRef),
    clause_property(CRef, file(File)),
    clause_property(CRef, line_count(Line)),
    read_file_to_string(File, String, []),
    split_string(String, '\n', '', SubStrings),
    nth1(Line, SubStrings, LineTxt),
    Target=_:Pred,
    functor(Pred, Name, _),
    sub_string(LineTxt, Char, _, _, Name),
    Line1 is Line-1,
    output_file_line_char(File, Line1, Char),
    fail.
output_definition_locations1(Target) :-
    clause(Target, _, CRef),
    clause_property(CRef, file(File)),
    output_export_location(Target, File),
    writeln('{"reference":"definition location found"}'), !.
output_definition_locations1(_) :-
    writeln('{"reference":"definition location found"}'), !.

output_export_location(_, _) :-
    nb_getval(export_located, true), !.
output_export_location(Target, File) :-
    setup_call_cleanup(open(File, read, RStream),
                       ( repeat,
                         read_term(RStream,
                                   Term,
                                   [subterm_positions(STPos)]),
                         (   Term=end_of_file
                         ->   !
                         ;   Term=(:-module(_, Exports)), !,
                             nb_setval(export_located, true),
                             Target=_:PI,
                             functor(PI, PN, Arity),
                             nth1(Index, Exports, PN/Arity),
                             STPos=term_position(_, _, _, _, [term_position(_, _, _, _, [_, list_position(_, _, Elems, _)])]),
                             nth1(Index,
                                  Elems,
                                  term_position(_,
                                                _,
                                                _,
                                                _,
                                                [From-_, _])),
                             prolog_codewalk:filepos_line(File, From, Line, CharA),
                             Line1 is Line-1,
                             output_file_line_char(File, Line1, CharA)
                         )
                       ),
                       close(RStream)), !.
output_export_location(_, _).


    
    
resolve(M:Name/Arity, Target) :-
    atom(Name),
    integer(Arity), !,
    functor(Head, Name, Arity),
    resolve(M:Head, Target).
resolve(M:Head, Target) :-
    callable(Head), !,
    (   predicate_property(M:Head, imported_from(M2))
    ->  Target=M2:Head
    ;   Target=M:Head
    ).
resolve(To, _) :-
    type_error(callable, To).

hit(_Callee, _Caller, Location) :-
    phrase(prolog:message_location(Location), [_-Locs]),
    (length(Locs, 3)->Locs=[File,Line,Char];Locs=[File,Line],Char is 0),
    Line1 is Line - 1,
    output_file_line_char(File, Line1, Char).

output_file_line_char(File, _, _) :-
    module_property(findrefs, file(File)), !.
output_file_line_char(File, Line, Char) :-
    Dict=_{reference:_{char:Char, file:File, line:Line}},
    atom_json_dict(DictText1, Dict, []),
    split_string(DictText1, '\n', ' \t', Subs),
    atomic_list_concat(Subs, DictText),
    format('~w~n', DictText).
