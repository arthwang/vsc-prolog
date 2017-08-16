:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(prolog_codewalk)).

:- meta_predicate findallrefs(:).
:- (thread_local hit/1).
all_source_files_under_cwd(Files) :-
    working_directory(D, D),
    directory_source_files(D, Files, [recursive(true), if(false)]).
    

findallrefs(To) :-
    nb_linkval(ref_locations, _{locs:[]}),
    all_source_files_under_cwd(Files),
    forall(member(File, Files),
           setup_call_cleanup(load_files(File),
                              findrefs(To),
                              unload_file(File))).

findrefs(To) :-
    resolve(To, Target),
    prolog_walk_code(
                     [ module_class([user]),
                       infer_meta_predicates(true),
                       trace_reference(Target),
                       on_trace(hit)
                     ]),
    nb_getval(ref_locations, RefLocs),
    atom_json_dict(DictText, RefLocs, []),
    format('referencs:~w~n', [DictText]).
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
    Dict = _{file:File,line:Line,char:Char},
    nb_getval(ref_locations, LocsDict),
    NewLocs = [Dict|LocsDict.locs],
    nb_link_dict(locs, LocsDict, NewLocs).