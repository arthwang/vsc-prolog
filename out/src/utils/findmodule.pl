find_module(_, PredHead, Module) :-
    PredHead=Module:_, !.
find_module(FileName, PredHead, Module) :-
    atom(FileName),
    load_files(FileName),
    (   clause(PredHead, _, CRef),
        clause_property(CRef, module(Module))
    ;   source_file_property(FileName, module(Module))
    ), !.
find_module(_, _, user).
    
    

