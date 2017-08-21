%% redefusemodule.pl
:- redefine_system_predicate(use_module(_)).
:- redefine_system_predicate(use_module(_, _)).
:- redefine_system_predicate(reexport(_)).
:- redefine_system_predicate(reexport(_, _)).
:- (dynamic user:active_file/2).
use_module(ModFiles) :-
    is_list(ModFiles), !,
    forall(member(File, ModFiles), use_module1(File)).
use_module(ModFiles) :-
    use_module1(ModFiles).
    
use_module1(ModFile) :-
    exists_prolog_file(ModFile, File),
    writeln(file:File),
    (   user:active_file(_, File)
    ;   load_files(ModFile, [if(not_loaded), must_be_module(true)])
    ).
use_module(ModFile, Imports) :-
    exists_prolog_file(ModFile, File),
    writeln(file:File),
    (   user:active_file(_, File)
    ;   load_files(ModFile,
                   [if(not_loaded), imports(Imports), must_be_module(true)])
    ).

exists_prolog_file(ModFile, File) :-
    absolute_file_name(ModFile,
                       File,
                       [expand(true), solutions(all), file_type(prolog)]),
    exists_file(File), !.

reexport1(ModFile) :-
    exists_prolog_file(ModFile, File),
    (   user:active_file(FileId, File),
        module_property(Module, file(FileId)),
        module_property(Module, exports(Exports)),
        forall(member(Export, Exports), export(Export))
    ;   load_files(ModFile,
                   [if(not_loaded), must_be_module(true), reexport(true)])
    ).
reexport(ModFiles) :-
    is_list(ModFiles), !,
    forall(member(File, ModFiles), reexport1(File)).
reexport(ModFiles) :-
    reexport1(ModFiles).
reexport(ModFile, Exports) :-
    exists_prolog_file(ModFile, File),
    (   user:active_file(_, File),
        forall(member(Export, Exports), export(Export))
    ;   load_files(ModFile,
                   
                   [ if(not_loaded),
                     must_be_module(true),
                     imports(Exports),
                     reexport(true)
                   ])
    ).


     
