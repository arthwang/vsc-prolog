%% snippets_from_sources.pl
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(regex)).
:- use_module(library(http/json)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- (dynamic dir_handled/1, module_spec/2).

:- (volatile dir_handled/1).

% start :-
%     generate_vscode_swipl_snippets([]).

generate_vscode_swipl_snippets(Options) :-
    option(detailed_description(TrueOrFalse), Options, true),
    nb_setval(detailed_description, TrueOrFalse),
    option(dict_in(DictIn), Options, _{}),
    option(json_file(SnippetsFile1), Options, './snippets/prolog.swi.json'),
    absolute_file_name(SnippetsFile1, SnippetsFile),
    nb_linkval(snippet_dict, DictIn),
    retractall(dir_handled(_)),
    retractall(module_spec(_)),
    writeln("Handling source files ..."),
    traverse_dirs,
    writeln("Handling html files ..."),
    option(doc_dirs(DirSpecs), Options, [swi(doc)]),
    vscode_swipl_snippets_from_html_under_dirs(DirSpecs),
    nb_getval(snippet_dict, ResltDict),
    write_to_json_file(SnippetsFile, ResltDict).

%% ----------------------------------------------------
traverse_dirs :-
    file_search_path(library, Dir0),
    forall(absolute_file_name(Dir0,
                              Dir,
                              
                              [ expand(true),
                                file_type(directory),
                                file_errors(fail),
                                solutions(all)
                              ]),
           handle_dir(Dir)),
    fail.
traverse_dirs.

handle_dir(Dir) :-
    dir_handled(Dir), !.
handle_dir(Dir) :-
    assert(dir_handled(Dir)),
    ensure_slash(Dir, DirS), !,
    writeln('Enter directory':Dir),
    setup_call_cleanup(working_directory(Old, DirS),
                       handle_dir,
                       working_directory(_, Old)),
    writeln(Dir:' over.'), !.

handle_dir :-
    get_plfiles(Files),
    forall(member(File, Files), vscode_swipl_snippets_from_source(File)),
    recurse_subdirs.

recurse_subdirs :-
    exists_file('./MKINDEX.pl'), !.
recurse_subdirs :-
    directory_files('.', Entries1),
    delete(Entries1, '.', Entries2),
    delete(Entries2, .., Entries),
    maplist(absolute_file_name, Entries, AbsoluteEntries),
    include(exists_directory, AbsoluteEntries, SubDirs),
    forall(member(SubDir, SubDirs), handle_dir(SubDir)).

get_plfiles(Files) :-
    absolute_file_name('MKINDEX.pl', F),
    access_file(F, read),
    open("MKINDEX.pl", read, MkIndx),
    read(MkIndx,  (:-make_library_index(_, Patterns))),
    pattern_files(Patterns, Files),
    close(MkIndx), !.
get_plfiles(Files) :-
    findall(Pattern, source_file_pattern(Pattern), Patterns),
    pattern_files(Patterns, Files).

source_file_pattern(Pattern) :-
    user:prolog_file_type(PlExt, prolog),
    atom_concat(*., PlExt, Pattern).

pattern_files([], []).
pattern_files([H|T], Files) :-
    expand_file_name(H, Files0),
    '$append'(Files0, Rest, Files),
    pattern_files(T, Rest).

ensure_slash(Dir, DirS) :-
    (   sub_atom(Dir, _, 1, 0, /)
    ->  DirS=Dir
    ;   atom_concat(Dir, /, DirS)
    ).

vscode_swipl_snippets_from_source(FileSpec) :-
    format("Handling file: ~w~n", FileSpec),
    doc_file_objects(FileSpec, _, Objects, FileOptions, []),
    option(module(Module), FileOptions),
    option((public Exports), FileOptions),
    maplist(digout_predicate_indicator, Objects, Objects1),
    flatten(Objects1, Objects2),
    exclude(exported_pred_in_objects(Module, Objects2), Exports, Objects3),
    maplist(add_nocomment_predicate(Module), Objects3, Objects4),
    append(Objects, Objects4, NewObjects),
    write_objects(Module, Exports, NewObjects), !.
vscode_swipl_snippets_from_source(_).

add_nocomment_predicate(Module,Pred,doc(Module:Pred, null, Comm)) :-
    Pred=Fac/Arity,
    gen_params(Arity, 1, Params),
    Predicate=..[Fac|Params],
    term_to_atom(Predicate, Comm).

gen_params(Arity, Num, [H|T]) :-
    Num=<Arity, !,
    atomic_list_concat(['Param', Num], H),
    Num1 is Num+1,
    gen_params(Arity, Num1, T).
gen_params(_, _, []).

export_with_module(Module, Export, Module:Export).

exported_pred_in_objects(Module, PredsInObjs, ExportedPred) :-
    memberchk(Module:ExportedPred, PredsInObjs).

digout_predicate_indicator(Doc, PI) :-
    Doc=doc(PI, _, _).

isExportedPredicate(Exports, PI) :-
    PI=_:PI1,
    memberchk(PI1, Exports).

write_objects(Module,
                          Exports,
                          [doc(PI, Pos, Comment)|T]) :-
    is_list(PI), !,
    forall(member(PIMember, PI),
           write_objects1(Module, Exports, PIMember, Pos, Comment)),
    write_objects(Module, Exports, T).
write_objects(Module,
                          Exports,
                          [doc(PI, Pos, Comment)|T]) :-
    write_objects1(Module, Exports, PI, Pos, Comment),
    write_objects(Module, Exports, T).
write_objects(Module, Exports, [_|T]) :-
    write_objects(Module, Exports, T), !.
write_objects(_, _, []).

write_objects1(_, Exports, PI, _, _) :-
    \+ isExportedPredicate(Exports, PI), !.
write_objects1(_, _, PI, _, _) :-
    PI=_:Pred/_,
    % exclude these kinds of predicates in snippets
    Pred=~"[()~!@#:.%&{\\\\[/+\\-<>?= ]", !.
write_objects1(Module, _, PI, Pos, Comment) :-
    PI=Module:Pred/Arity,
    (   atom_concat($, _, Pred)
    ->  atom_concat(\, Pred, Pred1)
    ;   Pred1=Pred
    ),
    gen_snippet_body(Pred1, Arity, Comment, Body),
    uncomment_comment(Comment, UnComm),
    (   nb_getval(detailed_description, false)
    ->  (   Pos==null
        ->  Desc=Comment
        ;   parse_comment(Comment, Pos, Parsed),
            memberchk(predicate(_, Summary, _), Parsed),
            (   Arity=:=0
            ->  Desc=Summary
            ;   A1 is Arity-1,
                atomic_list_concat(
                                   [ " *(",
                                     Pred,
                                     "\\([^,]+(,[^),]+){",
                                     A1,
                                     "}\\)[^.]*\\.)"
                                   ],
                                   Pattern),
                regex_str(Pattern, [], UnComm, [PredSig|_]),
                atomic_list_concat([PredSig, '\t', Summary], Desc)
            )
        )
    ;   Desc=UnComm
    ),
    dict_create(Snippet,
                _,
                [prefix:Pred, body:[Body], description:Desc]),
    term_to_atom(PI, PIA),
    shortest_module_spec(Module, Subs),
    atomic_list_concat([Subs, PIA], Key1),
    atom_concat(/, Key, Key1),
    nb_getval(snippet_dict, DictIn),
    put_dict(Key, DictIn, Snippet, NewDict),
    nb_linkval(snippet_dict, NewDict).

shortest_module_spec(Module, Subs) :-
    module_spec(Module, Subs), !.
shortest_module_spec(Module, Subs) :-
    working_directory(Dir, Dir),
    file_search_path(library, Path),
    absolute_file_name(Path, Abs),
    sub_atom(Dir, 0, _, After, Abs),
    (   module_spec(Module, Subs1),
        atom_length(Subs1, Len1),
        After<Len1,
        retract(module_spec(Module, _))
    ;   \+ module_spec(Module, _)
    ),
    atom_length(Abs, Start),
    sub_atom(Dir, Start, _, 0, Subs),
    assert(module_spec(Module, Subs)),
    fail.
shortest_module_spec(Module, Subs) :-
    module_spec(Module, Subs).

is_valid_directory(Dir, FileName) :-
    FileName\=('.'),
    FileName\= ..,
    directory_file_path(Dir, FileName, Path),
    exists_directory(Path).

valid_subdirs(Parent, SubDirs) :-
    directory_files(Parent, Entries1),
    include(is_valid_directory(Parent), Entries1, SubDirs).

is_in_subdir(Module, BaseDir, ParentDir, RelativePath) :-
    atomic_list_concat([BaseDir, ParentDir, /, Module], FullPath),
    file_name_extension(FullPath, '.pl', Name),
    (   exists_file(Name)
    ;   is_in_different_file(BaseDir, ParentDir, Module)
    ),
    atomic_list_concat([ParentDir, /, Module], RelativePath), !.
is_in_subdir(Module, BaseDir, ParentDir, RelativePath) :-
    atomic_list_concat([BaseDir, /, ParentDir], NewBase),
    valid_subdirs(NewBase, SubDirs),
    member(SubDir, SubDirs),
    atomic_list_concat([ParentDir, /, SubDir], NewParent),
    is_in_subdir(Module, BaseDir, NewParent, RelativePath).

is_in_different_file(BaseDir, ParentDir, Module) :-
    atomic_list_concat([BaseDir, ParentDir], Dir),
    directory_files(Dir, Files),
    member(File, Files),
    file_name_extension(_, '.pl', File),
    directory_file_path(Dir, File, FullName),
    setup_call_cleanup(open(FullName, read, FS),
                       ( catch(read(FS,  (:-module(Module, _))),
                               _,
                               fail), !
                       ),
                       close(FS)).

uncomment_comment(Comment, UnComm) :-
    (   Comment=~"^%!"
    ;   Comment=~"^%%"
    ), !,
    sub_string(Comment, 2, _, 0, Comm0),
    atomic_replace(Comm0, "\n%!", "\n", Comm1),
    atomic_replace(Comm1, "\n%%", "\n", Comm2),
    atomic_replace(Comm2, "\n%", "\n", UnComm).
uncomment_comment(Comm, Comm).

gen_snippet_body(Pred, 0, _, Body) :-
    atomic_list_concat([Pred, "$1\n$0"], Body), !.
gen_snippet_body(Pred, Arity, Comm, Body) :-
    regex_str("([^(]*\\([^)]+\\))+", [s], Comm, Params),
    get_param_list(Arity, Params, ParamList),
    params_to_snippet(ParamList, 1, "", Snippet),
    End is Arity+1,
    atomic_list_concat([Pred, "(", Snippet, ")$", End, "\n$0"], Body).

get_param_list(Arity, [H|_], ParamList) :-
    regex_str(".*\\(([^)]+)\\)", [], H, [Params]),
    split_string(Params, ",", " ", ParamList),
    length(ParamList, Arity), !.
get_param_list(Arity, [_|T], ParamList) :-
    get_param_list(Arity, T, ParamList), !.
get_param_list(_, [], _) :-
    fail.

vscode_swipl_snippets_from_html_under_dirs(DirSpecs) :-
    get_html_files_under_dirs(DirSpecs, HtmlFiles),
    forall(member(File, HtmlFiles), vscode_swipl_snippets_from_html(File)).

get_html_files_under_dirs([DirSpec|DT], Files) :-
    get_html_files_under_dir(DirSpec, Files1),
    get_html_files_under_dirs(DT, Files2),
    append(Files1, Files2, Files).
get_html_files_under_dirs([], []).

get_html_files_under_dir(DirSpec, Files) :-
    absolute_file_name(DirSpec, Dir1), !,
    atomic_list_concat([Dir1, '/*'], Pattern),
    expand_file_name(Pattern, Subs),
    get_html_files_under_dir1(Subs, Files).

get_html_files_under_dir1([H|T], Files) :-
    exists_directory(H),
    get_html_files_under_dir(H, Files1),
    get_html_files_under_dir1(T, Files2),
    append(Files1, Files2, Files), !.
get_html_files_under_dir1([H|T], [H|TT]) :-
    file_name_extension(_, html, H),
    get_html_files_under_dir1(T, TT), !.
get_html_files_under_dir1([_|T], Files) :-
    get_html_files_under_dir1(T, Files), !.
get_html_files_under_dir1([], []).

vscode_swipl_snippets_from_html(HtmlFile) :-
    writeln("Handling html file":HtmlFile),
    nb_linkval(current_snippet, null),
    load_html(HtmlFile, Dom, []),
    nb_setval(current_module, null),
    filter_dom(Dom, Filtered),
    Filtered\=[],
    handle_contents(Filtered), !.
vscode_swipl_snippets_from_html(_).

handle_contents([element(module, _, [Module])|T]) :-
    nb_setval(current_module, Module), !,
    handle_contents(T).
handle_contents([element(dl, _, Contents)|T]) :-
    handle_dt(Contents),
    handle_contents(T).
handle_contents([]).

filter_dom(Dom, Filtered) :-
    xpath(Dom, //body, element(body, _, Subs)),
    filter_dom1(Subs, Filtered).

filter_dom1([H|T], Filtered) :-
    (   H=element(h2, _, _)
    ;   H=element(h3, _, _)
    ), !,
    xpath(H, /self(text), Text),
    (   (   regex_str("library\\(([^).]+)\\)", [], Text, [Mod])
        ;   regex_str("([^ .]+)\\.pl:", [], Text, [Mod])
        )
    ->  FH=element(module, [], [Mod]),
        filter_dom1(T, FT),
        Filtered=[FH|FT]
    ;   filter_dom1(T, Filtered)
    ), !.
filter_dom1([H|T], [H|FT]) :-
    H=element(dl, _, [element(dt, [class=pubdef], _)|_]), !,
    filter_dom1(T, FT).
filter_dom1([_|T], Filtered) :-
    filter_dom1(T, Filtered).
filter_dom1([], []).

handle_dt([H|T]) :-
    H=element(dt, _, _), !,
    xpath(H, /self(normalize_space), Pred1),
    atomic_replace(Pred1, "\n", "", Pred2),
    regex_str("^(\\[.+])?(.+)", [], Pred2, Pred3),
    (   length(Pred3, 2)
    ->  reverse(Pred3, R),
        atomic_list_concat(R, Pred)
    ;   Pred3=[Pred]
    ),
    regex_str("^([^(]+)(\\((.+)\\))?", [], Pred, [Factor|Params1]),
    (   Params1==[]
    ->  atomic_list_concat([Factor, "$1\n$0"], SnippetBody),
        Arity is 0
    ;   Params1=[_, Params],
        split_string(Params, ",", " ", ParamList),
        length(ParamList, Arity),
        params_to_snippet(ParamList, 1, "", SnippetBody1),
        End is Arity+1,
        atomic_list_concat([Factor, '(', SnippetBody1, ")$", End, "\n$0"], SnippetBody)
    ),
    (   regex_str("^([-: ]+)(.+)?", [], Factor, Factor1)
    ->  Factor1=[_, Factor2]
    ;   Factor2=Factor
    ),
    atomic_list_concat([Pred2, '.\n'], InitDesc),
    dict_create(SnipDict,
                _,
                [prefix:Factor2, body:SnippetBody, description:InitDesc]),
    nb_getval(current_snippet, CurrSnippet),
    (   CurrSnippet==null
    ->  true
    ;   update_snippet_dict
    ),
    nb_linkval(current_snippet, SnipDict),
    atom_string(FactorA, Factor),
    term_to_atom(FactorA/Arity, FA),
    nb_setval(current_pred, FA),
    handle_dt(T), !.
handle_dt([H|T]) :-
    H=element(dd, _, _), !,
    (   nb_getval(detailed_description, true)
    ->  xpath(H, /self(content), Content),
        format_description(1, 4, Content, NewContent),
        NewDD=element(dd, [], NewContent),
        xpath(NewDD, /self(text), Desc1),
        atomic_replace(Desc1, '\n', ' ', Desc2),
        atomic_replace(Desc2, '!@#', '\n', Desc)
    ;   xpath(H, /self(text), Text),
        (   regex_str("^([^.]+\\.)", [], Text, [Text1]),
            atomic_replace(Text1, "\n", "", Desc)
        ;   Desc=''
        )
    ),
    nb_getval(current_snippet, SnipDict),
    get_dict(description, SnipDict, OldDesc),
    atomic_list_concat([OldDesc, Desc], NewDesc),
    nb_set_dict(description, SnipDict, NewDesc),
    handle_dt(T), !.
handle_dt([_|T]) :-
    handle_dt(T), !.
handle_dt([]) :-
    update_snippet_dict.

format_description(Level, FinalLevel, Content, FinalContent) :-
    Level=<FinalLevel,
    scan_content(Level, Content, NewContent),
    NextLevel is Level+1,
    format_description(NextLevel, FinalLevel, NewContent, FinalContent).
format_description(_, _, FinalContent, FinalContent) :- !.

scan_content(Level, [H|T], [HH|TT]) :-
    add_newline(Level, H, HH), !,
    scan_content(Level, T, TT), !.
scan_content(Level, [H|T], [HH|TT]) :-
    H=element(Tag, Attrs, Content),
    scan_content(Level, Content, NewContent),
    HH=element(Tag, Attrs, NewContent),
    scan_content(Level, T, TT), !.
scan_content(Level, [H|T], [H|TT]) :-
    scan_content(Level, T, TT), !.
scan_content(_, [], []).

add_newline(_, Elem, Text) :-
    Elem=element(pre, _, _),
    xpath(Elem, /self(text), Text1),
    atomic_replace(Text1, "\n", "!@#", Text2),
    atomic_list_concat(['!@#!@#', Text2, '!@#!@#'], Text).
add_newline(Level, Elem, Text) :-
    Elem=element(Tag, _, _),
    tag_need_separator(Level, Tag, Sep), !,
    xpath(Elem, /self(text), Text1),
    atomic_list_concat([Text1, Sep], Text).

tag_need_separator(1, li, '!@#').
tag_need_separator(1, pre, '!@#!@#').
tag_need_separator(2, ul, '!@#').
tag_need_separator(2, ol, '!@#').
tag_need_separator(3, dd, '!@#!@#').
tag_need_separator(4, dt, ': ').
tag_need_separator(4, p, '!@#!@#').

update_snippet_dict :-
    nb_getval(current_snippet, SnippetItem),
    update_snippet_dict(SnippetItem).

update_snippet_dict(SnippetItem) :-
    get_dict(prefix, SnippetItem, Prefix),
    Prefix=~"[()~!@#$:.%&{\\\\[/+\\-<>?= ]", !.
update_snippet_dict(SnippetItem) :-
    nb_getval(snippet_dict, DictIn),
    nb_getval(current_pred, Pred1),
    nb_getval(current_module, ModSpec),
    (   ModSpec==null
    ->  Pred=Pred1
    ;   atomic_list_concat([ModSpec, ":", Pred1], Pred)
    ),
    put_dict(Pred, DictIn, SnippetItem, NewDict),
    nb_linkval(snippet_dict, NewDict).

write_to_json_file(File, Dict) :-
    open(File, write, WStream),
    json_write_dict(WStream, Dict),
    close(WStream).

params_to_snippet([PH|PT], ParamNum, CurrSnippet, Snippet) :-
    regex_str("([^+\\-?:@ !]+)", [], PH, [Param]),
    (   CurrSnippet==""
    ->  Curr1=CurrSnippet
    ;   atomic_list_concat([CurrSnippet, ", "], Curr1)
    ),
    atomic_list_concat([Curr1, "${", ParamNum, ":", Param, "}"], NewSnippet),
    NextNum is ParamNum+1,
    params_to_snippet(PT, NextNum, NewSnippet, Snippet).
params_to_snippet([], _, Snippet, Snippet).

regex_str(Pattern, Options, String, RetStrs) :-
    regex(Pattern, Options, String, Codes),
    maplist(string_codes, RetStrs1, Codes),
    reverse(RetStrs1, RetStrs).

atomic_replace(Term, SubAtomic, ReplacedWith, NewTerm) :-
    atomic(Term),
    atomic_list_concat(TermList, SubAtomic, Term),
    atomic_list_concat(TermList, ReplacedWith, NewTerm).

find_index(File) :-
    file_search_path(_, Path1),
    absolute_file_name(Path1, Path),
    make_library_index(Path),
    atomic_list_concat([Path, "/INDEX.pl"], Pattern),
    expand_file_name(Pattern, [File]),
    exists_file(File).

print_list(List) :-
    is_list(List), !,
    forall(member(Elem, List), writeln(Elem)).
print_list(NotList) :-
    writeln(NotList).