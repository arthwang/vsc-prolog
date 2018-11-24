/**
*   debugger.pl
*/
:- module(prolog_debugger,
          [ load_source_file/1,
            startup/1
          ]).

:- (dynamic subterm_pos/4, clause_handled/2, file_chars/2, file_chars_computed/1, user:prolog_trace_interception/4, file_lines/2, file_line_start_end/4, file_lines_computed/1).

:- use_module(library(http/json)).

:- (initialization init).

init :-
    nb_setval(frame_id, 0),
    nb_setval(frame, _{}),
    nb_setval(breakpoints, _{}),
    nb_setval(bindings, _{}).

gen_frame_id(Id) :-
    nb_getval(frame_id, Id),
    NewId is Id+1,
    nb_setval(frame_id, NewId).

load_source_file(File) :-
    file_lines_computed(File), !.
load_source_file(File) :-
    (   source_file(File)
    ;   load_files(File, [module(user)])
    ),
    setup_file_lines_chars(File).

set_breakpoints(SourceFile1, BPs) :-
    nb_setval(breakpoints, _{}),
    split_string(BPs, ";", "", BPL),
    convert_to_dicts(BPL, BPDs),
    absolute_file_name(SourceFile1, SourceFile),
    (   source_file(SourceFile)
    ;   consult(SourceFile)
    ),
    clear_breakpoints(SourceFile),
    set_breakpoints(SourceFile, BPDs, Verified), !,
    Response=_{response:_{breakpoints:Verified}},
    dict_json(Response, ResStr),
    format('~n~w~n', ResStr).

convert_to_dicts([JH|JT], [DH|DT]) :-
    dict_json(DH, JH),
    convert_to_dicts(JT, DT).
convert_to_dicts([], []).

clear_breakpoints(File) :-
    breakpoint_property(Id, file(File)),
    delete_breakpoint(Id),
    fail.
clear_breakpoints(_).

set_breakpoints(File,
                            [BPDict|T],
                            
                            [ _{ line:BPDict.line,
                                 source:File,
                                 verified:Verified
                               }
                            | VT
                            ]) :-
    dict_keys(BPDict, BpKeys),
    (   memberchk(column, BpKeys)
    ->  Col is BPDict.column
    ;   Col is 0
    ),
    catch((   set_breakpoint(File, BPDict.line, Col, Id)
          ->  Verified=true,
              add_to_dict(Id, BpKeys, File, BPDict)
          ;   Verified=false
          ),
          _,
          Verified=false),
    set_breakpoints(File, T, VT), !.
set_breakpoints(_, [], []).

add_to_dict(Id, BpKeys, File, BPDict) :-
    NewBPD=BPDict.put(file, File),
    (   memberchk(hitCondition, BpKeys)
    ->  NNBPD=NewBPD.put(hits, 1)
    ;   NNBPD=NewBPD
    ),
    dict_create(DOut, _, [Id:NNBPD]),
    nb_setval(breakpoints, DOut).

spy_predicates(Preds) :-
    nospyall,
    spy_predicates(Preds, Verified),
    Response=_{response:_{functionbps:Verified}},
    dict_json(Response, ResStr),
    format('~n~w~n', ResStr).

spy_predicates([Pred|T],
                           
                           [ _{message:PredA, verified:Verified}
                           | VT
                           ]) :-
    term_to_atom(Pred, PredA),
    catch((   spy(Pred)
          ->  Verified=true
          ;   Verified=false
          ),
          _,
          Verified=false),
    spy_predicates(T, VT).
spy_predicates([], []).

startup(StartGoal) :-
    leash(-exit),
    visible(-exit),
    qualify(StartGoal, GStart),
    output_clause_location(StartGoal, _), !,
   % noguitracer, !,
    trace,
    call(GStart),
    notrace.

user:prolog_trace_interception(_, Frame, _, Action) :-
    prolog_frame_attribute(Frame, goal, Goal),
    (   locate_source(Frame)
    ;   output_clause_location(Goal, Frame)
    ),
    check_breakpoint(Action). 
check_breakpoint(continue) :-
    nb_getval(frame, FrDict),
    nb_getval(breakpoints, BpDict),
    nb_getval(bindings, BdDict),
    breakpoint_property(Id, file(FrDict.file)),
    breakpoint_property(Id, line_count(FrDict.line)),
    dict_keys(BpDict.Id, BpKeys),
    (   memberchk(condition, BpKeys),
        \+ meet_condition(BdDict, BpDict.Id)
    ;   memberchk(hitCondition, BpKeys),
        \+ meet_hit_condition(BpDict, Id)
    ),
    notrace, !.
check_breakpoint(_).

meet_hit_condition(BpDict, Id) :-
    number_string(HitCond, BpDict.Id.hitCondition),
    CurrHits is BpDict.Id.hits,
    NHits is CurrHits+1,
    BpIdDict1=BpDict.Id.put(hits, NHits),
    BpDict1=BpDict.put(Id, BpIdDict1),
    nb_setval(breakpoints, BpDict1),
    CurrHits>=HitCond.

meet_condition(BdDict, BpIdDict) :-
    Cond=BpIdDict.condition,
    atom_concat(BdDict.vars, Cond, AllCond),
    atom_to_term(AllCond, CTerm, _),
    call(CTerm).

frame_arguments(Frame, Args) :-
    arguments(Frame, 1, Args).

arguments(Frame, N, [Val|T]) :-
    prolog_frame_attribute(Frame, argument(N), Val), !,
    N1 is N+1,
    arguments(Frame, N1, T).
arguments(_, _, []).

qualify(Goal, Goal) :-
    functor(Goal, :, 2), !.
qualify(Goal, Module:Goal) :-
    context_module(Module).

output_clause_location(Head, Frame) :-
    qualify(Head, QHead),
    \+ predicate_property(QHead, foreign),
    (   catch(clause(QHead, _, CR),
              File,
              ( print_message(error, File),
                fail
              ))
    ->  true
    ;   print_message(error, not_found(Head)),
        format("Start debugging from the file where '~w' locates.", Head),
        fail
    ),
    clause_property(CR, file(File)),
    load_source_file(File),
    clause_property(CR, line_count(Line)),
    (   nonvar(Frame)
    ->  frame_attrs(Frame, Attrs),
        output_stack_frame(Attrs.id,
                           Attrs.level,
                           Attrs.pi,
                           File,
                           Line,
                           0)
    ;   true
    ).
output_clause_location(_, _).

dict_json(Dict, Json) :-
    is_dict(Dict), !,
    atom_json_dict(Text, Dict, []),
    split_string(Text, '\n', ' \t', Subs),
    atomic_list_concat(Subs, Json).
dict_json(Dict, Json) :-
    atom_json_dict(Json, Dict, []).

output_stack_frame(FrameId,
                               Level,
                               FrameName,
                               File,
                               Line,
                               Column) :-
    Response=_{response:_{frame:_{column:Column, file:File, id:FrameId, level:Level, line:Line, name:FrameName}}},
    nb_setval(frame, Response.response.frame),
    dict_json(Response, NoSpc),
    format('~n~w~n', NoSpc).

frame_attrs(Frame, Attrs) :-
    prolog_frame_attribute(Frame, goal, Goal1),
    prolog_frame_attribute(Frame, level, Level),
    prolog_frame_attribute(Frame, predicate_indicator, PI),
    term_to_atom(PI, PIA),
    (   Goal1=_:Goal
    ->  true
    ;   Goal=Goal1
    ),
    % gen_frame_id(FrameId),
    Attrs=_{goal:Goal, id:Frame, level:Level, pi:PIA}.

hide_children_frame(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal),
    (   predicate_property(Goal, nodebug)
    ->  true
    ;   predicate_property(Goal, foreign)
    ).


trace_parent(Level, Start, Start, Parent, ClauseRef, PC) :-
    Level>0,
    prolog_frame_attribute(Start, parent, Parent),
    \+ hide_children_frame(Parent),
    catch(prolog_frame_attribute(Parent, clause, ClauseRef), _, fail),
    prolog_frame_attribute(Start, pc, PC), !.
trace_parent(Level, Start, Child, GParent, ClauseRef, PC) :-
    Level>0,
    prolog_frame_attribute(Start, parent, Parent),
    Next is Level-1,
    trace_parent(Next, Parent, Child, GParent, ClauseRef, PC).
    

locate_source(Frame) :-
    prolog_frame_attribute(Frame, level, Level),
    trace_parent(Level, Frame, Child, Parent, PClauseRef, PC),
    frame_attrs(Frame, FAttrs),
    prolog_frame_attribute(Parent, goal, ParentGoal),
    \+ ParentGoal=startup(_),
    output_bindings(Parent, PClauseRef),
    (   Child=:=Frame
    ->  subgoal_position(PClauseRef, PC, File, CharA, _)
    ;   locate_from_term_position(Parent, PClauseRef, FAttrs.goal, CharA)
    ),
    from_char_to_line_char(File, CharA, Line, Column),
    output_stack_frame(FAttrs.id,
                       FAttrs.level,
                       FAttrs.pi,
                       File,
                       Line,
                       Column).
find_file_chars(File) :-
    file_chars_computed(File), !.
find_file_chars(File) :-
    retractall(file_chars(File, _)),
    retractall(subterm_pos(File, _, _, _)),
    retractall(clause_handled(File, _)),
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    string_codes(String, Codes),
    string_chars(String, Chars),
    assert(file_chars(File, Chars)),
    assert(file_chars_computed(File)).



chars_string(File, From, To, String) :-
    file_chars(File, Chars),
    sub_chars(Chars, From, To, Subs),
    string_chars(String, Subs).

sub_chars(_, To, To, []) :- !.
sub_chars(Chars, From, To, [H|T]) :-
    nth0(From, Chars, H),
    Next is From+1,
    sub_chars(Chars, Next, To, T).



strip_subterm(File, ClauseRef, _) :-
    clause_handled(File, ClauseRef). 
strip_subterm(File, ClauseRef, SubTerms) :-
    assert_subterm(File, ClauseRef, SubTerms),
    assert(clause_handled(File, ClauseRef)).
     
assert_subterm(File,
                           ClauseRef,
                           parentheses_term_position(_, _, Subs)) :-
    assert_subterm(File, ClauseRef, Subs), !.
assert_subterm(File,
                           ClauseRef,
                           term_position(_,
                                         _,
                                         FFrom,
                                         FTo,
                                         [L, R])) :-
    chars_string(File, FFrom, FTo, ","),
    assert_subterm(File, ClauseRef, L),
    assert_subterm(File, ClauseRef, R), !.
assert_subterm(File,
                           ClauseRef,
                           term_position(_,
                                         _,
                                         FFrom,
                                         FTo,
                                         [L, R])) :-
    chars_string(File, FFrom, FTo, ";"),
    assert_subterm(File, ClauseRef, L),
    assert_subterm(File, ClauseRef, R), !.
assert_subterm(File,
                           ClauseRef,
                           term_position(_,
                                         _,
                                         FFrom,
                                         FTo,
                                         [H|T])) :-
    chars_string(File, FFrom, FTo, "->"),
    assert_subterm(File, ClauseRef, H),
    assert_subterm(File, ClauseRef, T), !.
assert_subterm(File,
                           ClauseRef,
                           term_position(_,
                                         _,
                                         FFrom,
                                         FTo,
                                         [H, B])) :-
    chars_string(File, FFrom, FTo, ":-"),
    assert_subterm(File, ClauseRef, H),
    assert_subterm(File, ClauseRef, B), !.
assert_subterm(File, ClauseRef, [H|T]) :-
    assert_subterm(File, ClauseRef, H),
    assert_subterm(File, ClauseRef, T), !.    
assert_subterm(File,
                           ClauseRef,
                           term_position(From,
                                         To,
                                         _,
                                         _,
                                         Subs)) :-
    assert_subterm(File, ClauseRef, From, To),
    assert_subterm(File, ClauseRef, Subs), !.
assert_subterm(_, _, _).

assert_subterm(File, ClauseRef, From, To) :-
    chars_string(File, From, To, Goal),
    del_spaces(Goal, Goal1),
    assert(subterm_pos(File, ClauseRef, Goal1, From)).

del_spaces(Str, NoSpc) :-
    split_string(Str, ' ', '\n\t ', Subs),
    atomic_list_concat(Subs, NoSpc).

locate_from_term_position(PFrame, PCRef, Goal, CharA) :-
    term_variables(Goal, GVarList),
    clause_info(PCRef, File, SubTermPos, Vars),
    Vars=..[_|VarList],
    frame_arguments(PFrame, Args),
    goal_var_names(GVarList, VarList, Args, VarNames),
    find_file_chars(File),
    strip_subterm(File, PCRef, SubTermPos),
    subterm_pos(File, PCRef, GoalStr, CharA),
    term_string(Goal, GoalStr, [variable_names(Vars1)]),
    exclude(nonvar_var, Vars1, VarNames), !.

nonvar_var(_=V) :-
    nonvar(V).
goal_var_names([H|T],
                           Vars,
                           Args,
                           [NVar|VT]) :-
    goal_var_name(H, Vars, Args, NVar),
    goal_var_names(T, Vars, Args, VT).
goal_var_names([], _, _, []).

goal_var_name(Var,
                          [VH|_],
                          [AH|_],
                          VH=AH) :-
    Var==AH, !.
goal_var_name(Var, [_|VT], [_|AT], NVar) :-
    goal_var_name(Var, VT, AT, NVar).
     

% test(GoalStr) :-
%     writeln(given:GoalStr),
%     retractall(clause_handled(_, _)),
%     retractall(subterm_pos(_, _, _, _)),
%     writeln(oka),
%     F='/home/laowang/workspace/vsc-prolog/test/test.pl',
%     consult(F),
%     clause(rewrite_module_declaration(_, _), _, Ref),
%     locate_from_term_position(_, Ref, GoalStr, CharA),
%     from_char_to_line_char(F, CharA, Line, Ch),
%     writeln(location:Line:Ch).

print_properties(Frame, [H|T]) :-
    catch(( prolog_frame_attribute(Frame, H, Value),
            writeln(H:Value)
          ),
          _,
          true),
    print_properties(Frame, T).
print_properties(_, []).

output_bindings(ParentFrame, ParentClauseRef) :-
    frame_arguments(ParentFrame, Args),
    clause_info(ParentClauseRef, _, _, Vars),
    Vars=..[_|VarsLst],
    frame_bindings(Args, VarsLst, Bindings),
    output_bound_vars(Bindings).

frame_bindings([_|AT], [Var|VT], Bound) :-
    Var=='_', !,
    frame_bindings(AT, VT, Bound).
frame_bindings([Arg|AT],
                           [Var|VT],
                           [Var=Bound|BT]) :-
    Var\=='_',
    (   var(Arg)
    ->  Bound='_'
    ;   Bound=Arg
    ),
    frame_bindings(AT, VT, BT).
frame_bindings([], [], []).

output_bound_vars(Bound) :-
    convert_bound(Bound, Converted, NameVals),
    BDict=_{vars:NameVals},
    nb_setval(bindings, BDict),
    Response=_{response:_{variables:Converted}},
    dict_json(Response, ResStr),
    format('~n~w~n', ResStr).

convert_bound([Name=Val|NT],
                          [_{name:Name, value:'_'}|CT],
                          GivenVars) :-
    Val=='_', !,
    convert_bound(NT, CT, GivenVars).
convert_bound([Name=Val|NT],
                          [_{name:Name, value:Val1}|CT],
                          GivenVars) :-
    term_to_atom(Val, Val1),
    (   number(Val)
    ->  atomic_list_concat([Name, ' is ', Val], Given1)
    ;   atomic_list_concat([Name, ' = ', Val1], Given1)
    ),
    convert_bound(NT, CT, LeftGivenVars),
    atomic_list_concat([Given1, ',', LeftGivenVars], GivenVars).
convert_bound([], [], '').

clause_position(PC) :-
    integer(PC), !.
clause_position(exit).
clause_position(unify).
clause_position(choice(_)).

subgoal_position(ClauseRef, unify, File, CharA, CharZ) :- !,
    clause_info(ClauseRef, File, TPos, _),
    head_pos(ClauseRef, TPos, PosTerm),
    nonvar(PosTerm),
    arg(1, PosTerm, CharA),
    arg(2, PosTerm, CharZ).
subgoal_position(ClauseRef, choice(CHP), File, CharA, CharZ) :- !,
    (   prolog_choice_attribute(CHP, type, jump),
        prolog_choice_attribute(CHP, pc, To)
    ->  subgoal_position(ClauseRef, To, File, CharA, CharZ)
    ;   clause_end(ClauseRef, File, CharA, CharZ)
    ).
subgoal_position(ClauseRef, Port, File, CharA, CharZ) :-
    end_port(Port), !,
    clause_end(ClauseRef, File, CharA, CharZ).
subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
    clause_info(ClauseRef, File, TPos, _),
    (   '$clause_term_position'(ClauseRef, PC, List)
    ->  (   find_subgoal(List, TPos, PosTerm)
        ->  true
        ;   PosTerm=TPos,
            writeln('warning: Clause source-info could not be parsed'),
            fail
        ),
        nonvar(PosTerm),
        arg(1, PosTerm, CharA),
        arg(2, PosTerm, CharZ)
    ;   format('warning: No clause-term-position for ref=~w at PC=~w~n',
               [ClauseRef, PC]),
        fail
    ).

end_port(exit).
end_port(fail).
end_port(exception).

clause_end(ClauseRef, File, CharA, CharZ) :-
    clause_info(ClauseRef, File, TPos, _),
    nonvar(TPos),
    arg(2, TPos, CharA),
    CharZ is CharA+1.

head_pos(Ref, Pos, HPos) :-
    clause_property(Ref, fact), !,
    HPos=Pos.
head_pos(_,
                     term_position(_,
                                   _,
                                   _,
                                   _,
                                   [HPos, _]),
                     HPos).

find_subgoal(_, Pos, Pos) :-
    var(Pos), !.
find_subgoal([A|T],
                         term_position(_,
                                       _,
                                       _,
                                       _,
                                       PosL),
                         SPos) :-
    nth1(A, PosL, Pos), !,
    find_subgoal(T, Pos, SPos).
find_subgoal([1|T],
                         brace_term_position(_, _, Pos),
                         SPos) :- !,
    find_subgoal(T, Pos, SPos).
find_subgoal(List,
                         parentheses_term_position(_, _, Pos),
                         SPos) :- !,
    find_subgoal(List, Pos, SPos).
find_subgoal(_, Pos, Pos).

setup_file_lines_chars(File) :-
    file_lines_computed(File), !.
setup_file_lines_chars(File) :-
    retractall(file_lines_computed(File)),
    retractall(file_line_start_end(File, _, _, _)),
    retractall(file_lines(File, _)),
    read_file_to_string(File, String, [encoding(utf8)]),
    atomic_list_concat(List, '\n', String),
    asserta(file_lines(File, List)),
    maplist(string_length, List, Lengths1),
    maplist(plus(1), Lengths1, [FirstLen|Lengths]),
    assertz(file_line_start_end(File, 1, 0, FirstLen)),
    assert_all_lines(File, Lengths, 2),
    assert(file_lines_computed(File)), !.

assert_all_lines(File, [H|T], N) :-
    Last is N-1,
    file_line_start_end(File, Last, _, LastEnd),
    ThisStart is LastEnd,
    ThisEnd is ThisStart+H,
    assertz(file_line_start_end(File, N, ThisStart, ThisEnd)),
    Next is N+1,
    assert_all_lines(File, T, Next).
assert_all_lines(_, [], _).

from_char_to_line_char(File, CharA, Line, StartChar) :-
    load_source_file(File),
    file_line_start_end(File, Line, Start, End),
    CharA>=Start,
    CharA<End, !,
    StartChar is CharA-Start.

get_term_from_pc(ClauseRef, PC, TermTxt, Term, Binding) :-
    subgoal_position(ClauseRef, PC, File, CharA, CharZ),
    from_char_to_line_char(File, CharA, LineA, StartA),
    from_char_to_line_char(File, CharZ, LineZ, StartZ),
    get_term_text(File, LineA, StartA, LineZ, StartZ, TermTxt),
    atom_string(TermAtom, TermTxt),
    atom_to_term(TermAtom, Term1, Binding),
    (   Term1=_:Term
    ->  true
    ;   Term=Term1
    ).

get_term_text(File, _, _, _, _, _) :-
    \+ file_lines(File, _),
    load_source_file(File),
    fail.
get_term_text(File, Line, CharA, Line, CharZ, Text) :-
    file_lines(File, Lines),
    nth1(Line, Lines, String),
    Length is CharZ-CharA,
    sub_string(String, CharA, Length, _, Text), !.
get_term_text(File, LineA, CharA, LineZ, CharZ, Text) :-
    file_lines(File, Lines),
    nth1(LineA, Lines, String),
    sub_string(String, CharA, _, 0, SubStrA),
    LineB is LineA+1,
    merge_lines(Lines, LineB, LineZ, CharZ, SubStrA, Text).

merge_lines(Lines, LineZ, LineZ, CharZ, SubStr, Text) :-
    nth1(LineZ, Lines, String),
    sub_string(String, 0, CharZ, _, SubStrZ1),
    split_string(SubStrZ1, "", " \t\n", [SubStrZ]),
    string_concat(SubStr, SubStrZ, Text), !.
merge_lines(Lines, Line, LineZ, CharZ, SubStr, Text) :-
    nth1(Line, Lines, String1),
    split_string(String1, "", " \t\n", [String]),
    string_concat(SubStr, String, NewStr),
    NextLine is Line+1,
    merge_lines(Lines, NextLine, LineZ, CharZ, NewStr, Text).
