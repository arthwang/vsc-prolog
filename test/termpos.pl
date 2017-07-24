/*
*  termpos.pl
*/
:- (dynamic file_line_start_end/4, file_lines_computed/1, file_chars/2, subterm_pos/3).

find_file_chars(File) :-
    retractall(file_chars(_, _)),
    retractall(subterm_pos(_, _, _)),
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    string_codes(String, Codes),
    string_chars(String, Chars),
    assert(file_chars(File, Chars)).

load_source_file(File) :-
    user:file_lines_computed(File), !.
load_source_file(File) :-
    (   source_file(File)
    ;   consult(File)
    ),
    find_file_chars(File),
    setup_file_lines_chars(File).
setup_file_lines_chars(File) :-
    file_lines_computed(File), !.
setup_file_lines_chars(File) :-
    retractall(file_lines_computed(File)),
    retractall(file_line_start_end(File, _, _, _)),
    read_file_to_string(File, String, [encoding(utf8)]),
    atomic_list_concat(List, '\n', String),
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
chars_string(File, From, To, String) :-
    file_chars(File, Chars),
    sub_chars(Chars, From, To, Subs),
    string_chars(String, Subs).

sub_chars(_, To, To, []) :- !.
sub_chars(Chars, From, To, [H|T]) :-
    nth0(From, Chars, H),
    Next is From+1,
    sub_chars(Chars, Next, To, T).

strip_subterm(File, [H|T]) :-
    assert_subterm(File, H),
    strip_subterm(File, T), !.
strip_subterm(_, []).


assert_subterm(File, term_position(From, To, _, _, Subs)) :-
    assert_subgoal(File, From, To),
    strip_subterm(File, Subs), !.
% assert_subterm(File, From-To) :-
%     assert_subgoal(File, From, To), !.
% assert_subterm(File, string_position(From,To)) :-
%     assert_subgoal(File, From, To), !.
% assert_subterm(File, brace_term_position(From, To, _)) :-
%     assert_subgoal(File, From, To), !.
% assert_subterm(File, list_position(From, To, _)) :- 
%     assert_subgoal(File, From, To), !.
% assert_subterm(File, dict_position(From, To, _, _, _)) :-
%     assert_subgoal(File, From, To), !.
% assert_subterm(File, key_value_position(From, To, _, _, _, _, _)) :- 
%     assert_subgoal(File, From, To), !.
assert_subterm(File, parentheses_term_position(_, _, Subs)) :-
    strip_subterm(File, [Subs]), !.

% assert_subterm(File, quasi_quotation_position(_, _, From, To, _)) :-
%     assert_subgoal(File, From, To), !.
assert_subterm(_, _).


assert_subgoal(File, From, To) :-
    chars_string(File, From, To, Goal),
    del_spaces(Goal, Goal1),
    from_char_to_line_char(File, From, Line, StartChar),
    assert(subterm_pos(Goal1, Line, StartChar)).

del_spaces(Str, NoSpc) :-
    split_string(Str, ' ', '\n\t ', Subs),
    atomic_list_concat(Subs, NoSpc).
    
    
prolog_trace_interception(Port, Frame, _, _) :-
    % trace_parent(Frame).
    nl,
    prolog_frame_attribute(Frame, goal, Goal), !,
    writeln(port:Port;frame:Frame;goal:Goal),
    backtrace(20).
    
    
    % print_properties(Frame, [goal, clause, predicate_indicator,parent, parent_goal]),!.
%     prolog_frame_attribute(Frame, clause, ClauseRef),
%     writeln(clause:ClauseRef),
%     clause_info(ClauseRef, File, TermPos, _),
%     strip_subterm(File, [TermPos]),
%     fail.
% prolog_trace_interception(_, Frame, _, _) :-
%     prolog_frame_attribute(Frame, goal, Goal),
%     (   Goal=_:Goal1
%     ->  true
%     ;   Goal1=Goal
%     ),
%     subterm_pos(Goal1, Line, Char),
%     writeln(Goal1:Line:Char).

print_properties(Frame, [goal|T]) :-
    prolog_frame_attribute(Frame, goal, Goal), !,
    writeln(goal:Goal),
    catch(( clause(Goal, _, CRef),
            clause_property(CRef, line_count(Line)),
            writeln(clause:CRef;line:Line)
          ),
          _,
          true),
    print_properties(Frame, T).
print_properties(Frame, [parent|T]) :-
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, goal, PGoal),
    prolog_frame_attribute(Parent, parent, PParent),
    prolog_frame_attribute(PParent, goal, PPGoal),
    writeln(pgoal:PGoal),
    writeln(ppgoal:PPGoal),
    catch(( clause(PGoal, _, CRef),
            clause_property(CRef, file(File)),
            clause_property(CRef, line_count(Line)),
            writeln(parent:clause:CRef;file:File;line:Line)
          ),
          _,
          true), !,
    print_properties(Frame, T).
    
    
print_properties(Frame, [H|T]) :-
    catch(prolog_frame_attribute(Frame, H, Value),_,fail),
    writeln(H:Value),
    print_properties(Frame, T), !.
print_properties(Frame, [_|T]) :-
    print_properties(Frame, T), !.
    
trace_parent(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal), !,
    writeln(goal:Goal),
    prolog_stack_frame_property(Frame, location(File:Line)), !,
    writeln(file:File;line:Line),
    prolog_frame_attribute(Frame, parent, Parent),
    trace_parent(Parent).
trace_parent(Frame) :-
    prolog_frame_attribute(Frame, parent, Parent),
    trace_parent(Parent).
trace_parent(_).
    
    
    
    % backtrace(2).
    
    % get_prolog_backtrace(5, Backtrace),
    % assert_prolog_backtrace(current_output, Backtrace).
    
    % writeln(backtrace:BT).
    % prolog_stack_frame_property(BH, predicate(PI)),
    % writeln(pi:PI),
    % prolog_stack_frame_property(BH, location(File:Line)),
    % writeln(file:File;line:Line).
    % prolog_frame_attribute(Frame, goal, Goal),
    % prolog_frame_attribute(Frame, parent, Parent),
    % writeln(port:Port;frame:Frame;pframe:Parent;choice:Choice),
    % prolog_frame_attribute(Frame, pc, PC),
    % prolog_frame_attribute(Parent, goal, ParentGoal),
    % writeln(pc:PC;goal:Goal;pgoal:ParentGoal).