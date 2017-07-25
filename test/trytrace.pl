/*
*  trytrace.pl
*/

% :- module(trytrace,[start/0]).


min_numlist([H|T], Min) :-
    min_numlist(T, H, Min).

%!	min_numlist([], Min, Min)
%
%
min_numlist([], Min, Min).
min_numlist([H|T], Min0, Min) :-
    % nb_getval(test, Tval),
    % NVal is Tval*Tval,
    % writeln(val:Tval:new:NVal),
    Min1 is min(H, Min0),
    min_numlist(T, Min1, Min).

%!	start
%
%
start :-
%     % nb_setval(test, 200),
    data(D),
    min_numlist(D, Min),
%     % NN is 100/0,
%     % writeln(nn:NN),
%     regex('(\\d+)', [], abc123, Captures),
%     writeln(Captures),
    format('Min=~d~n', [Min]),
    fail.
start :-
    prompt(_, 'input:'),
    read_line_to_string(user_input, Str),
    writeln(got:Str).

data(D) :-
    D=[3, 6, 9, 1].
data(D) :-
    D=[100, 36, 90, 81].