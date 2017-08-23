/*
*  trytrace.pl
*/
:- use_module(library(regex)).

min_numlist([H|T], Min) :-
    min_numlist(T, H, Min).

min_numlist([], Min, Min).
min_numlist([H|T], Min0, Min) :-
    Min1 is min(H, Min0),
    min_numlist(T, Min1, Min).

start :-
    data(D),
    min_numlist(D, Min),
    format('Min=~d~n', [Min]),
    fail.
start.

data(D) :-
    D=[3, 6, 9, 1].
data(D) :-
    D=[100, 36, 90, 81].

% test1 :-
%     "hello"=~"^\\w".