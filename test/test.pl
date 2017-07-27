%%
:- use_module(library(regex)).

start(Str) :-
    % regex('\\d+', [], Str, Captures),

 Str =~ '\\d+',
    format('~s contains digitals.~n', [Str]).
