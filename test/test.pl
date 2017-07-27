%%
:- use_module(library(regex)).

start :-
 Str1=~'\\d+',
 writeln(str:Str1).
