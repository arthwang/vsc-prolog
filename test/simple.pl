/* 
* demo in README.md
*/

%!	test
%
%
:- use_module(library(http/json), [atom_json_dict/3]).
test :-
    Dict=_{name:'Arthur Wang'},
    atom_json_dict(Atom, Dict, []),
    writeln(Atom).

add_from_to(From, From, From) :- !.
add_from_to(From, To, Sum) :-
    From<To, !,
    From1 is From+1,
    add_from_to(From1, To, From, Sum), !.
add_from_to(From, To, Sum) :-
    add_from_to(To, From, Sum).

add_from_to(Num, To, SumNow, Sum) :-
    Num<To,
    SumNow1 is SumNow+Num,
    Num1 is Num+1,
    add_from_to(Num1, To, SumNow1, Sum).
add_from_to(To, To, Sum1, Sum) :-
    Sum is Sum1+To.

print_list([H|T]) :-
    writeln(H),
    print_list(T).
print_list([]).


is_http(Address):-
    atom_concat('http:', _, Address).
    
 
 is_http(Address) :-
    atom_concat('http:', _, Address).
    