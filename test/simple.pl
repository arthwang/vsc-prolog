/* 
* demo in README.md
*/
:- use_module(library(http/json), [atom_json_dict/3]).
test :-
    Dict=_{name:'Arthur Wang'},
    atom_json_dict(Atom, Dict, []),
    writeln(Atom).

