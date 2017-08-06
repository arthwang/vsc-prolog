/* 
* demo in README.md
*/
%


%
test :-
    Dict=_{name:'Arthur Wang'},
    atom_json_dict(Atom, Dict, []),
    writeln(Atom).
