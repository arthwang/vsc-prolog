%%
/*second line comment*/
:- use_module(library(regex)).
    % spaces before comment 

% about predicate
start1 :-
/* block comment
 */
 
    % Str1=~'\\d+',
    Str1=hello, %middle comments.
    writeln(str:Str1).% comments at end.

    /* footer comments

    */
