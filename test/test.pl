% demo in README.md
say_hi :-
    prompt(_, 'What\'s your name?'),
    read_line_to_string(current_input, Name),
    format('Hello, ~s!~n', Name).
    
random_score :-
    % get random number
    X is random(100),
    (   X>50
    ->  writeln(big)
    ;   writeln(small)
    ).
