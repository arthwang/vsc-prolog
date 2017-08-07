% demo in README.md
say_hi :-
    prompt(_, 'What\'s your name?'),
    read_line_to_string(current_input, Name),
    format('Hello, ~s!~n', Name).
    
