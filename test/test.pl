rewrite_module_declaration(Module, PI) :-
    F='/home/laowang/workspace/vsc-prolog/test/trytrace.pl',
    setup_call_cleanup(
        open(F, read, S),
        (   read_term(S, Term, [term_position(Pos)]),
            stream_position_data(line_count, Pos, Line),
            stream_position_data(line_position, Pos, Start),
            (   Term=(:-module(Module1, Exports))
            ->  (   memberchk(PI, Exports)
                ->  ReTerm=none,
                    Action=none
                ;   NewExp=[PI|Exports],
                    ReTerm=(:-module(Module1, NewExp)),
                    Action=replace
                )
            ;   ReTerm=(:-module(Module, [PI])),
                Action=insert
            ),
            format('Action=~s;Mod=~w;Line=~d;Start=~d;~n',
                [Action, ReTerm, Line, Start])
        ),
        close(S)
    ).
% start :-
%     rewrite_module_declaration(ttrace, min_numlist/3).
