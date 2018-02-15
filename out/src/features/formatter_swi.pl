%%
:- module(formatter,[]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(memfile)).

format_prolog_source(TabSize, TabDistance, RangeTxt, DocTxt) :-
    load_modules(DocTxt),
    % writeln(range:RangeTxt),
    atom_string(RangeAtom, RangeTxt),
    atom_to_memory_file(RangeAtom, MemFH),
    setup_call_cleanup(open_memory_file(MemFH, read, MemRStream),
                       read_and_portray_term(TabSize, TabDistance, MemRStream),
                       close(MemRStream)), !.
format_prolog_source(_, _, _, _) :-
    halt. 

load_modules(DocTxt) :-
    atom_string(DocAtom, DocTxt),
    atom_to_memory_file(DocAtom, DMFH),
    setup_call_cleanup(open_memory_file(DMFH, read, RStream),
                       load_modules1(RStream),
                       close(RStream)).

load_modules1(RStream) :-
    read_term(RStream, Term, []),
    handle_term(Term, RStream), !.
handle_term(end_of_file, _) :- !.
handle_term((:-use_module(MFile)), RStream) :-
    user:use_module(MFile),
    load_modules1(RStream), !.
handle_term((:-use_module(MFile, Imps)), RStream) :-
    user:use_module(MFile, Imps),
    load_modules1(RStream), !.
handle_term(_, RStream) :-
    load_modules1(RStream).
handle_term((:-reexport(MFile)), RStream) :-
    user:reexport(MFile),
    load_modules1(RStream), !.
handle_term((:-reexport(MFile, Imps)), RStream) :-
    user:reexport(MFile, Imps),
    load_modules1(RStream), !.
handle_term(_, RStream) :-
    load_modules1(RStream).

        

read_and_portray_term(TabSize, TabDistance, SourceStream) :-
    set_setting(listing:body_indentation, TabSize),
    set_setting(listing:tab_distance, TabDistance),
    setup_call_cleanup(new_memory_file(MFH),
                       read_terms(SourceStream, MFH),
                       free_memory_file(MFH)).

read_and_portray_term(_, _, _) :-
    halt. 
    
read_terms(ReadStream, MFH) :-
    repeat,
    catch(read_term(ReadStream,
                    Term,
                    [ variable_names(VarsNames1),
                      term_position(TPos),
                      comments(TermComms),
                      character_escapes(true)
                    ]),
          E,
          ( print_message(error, E),
            halt(1)
          )),
    setup_call_cleanup(open_memory_file(MFH, write, MFWS),
                    portray_clause(MFWS, Term),
                    close(MFWS)),
                
    setup_call_cleanup(open_memory_file(MFH, read, MFRS),
                        read_term(MFRS, Term1, [variable_names(VarsNames2)]),
                    close(MFRS)),
    writeln('TERMSEGMENTBEGIN:::'),
    (   maplist(var_name1, VarsNames1, VarsNames2, Varsa)
    ->  reverse(Varsa, Vars)
    ;   Vars=[givingup]
    ),
    format('VARIABLESBEGIN:::~w:::VARIABLESEND~n', [Vars]),
    stream_position_data(char_count, TPos, TermCharA),
    format('TERMPOSBEGIN:::~d:::TERMPOSEND~n', [TermCharA]),
    writeln('TERMBEGIN:::'),
    portray_clause(current_output, Term1, [quoted(true),character_escapes(true),module(formatter)]),
    writeln(':::TERMEND'),
    maplist(convert_comm_pos, TermComms, TermComms1),
    CommDict=_{comments:TermComms1},
    atom_json_dict(CDictTxt, CommDict, []),
    format('COMMENTSBIGIN:::~w:::COMMENTSEND~n', [CDictTxt]),
    writeln(':::TERMSEGMENTEND'),
    Term=end_of_file,
    writeln('::::::ALLOVER').


convert_comm_pos(Pos-Comm, _{location:CharA, comment:Comm}) :-
    stream_position_data(char_count, Pos, CharA).
    
var_name1(N1=_, N2=_, Pair) :-
    atomic_list_concat([N2, :, N1], Pair).
