repeat(C,0).
repeat(C,N):-
    N>0,
    write(C),
    N1 is N-1,
    repeat(C,N1).

display_positions_top :-
    First is 10,
    repeat(' ',First),
    write('  1   2   3   4   5'),
    nl,
    repeat(' ',First),
    write('  |   |   |   |   |'),
    nl.

display_positions_bottom :-
    First is 10,
    repeat(' ',First),
    write('  |   |   |   |   |'),
    nl,
    repeat(' ',First),
    write('  1   2   3   4   5'),
    nl.

display_top_line(((R,Q),Type)):-
    write(' / \\').

display_middle_line(((R,Q),Type)):- Type = white, format('| ~a ',[w]).
display_middle_line(((R,Q),Type)):- Type = black, format('| ~a ',[b]).
display_middle_line(((R,Q),Type)):- Type = empty, format('| ~a ',[' ']).

display_bottom_line(Line):-
    write(' \\ /').

display_line(Line) :- 
    nth0(0,Line,((R,Q),Type)),
    R < 0, !,
    Code is 69 - R,
    char_code(Letter,Code),
    Padding is R * (-2),
    repeat(' ',Padding+2),
    maplist(display_top_line,Line),
    nl,
    repeat(' ',Padding),
    write(Letter),
    write('-'),
    maplist(display_middle_line,Line),
    write('|-'),
    LineNumber is 9 + R,
    write(LineNumber),
    nl.

display_line(Line) :- 
    nth0(0,Line,((R,Q),Type)),
    R > 0, !,
    Code is 69 - R,
    char_code(Letter,Code),
    Padding is R * 2,    
    repeat(' ',Padding),
    write(Letter),
    write('-'),
    maplist(display_middle_line,Line),
    write('|-'),
    LineNumber is 9 - R,
    write(LineNumber),
    nl,
    repeat(' ',Padding+2),
    maplist(display_bottom_line,Line),
    nl.


display_line(Line) :-
   R = 0,
   write('  '),
   maplist(display_top_line,Line),
   nl,
   write('E-'),
   maplist(display_middle_line,Line),
   write('|-9'),
   nl,
   write('  '),
   maplist(display_bottom_line,Line),
   nl.

display_game(Board,TypeOfGame):-  
    TypeOfGame=4,
    !,
    display_positions_top,
    maplist(display_line,Board),
    display_positions_bottom,
    sleep(2).


display_game(Board,TypeOfGame):-   
    display_positions_top,
    maplist(display_line,Board),
    display_positions_bottom.