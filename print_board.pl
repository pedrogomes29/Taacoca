repeat(C,0).
repeat(C,N):-
    N>0,
    write(C),
    N1 is N-1,
    repeat(C,N1).


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
    Padding is R * (-2),
    repeat(' ',Padding),
    maplist(display_top_line,Line),
    repeat(' ',Padding),
    nl,
    repeat(' ',Padding),
    maplist(display_middle_line,Line),
    write('|'),
    repeat(' ',Padding),
    nl.

display_line(Line) :- 
    nth0(0,Line,((R,Q),Type)),
    R > 0, !,
    Padding is R * 2,    
    repeat(' ',Padding),
    maplist(display_middle_line,Line),
    write('|'),
    repeat(' ',Padding),
    nl,
    repeat(' ',Padding),
    maplist(display_bottom_line,Line),
    repeat(' ',Padding),
    nl.


display_line(Line) :-
   R = 0,
   maplist(display_top_line,Line),
   nl,
   maplist(display_middle_line,Line),
   write('|'),
   nl,
   maplist(display_bottom_line,Line),
   nl.

display_game(Board):-
    maplist(display_line,Board).