/**
* repeat(+C,+N).
* 
*
* Writes the argument (C) N times
*
* @param C The argument that we want to repeat
* @param N The number of times we want to repeat
*/
repeat(_C,0).
repeat(C,N):-
    N>0,
    write(C),
    N1 is N-1,
    repeat(C,N1).

/**
* log10(+X,-Y).
*
* Calculates the log10 of X
*
* @param X The number that we want to calculate the log10
* @param Y The result of the log10
*/
log10(X,Y):-
    Y is log(X)/log(10).


/**
* display_number(+CurrentNumber,+NumNumbers).
*
* Displays the numbers from 1 to NumNumbers
*
* @param CurrentNumber The current number that we want to display
* @param NumNumbers The number of numbers that we want to display
*/
display_number(CurrentNumber,NumNumbers):-
    CurrentNumber is NumNumbers+1,!.


display_number(CurrentNumber,NumNumbers) :-
    write(CurrentNumber),
    log10(CurrentNumber,OrderOfMagnitude),
    NrRepetitions is 3-floor(OrderOfMagnitude),
    repeat(' ',NrRepetitions),
    CurrentNumber1 is CurrentNumber + 1,
    display_number(CurrentNumber1,NumNumbers).

/**
* display_positions_top(+NumLados).
*
* Displays the numbers that represent the positions at the top of the board
*
* @param NumLados The number of hexagons in one side of the board
*/
display_positions_top(NumLados) :-
    First is 2 * NumLados + 2,
    repeat(' ',First),
    display_number(1,NumLados),
    nl,
    repeat(' ',First),
    repeat('|   ', NumLados),
    nl.

/**
* display_positions_bottom(+NumLados).
*
* Displays the numbers that represent the positions in the bottom of the board
*
* @param NumLados The number of hexagons in one side of the board
*/
display_positions_bottom(NumLados) :-
    First is 2 * NumLados + 2,
    repeat(' ',First),
    repeat('|   ', NumLados),
    nl,
    repeat(' ',First),
    display_number(1,NumLados),
    nl.

/**
* display_top_line(+Line).
*
* Displays the top line of a hexagon
*
* @param Line The line that we want to display
*/
display_top_line(_):-
    write(' / \\').

/**
* display_middle_line(+Line).
*
* Displays the middle line of a hexagon
*
* @param Line The line that we want to display
*/
display_middle_line((_,Type)):- Type = white, format('| ~a ',[w]).
display_middle_line((_,Type)):- Type = black, format('| ~a ',[b]).
display_middle_line((_,Type)):- Type = empty, format('| ~a ',[' ']).

/**
* display_bottom_line(+Line).
*
* Displays the bottom line of a hexagon
*
* @param Line The line that we want to display
*/
display_bottom_line(_):-
    write(' \\ /').


/**
* display_line(+NumLados,+Line).
*
* Displays a line of hexagons of the board
*
* @param NumLados The number of hexagons in one side of the board
* @param Line The line that we want to display
*/
display_line(NumLados,Line) :- 
    nth0(0,Line,((R,_),_)),
    R < 0, !,
    MiddleLetter is 65 + NumLados - 1,
    Code is MiddleLetter - R,
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
    LineNumber is 2*NumLados - 1 + R,
    write(LineNumber),
    nl.

display_line(NumLados,Line) :- 
    nth0(0,Line,((R,_),_)),
    R > 0, !,
    MiddleLetter is 65 + NumLados - 1,
    Code is MiddleLetter - R,
    char_code(Letter,Code),
    Padding is R * 2,    
    repeat(' ',Padding),
    write(Letter),
    write('-'),
    maplist(display_middle_line,Line),
    write('|-'),
    LineNumber is 2*NumLados - 1 - R,
    write(LineNumber),
    nl,
    repeat(' ',Padding+2),
    maplist(display_bottom_line,Line),
    nl.


display_line(NumLados,Line) :-
   R = 0,
   write('  '),
   maplist(display_top_line,Line),
   nl,
   MiddleLetterCode is 65 + NumLados - 1,
   char_code(MiddleLetter,MiddleLetterCode),
   write(MiddleLetter),
   write('-'),
   maplist(display_middle_line,Line),
   LineNumber is 2*NumLados -1 + R,
   write('|-'),
   write(LineNumber),
   nl,
   write('  '),
   maplist(display_bottom_line,Line),
   nl.

/**
* display_game(+Board,+TypeOfGame,+NumLados).
*
* Displays the board
*
* @param Board The board that we want to display
* @param TypeOfGame The type of game that we are playing
* @param NumLados The number of hexagons in one side of the board
*/
display_game(Board,TypeOfGame,NumLados):-  
    TypeOfGame=4,
    !,
    display_positions_top(NumLados),
    maplist(display_line(NumLados),Board),
    display_positions_bottom(NumLados),
    sleep(2).

display_game(Board,_,NumLados):-   
    display_positions_top(NumLados),
    maplist(display_line(NumLados),Board),
    display_positions_bottom(NumLados).