posToAxial(Pos,PosAxial):-
    (Row,Col)=Pos,
    char_code('E',ECharCode),
    char_code(Row,RowCharCode),
    R is ECharCode-RowCharCode,
    AuxOffset is 4+R+1, %1 because columns start at 1 rather than 0 in Taacoca
    Offset is min(4+1,AuxOffset),
    Q is Col-Offset,
    PosAxial=(R,Q).


getElementOfMatrix(Matrix,Line,Col,Element):-
    nth0(Line, Matrix, Row),
    nth0(Col, Row, Element).



getPiece(Board,(R,Q),Piece):-
    Line is R+4,
    AuxOffset is 4+R,
    Offset is min(4,AuxOffset),
    Col is Q + Offset,
    getElementOfMatrix(Board,Line,Col,(Pos,Piece)). 

initial_state(GameState-Player):-
    board(1,GameState),
    Player=white.


pos_in_board((R,Q)):-
    R=<0,
    !,
    LowerBoundQ is -4-R,
    between(LowerBoundQ,4,Q).
pos_in_board((R,Q)):-
    R>0,
    !,
    UpperBoundQ is 4-R,
    between(-4,UpperBoundQ,Q).


other_stones(Stone,[Stone|OtherStones],OtherStones).

other_stones(Stone,[X|R],OtherStones):-
    other_stones(Stone,R,OtherStonesRest),
    OtherStones=[X|OtherStonesRest].


legal_move_stone(GameState,(R,Q),Piece,Stones,Stone):-
    other_stones(Stone,Stones,OtherChosenStones),
    (OldR,OldQ)=Stone,
    NewR is OldR+R,
    NewQ is OldQ+Q,
    getPiece(GameState,(NewR,NewQ),PieceInNewPos),
    (PieceInNewPos\=Piece;
    member((NewR,NewQ),OtherChosenStones)),
    pos_in_board((NewR,NewQ)).  


legal_move(GameState,Stones,Move,Piece):-
    maplist(legal_move_stone(GameState,Move,Piece,Stones),Stones).



get_moves(GameState,Piece,Stones,InitialMoves,PossibleMoves):-
    findall(Move,(member(Move,InitialMoves),legal_move(GameState,Stones,Move,Piece)),PossibleMoves).


choose_stone(_,_,4,Stones,Stones):-!.

choose_stone(GameState,Player,PieceNr,StonesChosenSoFar,Stones):-
    repeat,
    format('Enter stone ~d\'s position\n',[PieceNr]),
    write('Line: '),
    read(Line),
    write('Col: '),
    read(Col),
    posToAxial((Line,Col),Stone),
    getPiece(GameState,Stone,Player),
    NewPieceNr is PieceNr+1,
    choose_stone(GameState,Player,NewPieceNr,[Stone|StonesChosenSoFar],Stones).



choose_stones(GameState,Player,PossibleMoves,Stones):-
    choose_stone(GameState,Player,1,[],Stones),
    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],
    get_moves(GameState,Player,Stones,InitialMoves,PossibleMoves).



display_move((0,1)):-write('East (E)'),nl.

display_move((1,0)):-write('South-East (SE)'),nl.

display_move((1,-1)):-write('South-West (SW)'),nl.

display_move((0,-1)):-write('West (W)'),nl.

display_move((-1,0)):-write('North-West (NW)'),nl.

display_move((-1,1)):-write('North-East (NE)'),nl.


parseMove('E',(0,1)).

parseMove('SE',(1,0)).

parseMove('SW',(1,-1)).

parseMove('W',(0,-1)).

parseMove('NW',(-1,0)).

parseMove('NE',(-1,1)).

display_possible_moves(PossibleMoves):-
    maplist(display_move,PossibleMoves).



choose_move(GameState, Player, Stones, Move):-
    choose_stones(GameState,Player,PossibleMoves,Stones),
    write('Possible Moves'),
    nl,
    display_possible_moves(PossibleMoves),
    write('Choose your move:'),
    read(MoveText),
    parseMove(MoveText,Move).


congratulate(Winner):-
    format('~a wins the game\n',[Winner]).

white_reach_opponent_home(GameState):-
    nth0(0, GameState, OpponentHome),
    between(0, 4, Col),
    nth0(Col,OpponentHome,(Pos,Piece)),
    Piece=white.

black_reach_opponent_home(GameState):-
    nth0(8, GameState, OpponentHome),
    between(0, 4, Col),
    nth0(Col,OpponentHome,(Pos,Piece)),
    Piece=black.


setPieceLine([((R,Q),_)|Rest],R,Q,Piece,[((R,Q),Piece)|Rest]):-!.

setPieceLine([X|Rest],R,Q,Piece,NewLine):-
    setPieceLine(Rest,R,Q,Piece,NewRest),
    NewLine=[X|NewRest].


setPiece([Line|Rest],R,Q,Piece,NewBoard):-
    setPieceLine(Line,R,Q,Piece,NewLine),
    NewBoard=[NewLine|Rest],!.

setPiece([Line|Rest],R,Q,Piece,NewBoard):-
    setPiece(Rest,R,Q,Piece,NewRest),
    NewBoard=[Line|NewRest].


moveStone(Board,Piece,(OldR,OldQ),(R,Q),MovedStones,NewBoard,(NewR,NewQ)):-
    member((OldR,OldQ),MovedStones),!,
    NewR is OldR+R,
    NewQ is OldQ+Q,
    setPiece(Board,NewR,NewQ,Piece,NewBoard).


moveStone(Board,Piece,(OldR,OldQ),(R,Q),MovedStones,NewBoard,(NewR,NewQ)):-
    NewR is OldR+R,
    NewQ is OldQ+Q,
    setPiece(Board,OldR,OldQ,empty,AuxBoard),
    setPiece(AuxBoard,NewR,NewQ,Piece,NewBoard).

    
move(GameState, Player, [], Move, _ , GameState).

move(GameState, Player, [Stone|R], Move, MovedStones,NewGameState):-
    moveStone(GameState,Player,Stone,Move,MovedStones,AuxGameState,NewPosition),
    move(AuxGameState,Player,R,Move,[NewPosition|MovedStones],NewGameState).


next_player(white,black).
next_player(black,white).


game_over(GameState-black, white):-
    white_reach_opponent_home(GameState).

game_over(GameState-white, black):-
    black_reach_opponent_home(GameState).

game:-
    initial_state(GameState-Player),
    display_game(GameState),
    game_cycle(GameState-Player).


game_cycle(GameState-Player):-
    game_over(GameState-Player, Winner), !,
    congratulate(Winner).


game_cycle(GameState-Player):-
    choose_move(GameState, Player, Stones, Move),
    move(GameState, Player, Stones, Move, [],NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState), !,
    game_cycle(NewGameState-NextPlayer).


