:- consult(board).
:- consult(print_board).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).

posToAxial((Row,Col),Offset,PosAxial):-
    char_code('A',ACharCode),
    MiddleRow is ACharCode + Offset,
    char_code(Row,RowCharCode),
    R is MiddleRow-RowCharCode,
    AuxOffset is 4+R+1, %1 because columns start at 1 rather than 0 in Taacoca
    ColOffset is min(4+1,AuxOffset),
    Q is Col-ColOffset,
    PosAxial=(R,Q).


getElementOfMatrix(Matrix,Line,Col,Element):-
    nth0(Line, Matrix, Row),
    nth0(Col, Row, Element).



getPiece(Board,(R,Q),Offset,Piece):-
    Line is R+Offset,
    AuxOffset is Offset+R,
    ColOffset is min(Offset,AuxOffset),
    Col is Q + ColOffset,
    getElementOfMatrix(Board,Line,Col,(_,Piece)). 


possible_Q(Offset,_,R,_,Q):-
    R=<0,!,
    SmallestQ is -Offset-R,
    between(SmallestQ,Offset,Q).


possible_Q(Offset,SymmetricOffset,R,_,Q):-
    R>0,!,
    BiggestQ is Offset-R,
    between(SymmetricOffset,BiggestQ,Q).

get_piece(R,_,_,empty):-
    R>=(-1),
    R=<1,!.

get_piece(R,SymmetricR,Q,black):-
    R=<1,
    Q=<SymmetricR,
    Q>=0,!.

get_piece(R,_,_,empty):-
    R=<1,!.

get_piece(R,SymmetricR,Q,white):-
    R>1,
    Q>=SymmetricR,
    Q=<0,!.

get_piece(R,_,_,empty):-
    R>1.



possible_R_Q(NumLados,R,Q,Piece):-
    Offset is NumLados-1,
    SymmetricOffset is -Offset,
    between(SymmetricOffset,Offset,R),
    SymmetricR is -R,
    possible_Q(Offset,SymmetricOffset,R,SymmetricR,Q),
    get_piece(R,SymmetricR,Q,Piece).


getEveryElementExceptLast(List,EveryElementExceptLast):-
    reverse(List,ListReversed),
    ListReversed = [_|EveryElementExceptLastReversed],
    reverse(EveryElementExceptLastReversed,EveryElementExceptLast).


convertAux([],_,CurrentMatrix,CurrentMatrix).

convertAux([((CurrentR,Q),Piece)|Rest],CurrentR, CurrentMatrix,GameStateMatrix):-
    last(CurrentMatrix,LastRow),
    append(LastRow,[((CurrentR,Q),Piece)],NewLastRow),
    getEveryElementExceptLast(CurrentMatrix,CurrentMatrixExceptLastRow),
    length(CurrentMatrixExceptLastRow,0),
    !,
    NewCurrentMatrix=[NewLastRow],
    convertAux(Rest,CurrentR,NewCurrentMatrix,GameStateMatrix).


convertAux([((CurrentR,Q),Piece)|Rest],CurrentR, CurrentMatrix,GameStateMatrix):-
    last(CurrentMatrix,LastRow),
    append(LastRow,[((CurrentR,Q),Piece)],NewLastRow),
    getEveryElementExceptLast(CurrentMatrix,CurrentMatrixExceptLastRow),
    \+length(CurrentMatrixExceptLastRow,0),
    !,
    append(CurrentMatrixExceptLastRow,[NewLastRow],NewCurrentMatrix),  
    convertAux(Rest,CurrentR,NewCurrentMatrix,GameStateMatrix).

convertAux([((NewR,Q),Piece)|Rest],CurrentR, CurrentMatrix,GameStateMatrix):-
    NewR\=CurrentR,
    LastRow = [((NewR,Q),Piece)],
    append(CurrentMatrix,[LastRow],NewCurrentMatrix),
    convertAux(Rest,NewR,NewCurrentMatrix,GameStateMatrix).


convertArrayToMatrix([((R,Q),Piece)|Rest],GameStateMatrix):-
    convertAux(Rest,R,[[((R,Q),Piece)]],GameStateMatrix).


initial_state(NumLados,GameState,white):-
    %board(1,GameState).
    findall(((R,Q),Piece),possible_R_Q(NumLados,R,Q,Piece),GameStateArray),
    convertArrayToMatrix(GameStateArray,GameState).


pos_in_board((R,Q),Offset):-
    R=<0,
    !,
    LowerBoundQ is -Offset-R,
    between(LowerBoundQ,Offset,Q).
pos_in_board((R,Q),Offset):-
    R>0,
    !,
    UpperBoundQ is Offset-R,
    SymmetricOffset is -Offset, 
    between(SymmetricOffset,UpperBoundQ,Q).


other_stones(Stone,[Stone|OtherStones],OtherStones).

other_stones(Stone,[X|R],OtherStones):-
    other_stones(Stone,R,OtherStonesRest),
    OtherStones=[X|OtherStonesRest].


legal_move_stone(GameState,(R,Q),Piece,Stones,Offset,Stone):-
    other_stones(Stone,Stones,OtherChosenStones),
    (OldR,OldQ)=Stone,
    NewR is OldR+R,
    NewQ is OldQ+Q,
    getPiece(GameState,(NewR,NewQ),Offset,PieceInNewPos),
    (PieceInNewPos\=Piece;
    member((NewR,NewQ),OtherChosenStones)),
    pos_in_board((NewR,NewQ),Offset).  


legal_move(GameState,Stones,Move,Piece,Offset):-
    maplist(legal_move_stone(GameState,Move,Piece,Stones,Offset),Stones).



get_moves(GameState,Piece,Stones,InitialMoves,Offset,PossibleMoves):-
    findall(Move,(member(Move,InitialMoves),legal_move(GameState,Stones,Move,Piece,Offset)),PossibleMoves).


choose_stone(_,_,4,_,Stones,Stones):-!.

choose_stone(GameState,Player,PieceNr,Offset,StonesChosenSoFar,Stones):-
    repeat,
    format('Enter stone ~d\'s position\n',[PieceNr]),
    write('Line: '),
    read(Line),
    write('Col: '),
    read(Col),
    posToAxial((Line,Col),Offset,Stone),
    \+member(Stone,StonesChosenSoFar),
    getPiece(GameState,Stone,Offset,Player),
    NewPieceNr is PieceNr+1,
    choose_stone(GameState,Player,NewPieceNr,Offset,[Stone|StonesChosenSoFar],Stones).





getPlayerStonesLine([],_,CurrentStones,CurrentStones).

getPlayerStonesLine([(Pos,Player)|RestLine],Player,CurrentStones,PlayerStones):-
    !,
    
    getPlayerStonesLine(RestLine,Player,[Pos|CurrentStones],PlayerStones).

getPlayerStonesLine([_|RestLine],Player,CurrentStones,PlayerStones):-
    getPlayerStonesLine(RestLine,Player,CurrentStones,PlayerStones).


getPlayerStonesAux([],_,CurrentStones,CurrentStones).

getPlayerStonesAux([Line|OtherLines],Player,CurrentStones,PlayerStones):-
    getPlayerStonesLine(Line,Player,[],LineStones),
    append(CurrentStones,LineStones,NewCurrentStones),
    getPlayerStonesAux(OtherLines,Player,NewCurrentStones,PlayerStones).


getPlayerStones(GameState,Player,PlayerStones):-
    getPlayerStonesAux(GameState,Player,[],PlayerStones).

choose_stones(GameState,Player,human,Offset,PossibleMoves,Stones):-
    choose_stone(GameState,Player,1,Offset,[],Stones),
    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],
    get_moves(GameState,Player,Stones,InitialMoves,Offset,PossibleMoves),
    !.


choose_stones(GameState,Player,computer,Offset,PossibleMove,Stones):-
    getPlayerStones(GameState,Player,PlayerStones),
    length(PlayerStones,NrStones),
    NrStones>=3,
    !,
    LastStoneIndex is NrStones-1,

    between(0,LastStoneIndex,Stone1Index),
    nth0(Stone1Index,PlayerStones,Stone1),

    between(0,LastStoneIndex,Stone2Index),
    Stone2Index>Stone1Index, %stones are sorted so there are no permutations
    nth0(Stone2Index,PlayerStones,Stone2),

    between(0,LastStoneIndex,Stone3Index),
    Stone3Index>Stone2Index,
    nth0(Stone3Index,PlayerStones,Stone3),

    Stones=[Stone1,Stone2,Stone3],

    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],


    get_moves(GameState,Player,Stones,InitialMoves,Offset,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,Offset,PossibleMove,Stones):-

    getPlayerStones(GameState,Player,PlayerStones),
    length(PlayerStones,NrStones),
    NrStones=2,
    !,
    
    nth0(0,PlayerStones,Stone1),
    nth0(1,PlayerStones,Stone2),
    Stones=[Stone1,Stone2],

    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],

    get_moves(GameState,Player,Stones,InitialMoves,Offset,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,Offset,PossibleMove,Stones):-

    getPlayerStones(GameState,Player,PlayerStones),
    length(PlayerStones,NrStones),
    NrStones=1,
    !,

    nth0(0,PlayerStones,Stone1),
    Stones=[Stone1],

    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],

    get_moves(GameState,Player,Stones,InitialMoves,Offset,PossibleMoves),
    member(PossibleMove,PossibleMoves).




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


valid_moves(GameState,Player,Offset,Moves):-
    findall(Stones-PossibleMove,choose_stones(GameState,Player,computer,Offset,PossibleMove,Stones), Moves).



enemy(white,black).

enemy(black,white).



min_moves_to_win(white,Offset,(R,_),Distance):-
    BlackRow = -Offset,
    Distance is sqrt(R-BlackRow).

min_moves_to_win(black,Offset,(R,_),Distance):-
    WhiteRow = Offset,
    Distance is sqrt(WhiteRow-R).

average( List, Average ):- 
    sumlist( List, Sum ),
    length( List, Length ),
    Length > 0, 
    Average is Sum / Length.

evaluate_move(GameState, Player, Value,Offset) :-
    getPlayerStones(GameState,Player,AllyStones),
    maplist(min_moves_to_win(Player,Offset),AllyStones,DistanceListAlly),
    average(DistanceListAlly,DistanceToWin),

    enemy(Player,Enemy),
    getPlayerStones(GameState,Enemy,EnemyStones),
    maplist(min_moves_to_win(Enemy,Offset),EnemyStones,DistanceListEnemy),
    average(DistanceListEnemy,DistanceToLose),

    length(EnemyStones,NrEnemyStones),

    Distance is DistanceToWin-DistanceToLose,
    
    Value is Distance + NrEnemyStones.



best_move(1, _, _, Moves,_, Stones-Move):-
    random_member(Stones-Move,Moves).


best_move(2, GameState, Player, Moves,Offset, Stones-Move):-
    setof(Value-(SomeStones,SomeMove), NewGameState^(member(SomeStones-SomeMove, Moves),
            move(GameState, Player, SomeStones, SomeMove, [],NewGameState),
            evaluate_move(NewGameState, Player, Value,Offset) ), [_V-(Stones,Move)|_]).



choose_move(GameState, Player,human-_, Offset,Stones-Move):-
    repeat,
    choose_stones(GameState,Player,human,Offset,PossibleMoves,Stones),
    length(PossibleMoves,NrPosMoves),
    NrPosMoves>0,
    write('Possible Moves'),
    nl,
    display_possible_moves(PossibleMoves),
    write('Choose your move:'),
    read(MoveText),
    parseMove(MoveText,Move).


choose_move(GameState, Player, computer-Level,Offset, Stones-Move):-
    valid_moves(GameState, Player,Offset, Moves),
    best_move(Level, GameState, Player, Moves,Offset, Stones-Move).


congratulate(Winner):-
    format('~a wins the game\n',[Winner]).

white_reach_opponent_home(GameState,Offset):-
    nth0(0, GameState, OpponentHome),
    between(0, Offset, Col),
    nth0(Col,OpponentHome,(_,Piece)),
    Piece=white.

black_reach_opponent_home(GameState,Offset):-
    WhiteRowIndex is 2*Offset,
    nth0(WhiteRowIndex, GameState, OpponentHome),
    between(0, Offset, Col),
    nth0(Col,OpponentHome,(_,Piece)),
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


moveStone(Board,Piece,(OldR,OldQ),(R,Q),_,NewBoard,(NewR,NewQ)):-
    NewR is OldR+R,
    NewQ is OldQ+Q,
    setPiece(Board,OldR,OldQ,empty,AuxBoard),
    setPiece(AuxBoard,NewR,NewQ,Piece,NewBoard).

    
move(GameState, _, [], _, _ , GameState).

move(GameState, Player, [Stone|R], Move, MovedStones,NewGameState):-
    moveStone(GameState,Player,Stone,Move,MovedStones,AuxGameState,NewPosition),
    move(AuxGameState,Player,R,Move,[NewPosition|MovedStones],NewGameState).


next_player(white,black).
next_player(black,white).


game_over(GameState-black, white, Offset):-
    white_reach_opponent_home(GameState, Offset),
    write('White has reached black\'s Home row'),
    nl.

game_over(GameState-black, white, Offset):-
    valid_moves(GameState, black,Offset, Moves),
    length(Moves,0),
    write('Black has no possible moves'),
    nl.


game_over(GameState-white, black, Offset):-
    black_reach_opponent_home(GameState, Offset),
    write('Black has reached white\'s Home row'),
    nl.

game_over(GameState-white, black, Offset):-
    valid_moves(GameState, white,Offset, Moves),
    length(Moves,0),
    write('White has no possible moves'),
    nl.



make_choice(TypeOfGame):-
    write('What mode do you want to play?'),
    nl,
    write('The game is white stones vs black stones'),
    nl,
    write('1. Player (white) vs Player (black)'),
    nl,
    write('2. Player (white) vs AI (black)'),
    nl,
    write('3. AI (white) vs Player (black)'),
    nl,
    write('4. AI vs AI'),
    nl,
    write('Your choice: '),
    read(TypeOfGame). 



choose_board_size(NumLados):-
    repeat,
    write('Choose the side of the hexagonal grid (minimum is 3, maximum is 13,default is 5):'),
    read(NumLados),
    NumLados>2,
    NumLados<14.

choose_level(1,_).

choose_level(TypeOfGame,Level):-
    TypeOfGame<5,
    TypeOfGame>0,
    write('Choose the game dificulty:'),
    nl,
    write('1. Easy mode'),
    nl, 
    write('2, Hard mode'),
    nl,
    write('Your choice: '),
    read(Level).

get_type_of_player(_,1,human).

get_type_of_player(white,2,human).
get_type_of_player(black,2,computer).

get_type_of_player(black,3,human).
get_type_of_player(white,3,computer).

get_type_of_player(_,4,computer).


game:-
    make_choice(TypeOfGame),
    choose_level(TypeOfGame,Level),
    choose_board_size(NumLados),
    initial_state(NumLados,GameState,Player),
    display_game(GameState,TypeOfGame,NumLados),
    game_cycle(GameState,Player,TypeOfGame,Level,NumLados).


game_cycle(GameState,Player,_,_,NumLados):-
    Offset is NumLados-1,
    game_over(GameState-Player, Winner,Offset), !,
    congratulate(Winner).

game_cycle(GameState,Player,TypeOfGame,Level, NumLados):-
    Offset is NumLados-1,
    get_type_of_player(Player,TypeOfGame,TypeOfPlayer),
    choose_move(GameState, Player,TypeOfPlayer-Level,Offset, Stones-Move),
    move(GameState, Player, Stones, Move, [],NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState,TypeOfGame, NumLados), !,
    game_cycle(NewGameState,NextPlayer,TypeOfGame,Level, NumLados).

    


