:- consult(board).
:- consult(print_board).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).

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
    getElementOfMatrix(Board,Line,Col,(_,Piece)). 

initial_state(GameState-white):-
    board(1,GameState).


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

choose_stones(GameState,Player,human,PossibleMoves,Stones):-
    choose_stone(GameState,Player,1,[],Stones),
    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],
    get_moves(GameState,Player,Stones,InitialMoves,PossibleMoves),
    !.


choose_stones(GameState,Player,computer,PossibleMove,Stones):-

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

    get_moves(GameState,Player,Stones,InitialMoves,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,PossibleMove,Stones):-

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

    get_moves(GameState,Player,Stones,InitialMoves,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,PossibleMove,Stones):-

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

    get_moves(GameState,Player,Stones,InitialMoves,PossibleMoves),
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


valid_moves(GameState,Player,Moves):-
    findall(Stones-PossibleMove,choose_stones(GameState,Player,computer,PossibleMove,Stones), Moves).



enemy(white,black).

enemy(black,white).


min_moves_to_win(white,(-4,_),-1):-!.

min_moves_to_win(white,(R,_),Distance):-
    BlackRow = -4,
    Distance is sqrt(R-BlackRow).

min_moves_to_win(black,(4,_),-1):-!.

min_moves_to_win(black,(R,_),Distance):-
    WhiteRow = 4,
    Distance is sqrt(WhiteRow-R).

average( List, Average ):- 
    sumlist( List, Sum ),
    length( List, Length ),
    Length > 0, 
    Average is Sum / Length.

evaluate_move(GameState, Player, Value) :-
    getPlayerStones(GameState,Player,AllyStones),
    maplist(min_moves_to_win(Player),AllyStones,DistanceListAlly),
    average(DistanceListAlly,DistanceToWin),

    enemy(Player,Enemy),
    getPlayerStones(GameState,Enemy,EnemyStones),
    maplist(min_moves_to_win(Enemy),EnemyStones,DistanceListEnemy),
    average(DistanceListEnemy,DistanceToLose),

    length(EnemyStones,NrEnemyStones),

    Distance is DistanceToWin-DistanceToLose,
    
    Value is Distance + NrEnemyStones.



best_move(1, _, _, Moves, Stones-Move):-
    random_member(Stones-Move,Moves).


best_move(2, GameState, Player, Moves, Stones-Move):-
    setof(Value-(SomeStones,SomeMove), NewGameState^( member(SomeStones-SomeMove, Moves),
            move(GameState, Player, SomeStones, SomeMove, [],NewGameState),
            evaluate_move(NewGameState, Player, Value) ), [_V-(Stones,Move)|_]).



choose_move(GameState, Player,human-_, Stones-Move):-
    repeat,
    choose_stones(GameState,Player,human,PossibleMoves,Stones),
    length(PossibleMoves,NrPosMoves),
    NrPosMoves>0,
    write('Possible Moves'),
    nl,
    display_possible_moves(PossibleMoves),
    write('Choose your move:'),
    read(MoveText),
    parseMove(MoveText,Move).


choose_move(GameState, Player, computer-Level, Stones-Move):-
    valid_moves(GameState, Player, Moves),
    best_move(Level, GameState, Player, Moves, Stones-Move).


congratulate(Winner):-
    format('~a wins the game\n',[Winner]).

white_reach_opponent_home(GameState):-
    nth0(0, GameState, OpponentHome),
    between(0, 4, Col),
    nth0(Col,OpponentHome,(_,Piece)),
    Piece=white.

black_reach_opponent_home(GameState):-
    nth0(8, GameState, OpponentHome),
    between(0, 4, Col),
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


game_over(GameState-black, white):-
    white_reach_opponent_home(GameState),
    write('White has reached black\'s Home row'),
    nl.

game_over(GameState-black, white):-
    valid_moves(GameState, black, Moves),
    length(Moves,0),
    write('Black has no possible moves'),
    nl.


game_over(GameState-white, black):-
    black_reach_opponent_home(GameState),
    write('Black has reached white\'s Home row'),
    nl.

game_over(GameState-white, black):-
    valid_moves(GameState, white, Moves),
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
    initial_state(GameState-Player),
    display_game(GameState,TypeOfGame),
    game_cycle(GameState,Player,TypeOfGame,Level).


game_cycle(GameState,Player,_,_):-
    game_over(GameState-Player, Winner), !,
    congratulate(Winner).

game_cycle(GameState,Player,TypeOfGame,Level):-
    get_type_of_player(Player,TypeOfGame,TypeOfPlayer),
    choose_move(GameState, Player,TypeOfPlayer-Level, Stones-Move),
    move(GameState, Player, Stones, Move, [],NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState,TypeOfGame), !,
    game_cycle(NewGameState,NextPlayer,TypeOfGame,Level).

    


