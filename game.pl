:- consult(board).
:- consult(print_board).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).


/**
* posToAxial(+Pos,+MiddleRow,-PosAxial).
* 
*
* Succeeds if the third argument is the conversion of Pos to axial coordinates.
*
* @param Pos - The position represented in the way the user sees (the coordinate system shown in the board)
* @param MiddleRow - The Size of the Hexagonal grid - 1, the index of the middle row (if the first row is 0)
* @param PosAxial - The position represented in axial coordinates
*/
posToAxial((Row,Col),MiddleRow,PosAxial):-
    char_code('A',ACharCode),
    MiddleRowCharCode is ACharCode + MiddleRow,
    char_code(Row,RowCharCode),
    R is MiddleRowCharCode-RowCharCode,
    AuxOffset is MiddleRow+R+1, %1 because columns start at 1 rather than 0 in Taacoca
    ColOffset is min(MiddleRow+1,AuxOffset),
    Q is Col-ColOffset,
    PosAxial=(R,Q).


/**
* getElementOfMatrix(+Matrix,+Line,+Col,-Element).
* 
*
* Succeeds if Element=Matrix[Line][Col].
*
* @param Matrix - The Matrix whose Element we want to retrieve
* @param Line - The Line in which that Element is
* @param Col - The Col in which that ELement is
* @param Element - The Element at Matrix[Line][Col]
*/
getElementOfMatrix(Matrix,Line,Col,Element):-
    nth0(Line, Matrix, Row),
    nth0(Col, Row, Element).


/**
* getPiece(+Board,+PosAxial,+MiddleRow,-Piece).
* 
*
* Succeeds if Piece is the piece in the Board at PosAxial.
* Finds the Line and Col in the Board corresponding to the posAxial and retrieves that Piece
*
* @param Board - The internal representation of the board
* @param PosAxial - The Position (in axial coordinates) of the piece we want to retrieve
* @param MiddleRow - The Size of the Hexagonal grid - 1, the index of the middle row (if the first row is 0)
* @param Piece - The Piece in the board at PosAxial
*/
getPiece(Board,(R,Q),MiddleRow,Piece):-
    Line is R+MiddleRow,
    AuxOffset is MiddleRow+R,
    ColOffset is min(MiddleRow,AuxOffset),
    Col is Q + ColOffset,
    getElementOfMatrix(Board,Line,Col,(_,Piece)). 

/**
* possible_Q(+MiddleRow,+SymmetricMiddleRow,+R,-Q).
* 
*
* Succeeds if for the board size which has the corresponding MiddleRow and SymmetricMiddleRow,
* the Q is a possible value for the given R.
*
* @param MiddleRow - The Size of the Hexagonal grid - 1, the index of the middle row (if the first row is 0)
* @param SymmetricMiddleRow - SymmetricMiddleRow = -MiddleRow
* @param R - the R of the axial coordinate
* @param Q - a Q so that (R,Q) is on the board for the given board size.
*/
possible_Q(MiddleRow,_,R,Q):-
    R=<0,!,
    SmallestQ is -MiddleRow-R,
    between(SmallestQ,MiddleRow,Q).


possible_Q(MiddleRow,SymmetricMiddleRow,R,Q):-
    R>0,!,
    BiggestQ is MiddleRow-R,
    between(SymmetricMiddleRow,BiggestQ,Q).


/**
* getPieceStart(+R,+Q,-Piece)
* 
*
* Succeeds if Piece is the piece at the pos (R,Q) at the start of the game
*
* @param R - the R of the axial coordinate of the piece
* @param Q - the Q of the axial coordinate of the piece
* @param Piece - The Piece on the board at the axial coordinate given by (R,Q) at the beggining of the game
*/
getPieceStart(R,_,empty):-
    R>=(-1),
    R=<1,!.

getPieceStart(R,Q,black):-
    R=<1,
    SymmetricR is -R,
    Q=<SymmetricR,
    Q>=0,!.

getPieceStart(R,_,empty):-
    R=<1,!.

getPieceStart(R,Q,white):-
    R>1,
    SymmetricR is -R,
    Q>=SymmetricR,
    Q=<0,!.

getPieceStart(R,_,empty):-
    R>1.


/**
* possible_R_Q_Piece(+NumLados,-R,-Q,-Piece).
*
* Given the number of hexagons in a side, returns a possible R,Q and the piece at that (R,Q)
*
* @param NumLados - The number of hexagons in a side
* @param R - The axial coordinate of the piece
* @param Q - The axial coordinate of the piece
* @param Piece - The piece that should be at the axial coordinate (R,Q)
*/
possible_R_Q_Piece(NumLados,R,Q,Piece):-
    MiddleRow is NumLados-1,
    SymmetricMiddleRow is -MiddleRow,
    between(SymmetricMiddleRow,MiddleRow,R),
    possible_Q(MiddleRow,SymmetricMiddleRow,R,Q),
    getPieceStart(R,Q,Piece).


/**
* getEveryElementExceptLast(+List,-EveryElementExceptLast)
*
* Given a list, it returns every element except the last
*
* @param List - The input list to which we want to remove the last element
* @param EveryElementExceptLast - Every element of that list except the last one
*/
getEveryElementExceptLast(List,EveryElementExceptLast):-
    reverse(List,ListReversed),
    ListReversed = [_|EveryElementExceptLastReversed],
    reverse(EveryElementExceptLastReversed,EveryElementExceptLast).


/**
* convertAux(+GameStateList,+CurrentR, +CurrentGameStateMatrix,-GameStateMatrix)
*
* Auxiliary function to convert that converts the remaining GameStateList, appending it to the GameStateMatrix
* Positions with the same R become one of the lines of the matrix.
* @param GameStateList - GameState represented in an list
* @param CurrentR - The R of the Element that was last added to the GameStateMatrix
* (used to see if the next step is to create a new line at the matrix or append a new element to the last line)
* @param CurrentGameStateMatrix - The Matrix we have built up to the current function call
* @param GameStateMatrix - the internal representation of the GameState (using a Matrix)
*/
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


/**
* convertArrayToMatrix(+GameStateList-GameStateMatrix)
*
* Converts the GameState represented in an list to the GameState represented in a Matrix 
* Positions with the same R become one of the lines of the matrix.
* @param GameStateList - GameState represented in an list
* @param GameStateMatrix - the internal representation of the GameState (using a Matrix)
*/
convertArrayToMatrix([((R,Q),Piece)|Rest],GameStateMatrix):-
    convertAux(Rest,R,[[((R,Q),Piece)]],GameStateMatrix).

/**
* initial_state(+NumLados,-GameState,-Player)
*
* Given the number of hexagons in a side, it creates the internal representation of the initial state of the 
* game and the Player whose turn it is to play
* @param NumLados - The number of hexagons in a side
* @param GameState - The internal representation of the initial GameState (Board) 
* @param Player - The player whose turn it is to play
*/
initial_state(NumLados,GameState,white):-
    %board(1,GameState).
    findall(((R,Q),Piece),possible_R_Q_Piece(NumLados,R,Q,Piece),GameStateList),
    convertArrayToMatrix(GameStateList,GameState).

/**
* pos_in_board((+R,+Q),+MiddleRow)
*
* Succedes if given an axial coordinate, that coordinate is in the board
*
* @param R - The R of the axial coordinate
* @param Q - The Q of the axial coordinate
* @param MiddleRow - The middle row of the board
*/
pos_in_board((R,Q),MiddleRow):-
    R=<0,
    !,
    LowerBoundQ is -MiddleRow-R,
    between(LowerBoundQ,MiddleRow,Q).
pos_in_board((R,Q),MiddleRow):-
    R>0,
    !,
    UpperBoundQ is MiddleRow-R,
    SymmetricMiddleRow is -MiddleRow, 
    between(SymmetricMiddleRow,UpperBoundQ,Q).

/**
* other_stones(+Stone,+AllStones,-OtherStones)
*
* Succedes if given a position Stone and a list of positions AllStones, OtherStones is a list of all positions
* in AllStones except for Stone.
*
* @param Stone - The Position of the Stone which we want to be removed in OtherStones
* @param AllStones - The Positions of all Stones
* @param MiddleRow - The Positions of all Stones except the one given by Stone.
*/
other_stones(Stone,[Stone|OtherStones],OtherStones).

other_stones(Stone,[X|R],OtherStones):-
    other_stones(Stone,R,OtherStonesRest),
    OtherStones=[X|OtherStonesRest].

/**
* legal_move_stone(+GameState,+Move,+Piece,+Stones,+MiddleRow,+Stone)
*
* Suceedes if given a GameState, a Move, a Piece, a list of Stones, the MiddleRow of the board and a Stone,
* the Stone can move in the Move direction
*
* @param GameState - The internal representation of the GameState (Board)
* @param Move - The direction in which the Stone is going to move
* @param Piece - The color of the Stone (black/white)
* @param Stones - The list of stones chosen to move
* @param MiddleRow - The middle row of the board
* @param Stone - One of the stones that is going to move
*/
legal_move_stone(GameState,(R,Q),Piece,Stones,MiddleRow,Stone):-
    other_stones(Stone,Stones,OtherChosenStones),
    (OldR,OldQ)=Stone,
    NewR is OldR+R,
    NewQ is OldQ+Q,
    getPiece(GameState,(NewR,NewQ),MiddleRow,PieceInNewPos),
    (PieceInNewPos\=Piece;
    member((NewR,NewQ),OtherChosenStones)),
    pos_in_board((NewR,NewQ),MiddleRow).  

/**
* legal_move(+GameState,+Stones,+Move,+Piece,+MiddleRow)
*
* Suceedes if given a GameState, a list of Stones, a Move, a Piece and the MiddleRow of the board,
* all the Stones can move in the Move direction
*
* @param GameState - The internal representation of the GameState (Board)
* @param Stones - The list of stones chosen to move
* @param Move - The direction in which the Stones are going to move
* @param Piece - The color of the Stones (black,white)
* @param MiddleRow - The middle row of the board
*/
legal_move(GameState,Stones,Move,Piece,MiddleRow):-
    maplist(legal_move_stone(GameState,Move,Piece,Stones,MiddleRow),Stones).


/**
* get_moves(+GameState,+Piece,+Stones,+InitialMoves,+MiddleRow,-PossibleMoves)
*
* Given a GameState, a Piece, a list of Stones, a list of InitialMoves, the MiddleRow of the board,
* it returns a list of PossibleMoves that are legal moves for the given Stones
*
* @param GameState - The internal representation of the GameState (Board)
* @param Piece - The color of the Stones (black,white)
* @param Stones - The list of stones chosen to move
* @param InitialMoves - The list of all the 6 directions in which the Stones can move
* @param MiddleRow - The middle row of the board
* @param PossibleMoves - The list of possible moves that are legal for the given Stones
*/
get_moves(GameState,Piece,Stones,InitialMoves,MiddleRow,PossibleMoves):-
    findall(Move,(member(Move,InitialMoves),legal_move(GameState,Stones,Move,Piece,MiddleRow)),PossibleMoves).

/**
* getPlayerStonesLine(+Line,+Player,+CurrentStones,-PlayerStones)
*
* Given a Line (or part of a Line) of the Game State Matrix and a player,
* it appends all of those player's stones in that line  to the CurrentStones list,
* and returns it in PlayerStones.
*
* @param Line - One line (or part of a line) of the internal representation of the GameState (Board)
* @param Player - The player whose stones we want to get
* @param CurrentStones - The stones we got so far
* @param PlayerStones - All fo the stones in that line
*/

getPlayerStonesLine([],_,CurrentStones,CurrentStones).

getPlayerStonesLine([(Pos,Player)|RestLine],Player,CurrentStones,PlayerStones):-
    !,
    
    getPlayerStonesLine(RestLine,Player,[Pos|CurrentStones],PlayerStones).

getPlayerStonesLine([_|RestLine],Player,CurrentStones,PlayerStones):-
    getPlayerStonesLine(RestLine,Player,CurrentStones,PlayerStones).


/**
* getPlayerStonesAux(GameState,Player,CurrentStones,PlayerStones)
*
* Auxilliary function to getPLayerStones that given the Game State Matrix (or part of it) 
* and a player, it appends all of those player's stones in that matrix to the CurrentStones
* list, and returns it in PlayerStones.
*
* @param GameState - the internal representation of the GameState (or part of it)
* @param Player - The player whose stones we want to get
* @param CurrentStones - The stones we got so far
* @param PlayerStones - All fo the stones in the matrix
*/

getPlayerStonesAux([],_,CurrentStones,CurrentStones).

getPlayerStonesAux([Line|OtherLines],Player,CurrentStones,PlayerStones):-
    getPlayerStonesLine(Line,Player,[],LineStones),
    append(CurrentStones,LineStones,NewCurrentStones),
    getPlayerStonesAux(OtherLines,Player,NewCurrentStones,PlayerStones).

/**
* getPlayerStones(GameState,Player,PlayerStones)
*
* Given a GameState, it returns a list with all the Player's Stones
*
* @param GameState - the internal representation of the GameState 
* @param Player - The player whose stones we want to get
* @param PlayerStones - All of the stones of that player
*/

getPlayerStones(GameState,Player,PlayerStones):-
    getPlayerStonesAux(GameState,Player,[],PlayerStones).


/**
* choose_stone(+GameState,+Player,+PieceNr,+StonesToChoose,+MiddleRow,+StonesChosenSoFar,-Stones)
*
* Given a GameState, a Player, a PieceNr and the MiddleRow of the board, it asks
* the user to choose a stone until the position given corresponds to the position of a stone
* it can choose, appends that stone to the stones chosen so far and recursively calls the 
* same function until PieceNr>StonesToChoose (meaning #StonesToChoose stones have been chosen already)
*
* @param GameState - The internal representation of the GameState (Board)
* @param Player - The player whose turn it is to play
* @param PieceNr - The number of Stones that the Player has chosen so far
* @param StonesToChoose - The number of Stones that the Player has to choose in total
* @param MiddleRow - The middle row of the board
* @param StonesChosenSoFar - The list of Stones chosen so far
* @param Stones - The Stones chosen by the player
*/
choose_stone(_,_,PieceNr,StonesToChoose,_,Stones,Stones):-
    PieceNr>StonesToChoose,!.

choose_stone(GameState,Player,PieceNr,StonesToChoose,MiddleRow,StonesChosenSoFar,Stones):-
    repeat,
    format('Enter stone ~d\'s position\n',[PieceNr]),
    write('Line: '),
    read(Line),
    write('Col: '),
    read(Col),
    posToAxial((Line,Col),MiddleRow,Stone),
    nl,
    \+member(Stone,StonesChosenSoFar),
    getPiece(GameState,Stone,MiddleRow,Player),
    NewPieceNr is PieceNr+1,
    choose_stone(GameState,Player,NewPieceNr,StonesToChoose,MiddleRow,[Stone|StonesChosenSoFar],Stones).



/**
* choose_stones(+GameState,+Player,+TypeOfPlayer,+MiddleRow,-PossibleMoves,-Stones)
*
* Given a GameState, a Player, a TypeOfPlayer (human/computer) and the MiddleRow of the board,
* it chooses (up to) 3 Stones and the Moves(directions) that can be chosen for those stones
*
* @param GameState - The internal representation of the GameState (Board)
* @param Player - The player whose turn it is to play
* @param TypeOfPlayer - human/computer
* @param MiddleRow - The middle row of the board
* @param PossibleMoves - The list of directions the chosen stones can go
* @param Stones - The Stones chosen 
*/
choose_stones(GameState,Player,human,MiddleRow,PossibleMoves,Stones):-
    getPlayerStones(GameState,Player,PlayerStones),
    length(PlayerStones,NrAllyStones),
    StonesToChoose is min(NrAllyStones,3),
    choose_stone(GameState,Player,1,StonesToChoose,MiddleRow,[],Stones),
    E = (0,1),
    SE = (1,0),
    SW = (1,-1),
    W = (0,-1),
    NW = (-1,0),
    NE = (-1,1),
    InitialMoves=[E,SE,SW,W,NW,NE],
    get_moves(GameState,Player,Stones,InitialMoves,MiddleRow,PossibleMoves),
    !.


choose_stones(GameState,Player,computer,MiddleRow,PossibleMove,Stones):-
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


    get_moves(GameState,Player,Stones,InitialMoves,MiddleRow,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,MiddleRow,PossibleMove,Stones):-

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

    get_moves(GameState,Player,Stones,InitialMoves,MiddleRow,PossibleMoves),
    member(PossibleMove,PossibleMoves).


choose_stones(GameState,Player,computer,MiddleRow,PossibleMove,Stones):-

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

    get_moves(GameState,Player,Stones,InitialMoves,MiddleRow,PossibleMoves),
    member(PossibleMove,PossibleMoves).



/**
* display_move(+Move)
* Displays the move (direction) in a human readable format
* @param Move - The move to be displayed
*/
display_move((0,1)):-write('East (E)'),nl.

display_move((1,0)):-write('South-East (SE)'),nl.

display_move((1,-1)):-write('South-West (SW)'),nl.

display_move((0,-1)):-write('West (W)'),nl.

display_move((-1,0)):-write('North-West (NW)'),nl.

display_move((-1,1)):-write('North-East (NE)'),nl.

/**
* parseMove(+Move,-AxialMove)
* Parses the move (direction) from a human readable format to an axial format
* @param Move - The move to be parsed
* @param AxialMove - The move in axial format
*/
parseMove('E',(0,1)).

parseMove('SE',(1,0)).

parseMove('SW',(1,-1)).

parseMove('W',(0,-1)).

parseMove('NW',(-1,0)).

parseMove('NE',(-1,1)).

/**
* display_possible_moves(+PossibleMoves)
* Displays all possible moves in a human readable format
* @param PossibleMoves - The list of possible moves
*/
display_possible_moves(PossibleMoves):-
    maplist(display_move,PossibleMoves).

/**
* valid_moves(+GameState,+Player,+MiddleRow,-Moves)
* Given a game state and a player, finds all the valid moves (a move here is (up to) 3 stones and a direction)
* @param GameState - The current game state
* @param Player - The player to find the moves for
* @param MiddleRow - The middle row of the board
* @param Moves - The list of valid moves
*/
valid_moves(GameState,Player,MiddleRow,Moves):-
    findall(Stones-PossibleMove,choose_stones(GameState,Player,computer,MiddleRow,PossibleMove,Stones), Moves).


/**
* enemy(+Player,-Enemy)
* Finds the enemy (white or black) of the player (white or black)
* @param Player - The player to find the enemy for (white or black)
* @param Enemy - The enemy of the player (white or black)
*/
enemy(white,black).

enemy(black,white).


/**
* min_moves_to_win(+Player,+MiddleRow,(+R,+Q),-Distance)
* Finds the square root of the minimum number of moves needed to win for the nearest stone of the player to win 
*
* @param Player - The player to find the minimum moves for (white or black)
* @param MiddleRow - The middle row of the board
* @param (R,Q) - The coordinates of the stone
* @param Distance - The square root of the minimum number of moves needed to win
*/
min_moves_to_win(white,MiddleRow,(R,_),Distance):-
    BlackRow = -MiddleRow,
    Distance is sqrt(R-BlackRow).

min_moves_to_win(black,MiddleRow,(R,_),Distance):-
    WhiteRow = MiddleRow,
    Distance is sqrt(WhiteRow-R).

/**
* average(+List,-Average)
* Finds the average of a list of numbers
* @param List - The list of numbers
* @param Average - The average of the list
*/
average( List, Average ):- 
    sumlist( List, Sum ),
    length( List, Length ),
    Length > 0, 
    Average is Sum / Length.

/**
* value(+GameState, +Player, +MiddleRow, -Value) 
* Rates a GameState for a given player (the better the game state, the smaller the value)
* @param GameState - Current game state
* @param Player - the player for whom we are rating the game state
* @param MiddleRow - the middle row of the board
* @param Value - The rating of the game state for that player (smaller = better)
*/

value(GameState, Player, MiddleRow, Value) :-
    getPlayerStones(GameState,Player,AllyStones),
    maplist(min_moves_to_win(Player,MiddleRow),AllyStones,DistanceListAlly),
    average(DistanceListAlly,DistanceToWin),

    enemy(Player,Enemy),
    getPlayerStones(GameState,Enemy,EnemyStones),
    maplist(min_moves_to_win(Enemy,MiddleRow),EnemyStones,DistanceListEnemy),
    average(DistanceListEnemy,DistanceToLose),

    length(EnemyStones,NrEnemyStones),

    Distance is DistanceToWin-DistanceToLose,
    
    Value is Distance + NrEnemyStones.


/**
* best_move(+Difficulty, +GameState, +Player, +Moves, +MiddleRow, -Stones-Move)
* In the easy mode returns a random move from the list of valid moves
* In the hard mode returns the best move (the one that minimizes the value function)
*
* @param Difficulty - The difficulty of the game (1 - easy, 2 - hard)
* @param GameState - The current game state
* @param Player - The player to find the best move for (white or black)
* @param Moves - The list of valid moves ([stones-direction]) that the player can make
* @param MiddleRow - The middle row of the board
* @param Move - The best move for the player
*/
best_move(1, _, _, Moves,_, Stones-Move):-
    random_member(Stones-Move,Moves).


best_move(2, GameState, Player, Moves,MiddleRow, Stones-Move):-
    setof(Value-(SomeStones,SomeMove), NewGameState^(member(SomeStones-SomeMove, Moves),
            move(GameState, Player, SomeStones, SomeMove, [],NewGameState),
            value(NewGameState, Player, MiddleRow, Value) ), [_V-(Stones,Move)|_]).


/**
* choose_move(+GameState, +Player, +computer-Level,+MiddleRow, -Stones-Move)
* Given a player (black or white), the type of player (human or computer) and 
* the difficulty (easy or hard), A move is chosen for a given gamestate and board size
*
* @param GameState - The current game state
* @param Player - The player to find the best move for (white or black)
* @param TypeOfPlayer-Level - the type of player (human or computer) and the difficulty (easy or hard)
* @param MiddleRow - The middle row of the board
* @param Move - A move (stones and direction) is chosen for the player
*/
choose_move(GameState, Player,human-_, MiddleRow,Stones-Move):-
    repeat,
    choose_stones(GameState,Player,human,MiddleRow,PossibleMoves,Stones),
    length(PossibleMoves,NrPosMoves),
    NrPosMoves>0,
    repeat,
    write('Possible Moves'),
    nl,
    display_possible_moves(PossibleMoves),
    write('Choose your move:'),
    read(MoveText),
    parseMove(MoveText,Move).


choose_move(GameState, Player, computer-Level,MiddleRow, Stones-Move):-
    valid_moves(GameState, Player,MiddleRow, Moves),
    best_move(Level, GameState, Player, Moves,MiddleRow, Stones-Move).

/**
* congratulate(+Winner)
* Prints a message congratulating the winner
* @param Winner - The winner of the game
*/
congratulate(Winner):-
    format('~a wins the game\n',[Winner]).

/**
* white_reach_opponent_home(+GameState,+MiddleRow)
* Checks if the white player has reached the opponent home
* @param GameState - The current game state
* @param MiddleRow - The middle row of the board
*/
white_reach_opponent_home(GameState,MiddleRow):-
    nth0(0, GameState, OpponentHome),
    between(0, MiddleRow, Col),
    nth0(Col,OpponentHome,(_,Piece)),
    Piece=white.

/**
* black_reach_opponent_home(+GameState,+MiddleRow)
* Checks if the black player has reached the opponent home
* @param GameState - The current game state
* @param MiddleRow - The middle row of the board
*/
black_reach_opponent_home(GameState,MiddleRow):-
    WhiteRowIndex is 2*MiddleRow,
    nth0(WhiteRowIndex, GameState, OpponentHome),
    between(0, MiddleRow, Col),
    nth0(Col,OpponentHome,(_,Piece)),
    Piece=black.

/**
* setPieceLine(+Line,+R,+Q,+Piece,-NewLine)
* Sets a piece on a given axial coordinate in a line
*
* @param Line - The line to set the piece on
* @param R - The row of the position
* @param Q - The column of the position
* @param Piece - The piece to set
* @param NewLine - The new line with the piece set
*/
setPieceLine([((R,Q),_)|Rest],R,Q,Piece,[((R,Q),Piece)|Rest]):-!.

setPieceLine([X|Rest],R,Q,Piece,NewLine):-
    setPieceLine(Rest,R,Q,Piece,NewRest),
    NewLine=[X|NewRest].

/**
* setPiece(+Board,+R,+Q,+Piece,-NewBoard)
*
* Sets a piece on a given position in a board
*
* @param Board - The board to set the piece on
* @param R - The row of the position
* @param Q - The column of the position
* @param Piece - The piece to set
* @param NewBoard - The new board with the piece set
*/
setPiece([Line|Rest],R,Q,Piece,NewBoard):-
    setPieceLine(Line,R,Q,Piece,NewLine),!,
    NewBoard=[NewLine|Rest].

setPiece([Line|Rest],R,Q,Piece,NewBoard):-
    setPiece(Rest,R,Q,Piece,NewRest),
    NewBoard=[Line|NewRest].

/**
* moveStone(+Board,+Piece,+OldPos,+Move,+MovedStones,-NewBoard,-NewPos)
* Moves a stone from a given position to a new position
* Since we are moving multiple stones, we don't want to set the old position to empty
* if that position is where some other stone moved. 
*
* @param Board - The board to move the stone on
* @param Piece - The piece to move
* @param OldPos - The old position of the piece
* @param Move - The direction to move the piece
* @param MovedStones - The positions of the stones that were already moved
* @param NewBoard - The new board with the piece moved
* @param NewPos - The new position of the piece
*/
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

    
/**
* move(+GameState,+Piece,+OldPos,+Move,+MovedStones,-NewGameState)
* Moves a stone from a given position to a new position
*
* @param GameState - The board to move the stones on
* @param Piece - The piece to move
* @param OldPos - A list with the current positions of the pieces
* @param Move - The direction to move the pieces
* @param MovedStones - The stones that have already been moved
* @param NewGameState - The new board with the pieces moved
*/
move(GameState, _, [], _, _ , GameState).

move(GameState, Player, [Stone|R], Move, MovedStones,NewGameState):-
    moveStone(GameState,Player,Stone,Move,MovedStones,AuxGameState,NewPosition),
    move(AuxGameState,Player,R,Move,[NewPosition|MovedStones],NewGameState).

/**
* next_player(+Player, -NextPlayer)
* Given the current player returns the next player
* @param Player The current player
* @param NextPlayer The next player
*/
next_player(white,black).
next_player(black,white).

/**
* game_over(+GameState, +Player, +MiddleRow)
* Checks if the game is over, and if it is, prints the winner
* @param GameState The current state of the game
* @param Player The player that has just played
* @param MiddleRow The middle row of the board
*/
game_over(GameState-black, white, MiddleRow):-
    white_reach_opponent_home(GameState, MiddleRow),
    write('White has reached black\'s Home row'),
    nl.

game_over(GameState-black, white, MiddleRow):-
    valid_moves(GameState, black,MiddleRow, Moves),
    length(Moves,0),
    write('Black has no possible moves'),
    nl.


game_over(GameState-white, black, MiddleRow):-
    black_reach_opponent_home(GameState, MiddleRow),
    write('Black has reached white\'s Home row'),
    nl.

game_over(GameState-white, black, MiddleRow):-
    valid_moves(GameState, white,MiddleRow, Moves),
    length(Moves,0),
    write('White has no possible moves'),
    nl.


/**
* make_choice(-TypeOfGame)
*
* This predicate asks the user to choose the type of game he wants to play
* and returns the type of game in TypeOfGame
*
* @param TypeOfGame - the type of game the user wants to play
*/
make_choice(TypeOfGame):-
    write('What mode do you want to play+'),
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


/**
* choose_board_size(-NumLados)
*
* This predicate asks the user to choose the side of the hexagonal grid
* and returns the size chosen in NumLados
*
* @param NumLados - the size of the board
*/
choose_board_size(NumLados):-
    repeat,
    write('Choose the side of the hexagonal grid (minimum is 3, maximum is 13,default is 5):'),
    read(NumLados),
    NumLados>2,
    NumLados<14.

/**
* choose_level(+TypeOfGame,-Level)
*
* Given the type of game that the player is playing this predicate asks the user to choose
* the difficulty of the game and returns the difficulty chosen in Level
*
* @param TypeOfGame - the type of game the user wants to play
* @param Level - the dificulty of the game
*/
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

/**
* get_type_of_player(+Player,+TypeOfGame,-TypeOfPlayer)
*
* Given the type of game that the player is playing and the player (white,black) 
* that is playing this predicate returns the type of the player (human or computer)
*
* @param Player - the player that is playing (white or black)
* @param TypeOfGame - the type of game the user is playing
* @param TypeOfPlayer - the type of the player
*/
get_type_of_player(_,1,human).

get_type_of_player(white,2,human).
get_type_of_player(black,2,computer).

get_type_of_player(black,3,human).
get_type_of_player(white,3,computer).

get_type_of_player(_,4,computer).

/**
* display_player(+Player)
*
* Displays a message indicating the player that has the next turn
*
* @param Player - the player that has the next turn
*/
display_player(Player):-
    format('~a\'s Turn\n',[Player]).

/**
* play
* This predicate is the main predicate of the game. It is responsible for the game cycle.
* It calls the predicates that ask the user to choose the type of game, the difficulty of the game
* and the size of the board. It then calls the predicate initial_state to get the initial state of the game
* and then calls the predicate display_game to display the initial state of the game.
* It then calls the predicate game_cycle to start the game cycle.
*/
play:-
    make_choice(TypeOfGame),
    choose_level(TypeOfGame,Level),
    choose_board_size(NumLados),
    initial_state(NumLados,GameState,Player),
    display_game(GameState,TypeOfGame,NumLados),
    game_cycle(GameState,Player,TypeOfGame,Level,NumLados).

/**
* game_cycle(+GameState,+Player,+TypeOfGame,+Level,+NumLados)
*
* It receives the current state of the game, the player that has the next turn,
* the type of game the user is playing, the dificulty of the game and the number of hexagons 
* in a side of the board.
* It displays the current state of the game, asks the player to choose a move and then updates the game state.
* It then checks if the game is over and if it is it displays a message indicating the winner.
* If the game is not over it calls itself again with the new game state and the next player.
*
* @param GameState - the current state of the game
* @param Player - the player that has the next turn
* @param TypeOfGame - the type of game the user is playing
* @param Level - the dificulty of the game
* @param NumLados - the size of the board
*/
game_cycle(GameState,Player,_,_,NumLados):-
    MiddleRow is NumLados-1,
    game_over(GameState-Player, Winner,MiddleRow), !,
    congratulate(Winner).

game_cycle(GameState,Player,TypeOfGame,Level, NumLados):-
    MiddleRow is NumLados-1,
    get_type_of_player(Player,TypeOfGame,TypeOfPlayer),
    display_player(Player),
    choose_move(GameState, Player,TypeOfPlayer-Level,MiddleRow, Stones-Move),
    move(GameState, Player, Stones, Move, [],NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState,TypeOfGame, NumLados), !,
    game_cycle(NewGameState,NextPlayer,TypeOfGame,Level, NumLados).

    


