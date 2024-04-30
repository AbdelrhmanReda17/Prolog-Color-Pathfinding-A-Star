% Define valid moves
move([X,Y], [X1,Y],1) :- X1 is X+1. % Defines a move to the right from cell [X,Y].
move([X,Y], [X,Y1],1) :- Y1 is Y+1. % Defines a move downward from cell [X,Y].
move([X,Y], [X1,Y],1) :- X1 is X-1, X1 >= 0. % Defines a move to the left from cell [X,Y], ensuring the new X coordinate is within the board boundaries.
move([X,Y], [X,Y1],1) :- Y1 is Y-1, Y1 >= 0. % Defines a move upward from cell [X,Y], ensuring the new Y coordinate is within the board boundaries.

% Checks if a cell [X,Y] is valid on the board by ensuring its coordinates are within the board boundaries.
valid_cell([X,Y], Board) :-
    length(Board, NRows),
    length(Board, NCols),
    X >= 0, X =< NRows,
    Y >= 0, Y =< NCols,
    nth0(X, Board, Row),
    nth0(Y, Row,_).
    

% Predicate to get the color of a cell
cell_color(Board, [X,Y], Color) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

% Check if two cells have the same color
same_color([X1,Y1], [X2,Y2], Board) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, C1),
    nth0(X2, Board, Row2),
    nth0(Y2, Row2, C2),
    C1 == C2.

calculateH([X1,Y1], [X2,Y2], H) :-
    H is abs(X2 - X1) + abs(Y2 - Y1).

detect_path(Board, Start, Goal) :-
    same_color(Start, Goal, Board),
    search([[Start,[],0,0,0]], [], Goal,Board).
    


search(Open, Closed, Goal,Board):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _),
    CurrentState == Goal, 
    printSolution([CurrentState,Parent,G,H,F], Closed), !.


search(Open, Closed, Goal,Board):-
    getBestState(Open, CurrentNode, TmpOpen), %override
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children,Board), % override
    addChildren(Children, TmpOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed), 
    search(NewOpen, NewClosed, Goal,Board). 

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children,Board):-  %override
    findall(Next, getNextState(Node,Open,Closed,Goal,Next,Board) , Children).


% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).


getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

findMin([X], X):- !.

findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,HeadH,HeadF],
    TmpMin = [_,_,_,TmpH,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF],Board):- %override
    move(State, Next, MoveCost),
    valid_cell(Next,Board),
    same_color(State,Next,Board),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    ( not(member([Next,_,_,_,_], Open)) ; memberButBetter(Next,Open,NewF) ),
    ( not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).


memberButBetter(Next, List, NewF):-
    findall(F, member([Next,_,_,_,F], List), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.

printSolution([State, [], G, H, F],_):-
    write([State, G, H, F]), nl.

printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write([State, G, H, F]), nl.



