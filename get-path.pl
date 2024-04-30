:- discontiguous same_color/3.

% Define the predicate to check if two cells have the same color
same_color([X1,Y1], [X2,Y2], Board) :-
    cell_color(Board, [X1,Y1], C1),
    cell_color(Board, [X2,Y2], C2),
    C1 == C2.

% Define the predicate to get the color of a cell
cell_color(Board, [X,Y], Color) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

% Define the predicate to check if a cell is a valid cell on the board
valid_cell([X,Y], Board) :-
    length(Board, NRows),
    nth0(0, Board, FirstRow),
    length(FirstRow, NCols),
    X >= 0, X < NRows,
    Y >= 0, Y < NCols.

% Define the predicate to calculate the heuristic value
calculateH([X1,Y1], [X2,Y2], H) :-
    H is abs(X2 - X1) + abs(Y2 - Y1).


% Define the main predicate to reach the goal
find_path(Board, Start, Goal) :-
    same_color(Start, Goal, Board),
    astar([[Start, [], 0, 0, 0]], Goal, [], Board) , !.

find_path(_, _, _) :-
    format("No path exists.~n").

astar([[State, Parent, G, H, F]|Rest], Goal, Closed, Board) :-
    (State == Goal  ->
        printSolution( [State, Parent, G, H, F] , Closed), !
    ;
        getAllValidNeighbors([State, Parent, G, _, _], Rest, Closed, Goal, Children, Board),
        append(Rest, Children, NewOpen),
        predsort(compareNewF, NewOpen, SortedAllChildren),
        astar(SortedAllChildren, Goal, [[State, Parent, G, H, F]|Closed], Board)
    ).

compareNewF(Order, [_, _, _, _, F1], [_, _, _, _, F2]) :-
    (F1 == F2 ->
        true
        ;
        compare(Order, F1, F2)
    ).

% Define the predicate to get all valid neighbors of a node
getAllValidNeighbors([State, _, G, _, _], Open, Closed, Goal, Children , Board) :-
    findall([Next, State, NewG, NewH, NewF],
            (getNextState([State, _, G, _, _], Open, Closed, Goal, [Next, State, NewG, NewH, NewF], Board),
            valid_cell(Next, Board)),
            Children).

% Define the predicate to get the next state
getNextState([State, _, G, _, _], Open, Closed, Goal, [Next, State, NewG, NewH, NewF] , Board) :-
    move(State, Next, MoveCost),
    same_color(State,Next,Board),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    ( not(member([Next,_,_,_,_], Open)) ; memberButBetter(Next,Open,NewF) ),
    ( not(member([Next,_,_,_,_], Closed)); memberButBetter(Next,Closed,NewF) ).

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

% Define possible moves
move([X,Y], [X1,Y], 1) :- X1 is X+1. % Move right
move([X,Y], [X,Y1], 1) :- Y1 is Y+1. % Move down
move([X,Y], [X1,Y], 1) :- X1 is X-1, X1 >= 0. % Move left, ensuring the new X coordinate is within the board boundaries
move([X,Y], [X,Y1], 1) :- Y1 is Y-1, Y1 >= 0. % Move up, ensuring the new Y coordinate is within the board boundaries
