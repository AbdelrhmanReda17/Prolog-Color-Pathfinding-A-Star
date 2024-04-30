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

astar([[State, Path, G, _, _]|Rest], Goal, Closed, Board) :-
    (State == Goal  ->
        reverse([Goal|Closed], Cycle),
        format("cycle found: ~w", [Cycle]),nl
    ;
        getAllValidNeighbors([State, Path, G, _, _], Rest, Closed, Goal, Children, Board),
        append(Children, Rest, NewOpen),
        % write(NewOpen),nl,
        predsort(compareNewF, NewOpen, SortedAllChildren),
        % write(SortedAllChildren),nl,
        astar(SortedAllChildren, Goal, [State|Closed], Board)
    ).

compareNewF(Order, [[X1,Y1], _, G1, H1, F1], [[X2,Y2], _, G2, H2, F2]) :-
    (F1 == F2 ->
        (H1 == H2 ->
            (G1 == G2 ->
                compare(Order, [X1,Y1], [X2,Y2])
                ;
                compare(Order, G1, G2)
            )
            ;
            compare(Order, H1, H2)
        )
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
    \+ member(Next, Open),
    \+ member(Next, Closed).


% Define possible moves
move([X,Y], [X1,Y], 1) :- X1 is X+1. % Move right
move([X,Y], [X,Y1], 1) :- Y1 is Y+1. % Move down
move([X,Y], [X1,Y], 1) :- X1 is X-1, X1 >= 0. % Move left, ensuring the new X coordinate is within the board boundaries
move([X,Y], [X,Y1], 1) :- Y1 is Y-1, Y1 >= 0. % Move up, ensuring the new Y coordinate is within the board boundaries

