/* MOVE ROOMS */
move(NewX, NewY) :-
           is_adjacent(NewX, NewY),
           retract(current_room(_,_)),       % retract old position
           assert(current_room(NewX, NewY)),        % assert new position
           change_score(-1),
           format("New coordinates: (~w, ~w)~n", [NewX, NewY]),
           assess.

backtrack(room(NewX, NewY)) :-                    % go back through a safe square
           retract(current_room(_,_)),           % retract old position
           assert(current_room(NewX, NewY)),     % assert new position
           change_score(-1),
           format("Backtrack to room: (~w, ~w)~n", [NewX, NewY]).

travel_to(Destination) :-
     here(Current_room),
     (   adjacent_rooms(Current_room, Destination)
     ->  enter_room(Destination)
     ;   shortest(Current_room, Destination, Path),  % compute shortest path
         travel_path(Path, Travel_path),             % determine travel path
         traverse(Travel_path),
         enter_room(Destination)
     ).

enter_room(Destination) :-              % 'move
        safe(Safe),
        member(Destination, Safe),      % check if the destination is safe
        backtrack(Destination)          % if it's safe, backtrack there
    ;
        get_coordinates(Destination, X, Y),  % get coordinates
        move(X,Y).                      % if not safe, move there


% Define 'traverse' predicate to move along path = traverse(Path).
traverse([]).
traverse([Room|RestPath]) :-
    backtrack(Room),
    traverse(RestPath).

/* SHORTEST PATH */
% Find the shortest safe path from Origin room to Destination using BFS
shortest(Origin, Destination, Path) :-
    bfs1([[Destination]], Origin, Path),
    %bfs1([[Destination]], Origin, PathReversed),
    %reverse(PathReversed, Path),
    !.

% BFS algorithm
bfs1([[Origin|Path]|_], Origin, [Origin|Path]).
bfs1([Path|Paths], Origin, FinalPath) :-
    extend(Path, NewPaths),
    append(Paths, NewPaths, ExtendedPaths),
    bfs1(ExtendedPaths, Origin, FinalPath).

extend([Node|Path], NewPaths) :-
    old_neighbors(Node, Adjacent),
    findall([AdjacentNode, Node|Path],
            (member(AdjacentNode, Adjacent),
             not(member(AdjacentNode, [Node|Path]))),
            NewPaths).

% Shorten the path to exclude starting and finihing rooms
travel_path(Path, Route) :-
    append([_|T], [_], Path),
    append(T, [], Route).


/* MOVEMENT FOR MANUAL PLAY */
walk(north) :-
            current_room(X, Y),
            NewY is Y + 1,
            move(X, NewY).

walk(south) :-
            current_room(X, Y),
            NewY is Y - 1,
            move(X, NewY).

walk(east) :-
            current_room(X, Y),
            NewX is X + 1,
            move(NewX, Y).

walk(west) :-
            current_room(X, Y),
            NewX is X - 1,
            move(NewX, Y).

