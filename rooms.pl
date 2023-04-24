/* ROOM ARRANGEMENT */

floor(X) :- X < 1.

ceiling(X) :-
           board_size(Max),
           X > Max.

wall(X) :- floor(X); ceiling(X).

horiz_move_possible(X1, Y1, X2, Y2) :-
           abs(X1 - X2) =:= 1,
           Y1 =:= Y2,
           \+ wall(X1),
           \+ wall(X2).

vert_move_possible(X1, Y1, X2, Y2) :-
           abs(Y1 - Y2) =:= 1,
           X1 =:= X2,
           \+ wall(Y1),
           \+ wall(Y2).

adjacent(X, Y, X1, Y1) :-
          horiz_move_possible(X, Y, X1, Y1);
          vert_move_possible(X, Y, X1, Y1).

is_adjacent(X1, Y1) :-    % return true if coordinates are adjacent to current_room
    current_room(X, Y),
    adjacent(X, Y, X1, Y1).
    % format("Current room (~w, ~w) is adjacent to new room (~w, ~w) ~n", [X, Y, X1, Y1]).

adjacent_rooms(Room1, Room2) :-  % return true if adjacent rooms
     Room1 = room(X1,Y1),
     Room2 = room(X2,Y2),
     Xdiff is abs(X1 - X2),
     Ydiff is abs(Y1 - Y2),
     (Xdiff =:= 1, Ydiff =:= 0 ; Xdiff =:= 0, Ydiff =:= 1).

/* WHAT ROOM IS THE AGENT IN */
here(Room) :-
           current_room(X,Y),
           Room = room(X,Y).

/* WHAT ARE ROOM COORDINATES */
get_coordinates(Room, X, Y) :-
           Room = room(X,Y).

/* ROOM DIRECTION */
direction(Room, Direction) :-     % returns north, south, east, or west
               current_room(X, Y),
               get_coordinates(Room, X1, Y1),
               (X = X1, Y < Y1, Direction = north;
                X = X1, Y > Y1, Direction = south;
                Y = Y1, X > X1, Direction = west;
                Y = Y1, X < X1, Direction = east).

/* SAFE, POTENTIAL & UNKNOWN ROOMS */

remember_safe :-
           current_room(X,Y),
           unknown(X,Y),                          % if room hasn't been visited
           safe(Rooms),                           % get the list of safe rooms
           retract(safe(Rooms)),
           assert(safe([room(X,Y) | Rooms])),     % assert current room is safe
           remove_potential_room(room(X,Y)),      % remove from list of potential rooms
           add_potential_rooms.                   % add unvisited new neighbors
           %format("Current room (~w, ~w) is safe. ~n", [X, Y]).

remove_potential_room(Room) :-           % remove a room from potential_rooms
       potential_rooms(List),
       retract(potential_rooms(List)),
       delete(List, Room, NewList),
       asserta(potential_rooms(NewList)).

add_potential_rooms :-
       neighbors(Neighbors),                              % get unvisited neighbors
       potential_rooms(Potentials),
       subtract(Potentials, Neighbors, List),             % remove existing neighbors
       append(Neighbors, List, NewPotentials),            % add all neighbors
       retract(potential_rooms(_)),
       assert(potential_rooms(NewPotentials)).            % update list of potential rooms


safe_room(X, Y) :-
           safe(Rooms),  % get the list of safe rooms
           member(room(X,Y), Rooms).

unknown(X, Y) :-         % return boolean TRUE if room is unknown
           safe(Rooms),  % get the list of safe rooms
           not(member(room(X,Y), Rooms)).

neighbors(Neighbors) :-    % return a list of unvisited neighbors of the current room
           current_room(X,Y),
           board_size(Max),
           findall(room(Xn,Yn), (
             ((Xn is X-1, Yn is Y);
              (Xn is X+1, Yn is Y);
              (Xn is X, Yn is Y-1);
              (Xn is X, Yn is Y+1)),
             between(1, Max, Xn), between(1, Max, Yn),
             \+safe_room(Xn,Yn)
           ), Neighbors).

old_neighbors(Room, Old_neighbors) :-    % return a list of visited neighbors of the room
           Room = room(Xi,Yi),
           board_size(Max),
           findall(room(Xj,Yj), (
             ((Xj is Xi-1, Yj is Yi);
              (Xj is Xi+1, Yj is Yi);
              (Xj is Xi, Yj is Yi-1);
              (Xj is Xi, Yj is Yi+1)),
             between(1, Max, Xj), between(1, Max, Yj),
             safe_room(Xj,Yj)
           ), Old_neighbors).

next_to(Room, Next_to) :-    % return a list of visited neighbors of the room
           Room = room(Xi,Yi),
           board_size(Max),
           findall(room(Xj,Yj), (
             ((Xj is Xi-1, Yj is Yi);
              (Xj is Xi+1, Yj is Yi);
              (Xj is Xi, Yj is Yi-1);
              (Xj is Xi, Yj is Yi+1)),
             between(1, Max, Xj), between(1, Max, Yj)
           ), Next_to).
