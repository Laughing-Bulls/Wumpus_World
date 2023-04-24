/* EXTRA, UNUSED CODE */

/* DANGER ROOM */
danger(X,Y) :- pit(X,Y); wumpus(X,Y).


/* INITIALIZE AGENT */
initialize_agent :-  % initialize to the value of the unvisited squares
       assert(remaining(0)),      % initialize # remaining rooms
       board_size(Max),
       Initial_unvisited is -Max * Max + 1,
       update_remaining(Initial_unvisited),
       format("Agent is initialized ~n").


/* UPDATE COST OF REMAINING PATH */
update_remaining(Change) :-
                    remaining(Current),
                    New is Current - Change,
                    retract(remaining(Current)),
                    assert(remaining(New)).
                    

/* TELEPORTATION ONLY USED IN DEBUGGING */
teleport_to_new(NewX, NewY) :-
           retract(current_room(_,_)),       % retract old position
           assert(current_room(NewX, NewY)),        % assert new position
           change_score(-10),
           format("New coordinates: (~w, ~w)~n", [NewX, NewY]),
           assess.

teleport(NewX, NewY) :-
               safe_room(NewX, NewY),
               retract(current_room(_,_)),       % retract old position
               assert(current_room(NewX, NewY)),        % assert new position
               change_score(-10),
               format("Teleported to new coordinates: (~w, ~w)~n", [NewX, NewY]).


/* CLOSEST ROOM */
distance(room(X1, Y1), room(X2, Y2), Distance) :-
    DistanceX is abs(X1 - X2),
    DistanceY is abs(Y1 - Y2),
    Distance is DistanceX + DistanceY.             % compute distnce between rooms

closest_room(Closest_room) :-
    here(Current_room),
    write("Current room: "), write(Current_room), nl,
    potential_rooms(Potential_rooms),
    write("Potential rooms: "), write(Potential_rooms), nl,
    maplist(distance(Current_room), Potential_rooms, Distances),
    write("Distances: "), write(Distances), nl,
    min_list(Distances, Min),
    write("Min: "), write(Min), nl,
    once(nth0(Index, Distances, Min)),         % index of the room with the smallest distance
    write("Index: "), write(Index), nl,
    nth0(Index, Potential_rooms, Closest_room),   % get the room at that index
    write("Closest room: "), write(Closest_room), nl.
