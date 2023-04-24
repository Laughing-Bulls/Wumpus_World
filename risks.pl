/* ASSESS ROOM */
assess :-
          current_room(X, Y),
          here(Current_room),
          (not(die) -> (remember_safe ; true) ; true),
          (breeze(Current_room) -> breezy ; zero_pit_risk),
          (smell(Current_room) -> smelly ; zero_wumpus_risk),
          (glitter(X, Y) -> format("Glitter! ~n") ; true).


/* IF A BREEZE OR SMELL */
breezy :-                     % room has breeze from a pit
       increment_pit_risk,    % increase risks of adjacent pits
       format("You feel a breeze! ~n").

smelly :-                              % room has smell of a wumpus
       wumpus_alive,                   % only smelly if the wumpus is alive
       format("You detect a smell! ~n"),
       increment_wumpus_risk.          % increase risks of adjacent wumpus


/* ASCRIBE ZERO RISKS IF NO BREEZE OR SMELL */
% Set the PitRisk of all neighbors of Room to 0.
zero_pit_risk :-
    neighbors(Neighbors),
    maplist(zero_pit_risk_helper, Neighbors).

% Helper predicate to set the PitRisk of a room to 0.
zero_pit_risk_helper(Room) :-
        retract(room_risk(Room, _, WumpusRisk)),
        assertz(room_risk(Room, 0, WumpusRisk)).

% Set the WumpusRisk of all neighbors of Room to 0.
zero_wumpus_risk :-
    neighbors(Neighbors),
    maplist(zero_wumpus_risk_helper, Neighbors).

% Helper predicate to set the WumpusRisk of a room to 0.
zero_wumpus_risk_helper(Room) :-
        retract(room_risk(Room, PitRisk, _)),
        assertz(room_risk(Room, PitRisk, 0)).


/* ASCRIBE RISK VALUES IF BREEZE OR SMELL */
% Increment the PitRisk of neighbors with zero PitRisk based on the number of neighbors with non-zero PitRisk.
increment_pit_risk :-
    neighbors(Neighbors),
    include(nonzero_pit_risk, Neighbors, NonZeros),
    length(NonZeros, Count),
    Count > 0,
    IncrementRisk is 1 / Count,
    maplist(increment_pit_risk_helper(IncrementRisk), NonZeros).

% Helper predicate to increment the PitRisk of a room.
increment_pit_risk_helper(IncrementRisk, Room) :-
    retract(room_risk(Room, OldRisk, WumpusRisk)),
    NewRisk is OldRisk + IncrementRisk,
    assertz(room_risk(Room, NewRisk, WumpusRisk)).

% Check if a neighboring room has non-zero PitRisk.
nonzero_pit_risk(Room) :-
    room_risk(Room, PitRisk, _),
    PitRisk > 0.

% Increment the WumpusRisk of neighbors with zero WumpusRisk based on the number of neighbors with non-zero WumpusRisk.
increment_wumpus_risk :-
    neighbors(Neighbors),
    include(nonzero_wumpus_risk, Neighbors, NonZeros),
    length(NonZeros, Count),
    Count > 0,
    IncrementRisk is 1 / Count,
    maplist(increment_wumpus_risk_helper(IncrementRisk), NonZeros).

% Helper predicate to increment the WumpusRisk of a room.
increment_wumpus_risk_helper(IncrementRisk, Room) :-
    retract(room_risk(Room, PitRisk, OldRisk)),
    NewRisk is OldRisk + IncrementRisk,
    assertz(room_risk(Room, PitRisk, NewRisk)),
    (shoot(Room) ; true).               % shoot at Wumpus, if probability is > 1

% Check if a neighboring room has non-zero PitRisk.
nonzero_wumpus_risk(Room) :-
    room_risk(Room, _, WumpusRisk),
    WumpusRisk > 0.


/* RISK ASSESSMENT */
% Compute the combined risk values for a room.
compute_combined_risk(Room, Combined_risk_value) :-
    room_risk(Room, PitRisk, WumpusRisk),
    (wumpus_alive ->
        Combined_risk_value is PitRisk + WumpusRisk
    ;
        Combined_risk_value is PitRisk
    ).

