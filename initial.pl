/* LOAD FILES */
:- ['rooms.pl', 'agent.pl', 'movement.pl', 'actions.pl', 'risks.pl'].

:- dynamic room/2.
:- dynamic pit/2.
:- dynamic pits/1.          % list of pits
:- dynamic breeze/1, smell/1, glitter/2.
:- dynamic(current_room/2).
:- dynamic potential_rooms/1.
:- dynamic(score/1).
:- dynamic(arrow/0).
:- dynamic(wumpus_alive/0).
:- dynamic(gold_there/0).
:- dynamic safe/1.
:- dynamic remaining/1.

/* BOARD SIZE */
board_size(4).

/* SETUP ROOMS IN MAZE */
assert_rooms :-
    board_size(Max),
    findall(room(X, Y), (between(1, Max, X), between(1, Max, Y)), RoomList),
    assert_rooms(RoomList).

assert_rooms([]).
assert_rooms([Room|Rest]) :-
    assert(Room),
    assert_rooms(Rest).
    
/* INITIALIZE ROOM RISKS AT A LOW, NON-ZERO VALUE*/
init_room_risk :-            % room_risk(Room, Pit, Wumpus)
    forall(room(X, Y), assertz(room_risk(room(X, Y), 0.01, 0.01))).
    
    
/* WUMPUS POSITION */
wumpus_room :-
            board_size(Board_size),
            Parameter is Board_size + 1,
            repeat,
            random(1, Parameter, X),      % randomly assign wumpus room
            random(1, Parameter, Y),
            not(initial_room(X, Y)),      % not initial room (1, 1)
            !,
            assert(wumpus(X, Y)),
            Wumpus_room = room(X,Y),
            next_to(Wumpus_room, Next_to_wumpus),
            forall(member(Room, Next_to_wumpus), assertz(smell(Room))),
            format("Wumpus is in room: (~w, ~w)~n", [X, Y]).

/* GOLD POSITION */
gold_room :-
            board_size(Board_size),
            Parameter is Board_size + 1,
            repeat,
            random(1, Parameter, X),
            random(1, Parameter, Y),
            not(initial_room(X, Y)),     % randomly assign gold room that isn't (1, 1)
            not(wumpus(X, Y)),              % and isn't a wumpus room
            !,
            assert(gold(X, Y)),
            assert(glitter(X, Y)),
            format("Gold is in room: (~w, ~w)~n", [X, Y]).


/* PIT POSITIONS */
dig_pits :-
         assertz(pits([])),
         forall(
         (room(X, Y), \+ gold(X, Y), \+ wumpus(X, Y), \+ initial_room(X, Y)),
         (random(0, 100, N), N < 20 ->
                  assert(pit(X, Y)),
                  add_pit(X, Y),
                  format("A pit is in room: (~w, ~w)~n", [X, Y])
                  ; true)
         ).                         % 20% probability

add_pit(X,Y) :-
    Pit_room = room(X,Y),
    retract(pits(Pits)),
    assertz(pits([Pit_room|Pits])),
    next_to(Pit_room, Next_to_pit),
    forall(member(Room, Next_to_pit), assertz(breeze(Room))).


/* STARTING PROCEDURE */
start :-
      initialize_maze,
      start_game.
