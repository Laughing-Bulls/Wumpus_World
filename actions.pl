/* SHOOT ARROW */
shoot_arrow(Direction) :-
            arrow,            % if agent still has the arrow
            (aim(Direction); true),
            use_arrow,        % update arrow status to FALSE
            change_score(-10).

aim(Aim_Direction) :-
               current_room(X, Y),
               wumpus(X_wumpus, Y_wumpus),    % aim at wumpus
               (X = X_wumpus, Y < Y_wumpus, Wumpus_direction = north;
                X = X_wumpus, Y > Y_wumpus, Wumpus_direction = south;
                Y = Y_wumpus, X > X_wumpus, Wumpus_direction = west;
                Y = Y_wumpus, X < X_wumpus, Wumpus_direction = east),
               Aim_Direction == Wumpus_direction,
               format("You shot ~w and the wumpus was ~w! ~n", [Aim_Direction, Wumpus_direction]),
               kill_wumpus,
               assertz(room_risk(room(X_wumpus, Y_wumpus), 0, 0)). % no pit in wumpus room

/* Change to False when arrow used */
use_arrow :-
    retract(arrow),
    assert(arrow(false)).

kill_wumpus :-
        retract(wumpus_alive),
        assert(wumpus_alive(false)),
        format("You hear an ear-shattering wumpus scream! ~n").
        
/* GET THE GOLD AFTER GLITTER HAS BEEN DETECTED */
get_gold :-
     current_room(X,Y),
     glitter(X,Y),                           % check room for gold
     format("You got the gold in room (~w, ~w)! ~n", [X, Y]),
     (gold_there -> change_score(1000) ; true), % points for getting the gold if there
     retract(gold_there),
     assert(gold_there(false)),
     initial_room(Xf, Yf),
     travel_to(room(Xf,Yf)),   % return to start
     win_game.                % WIN
     
/* CHANGE THE SCORE */
change_score(Change) :-
                     retract(score(S)),
                     S1 is S + Change,
                     assert(score(S1)),
                     format("Score: ~w. ~n", [S1]).
                     
/* END OF GAME ACTIONS */
die :-
     current_room(X,Y),
     (die_by_wumpus(X,Y); die_by_pit(X,Y)),       % check room for pit or wumpus
     format("You died in room (~w, ~w)! ~n", [X, Y]),
     change_score(-1000),
     halt(0).       % End game

die_by_wumpus(X, Y) :-
     wumpus(X, Y),
     wumpus_alive,
     format("The wumpus ate you! ~n").

die_by_pit(X, Y) :-
     pit(X, Y),
     format("You fell into a bottomless pit! ~n").

win_game :-
    format("You won the Wumpus Game! ~n"),
    score(Score),
    format("FINAL SCORE: ~w. ~n", [Score]),
    halt(0).       % End game

