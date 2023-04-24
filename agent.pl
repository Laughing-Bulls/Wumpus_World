/* INITIALIZE MAZE */
initialize_maze :-
      assert_rooms,
      assert(safe([])),          % initialize safe room list
      assert(score(0)),
      assert(arrow),             % the agent starts with ONE arrow
      wumpus_room,               % randomly generate wumpus room
      assert(wumpus_alive),      % the wumpus starts the game alive
      assert(gold_there),        % the gold is there
      gold_room,                 % randomly generate gold room
      dig_pits,
      init_room_risk,
      format("Maze is initialized ~n").

/* START GAME */
start_game :-
      initial_room(X, Y),
      assert(current_room(X, Y)),            % put Agent in starting room
      neighbors(Neighbors),                  % get adjacent rooms
      assertz(potential_rooms(Neighbors)),   % initialize potential rooms
      format("Starting game. ~nInitial agent coordinates: (~w, ~w) ~n", [X, Y]),
      assess.                                % assess the initial room
      
/* INITIAL ROOM */
initial_room(1, 1).


/* AGENT SEARCH */
agent :-                                  % utilize a breadth-first search
      current_room(X,Y),                  % get coordinates
      glitter(X,Y) -> get_gold.           % Win and end game

agent :-
      potential_rooms(Potential_rooms),                 % get list of all potential rooms
      safest_rooms(Potential_rooms, Safest_rooms),    % get lowest risk squares
      [Next_room|_] = Safest_rooms,
      %format("Planning move to: ~w ~n", [Next_room]),
      travel_to(Next_room),          % go to an unvisited potential room
      agent.                         % BFS recursive call to agent
      

/* RISK ASSESSMENT */
% Choose potential rooms with the lowest combined PitRisk and WumpusRisk
safest_rooms(Potential_rooms, Safest_rooms) :-      % return lowest risk ratings
      % Calculate the combined risks for each potential room
      maplist(compute_combined_risk, Potential_rooms, Combined_risks),
      min_list(Combined_risks, Min_combined_risk),    % find the minimum combined risk
      findall(Room,
             (nth0(Index, Potential_rooms, Room),
              nth0(Index, Combined_risks, Min_combined_risk)),
              Safest_rooms).
              
/* AGENT DECIDES TO SHOOT ARROW */
% Check if the WumpusRisk of the current room is greater than or equal to 1 and shoot the arrow
shoot(Room) :-
      wumpus_alive,
      room_risk(Room, _, WumpusRisk),      % check WumpusRisk at 1.0 or higher
      WumpusRisk >= 1,
      format("I smell a wumpus in room ~w and I'm gonna shoot! ~n", [Room]),
      direction(Room, Direction),
      shoot_arrow(Direction).
