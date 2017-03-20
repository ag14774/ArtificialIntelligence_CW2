
:- dynamic memory/2.
:- dynamic discarded/2.

memory(o(_),p(_,_)).
memory(c(_),p(_,_)).
discarded(o(_),p(_,_)).
discarded(c(_),p(_,_)).

init_oscar_memory :-
  retractall(memory(_A,_B)),
  retractall(discarded(_C,_D)).

remember(o(ID), p(_X,_Y)) :-
  memory(o(ID), p(_,_)),!.
remember(o(ID), p(X,Y)) :-
  \+ agent_check_oracle(oscar,o(ID)),
  \+ discarded(o(ID), p(X,Y)),
  assertz(memory(o(ID),p(X,Y))),!.
remember(o(_ID), p(_X,_Y)).

remember(c(ID), p(_X,_Y)) :-
  memory(c(ID), p(_,_)),!.
remember(c(ID), p(X,Y)) :-
  \+ discarded(c(ID), p(X,Y)),
  assertz(memory(c(ID),p(X,Y))),!.
remember(c(_ID), p(_X,_Y)).

forget(o(ID), p(X,Y)) :-
  memory(o(ID),p(X,Y)),
  retract(memory(o(ID),p(X,Y))),
  assertz(discarded(o(ID),p(X,Y))),!.
forget(o(_ID), p(_X,_Y)).

forget(c(ID), p(X,Y)) :-
  memory(c(ID),p(X,Y)),
  retract(memory(c(ID),p(X,Y))),
  assertz(discarded(c(ID),p(X,Y))),!.
forget(c(_ID), p(_X,_Y)).

look_around(Pos, AdjPos, OID) :-
  findall(f(ID,Adj),map_adjacent(Pos,Adj,ID),Adjacent),
  look_around(Adjacent,AdjPos,OID,[],Solutions),
  look_around_get_solution(Solutions,AdjPos,OID).

look_around([],_AdjPos,_OID,Solutions,Solutions).
look_around([First|Rest],AdjPos,OID,Solutions,Result) :-
  First = f(ID,Adj),
  ( remember(ID,Adj) -> true
  ; otherwise -> true
  ),
  ( unifiable(Adj,AdjPos,_),unifiable(ID,OID,_) -> NewSolutions = [First|Solutions]
  ; otherwise -> NewSolutions = Solutions
  ),
  look_around(Rest,AdjPos,OID,NewSolutions,Result).

look_around_get_solution([Solution|_Rest],AdjPos,OID) :-
  Solution = f(OID,AdjPos).
look_around_get_solution([_Solution|Rest],AdjPos,OID) :-
  look_around_get_solution(Rest,AdjPos,OID).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A) :-
  findall(Ac,actor(Ac),List),
  nextLink(List,A).

%nextLink([],_):-fail.
nextLink([A],A).
nextLink([ActorA,ActorB|Rest], A) :-
  %solve_task_3(find(o(O),Cost),
  go_to_next_oracle(oscar,OID),
  agent_ask_oracle(oscar,OID,link,L),
  findall(Actor, (member(Actor,[ActorA,ActorB|Rest]),wp(Actor,WT),wt_link(WT,L)), List2 ),
  sort(List2, List3),
  forget(OID,_),
  nextLink(List3, A).

%%%% IDEAS %%%%
%% -Internal memory can be a list of predicates
%% -We can add and remove items using assert and retract
%% -Instead of using map_adjacent create look_around.
%%  same as map_adjacent but first stores adjacent items in memory
%% Useful predicates:
%% -remember() to insert to memory
%% -forget() to remove from memory
%% -get_closest
%% -get_sorted
%% -look_around

get_closest_oracle_from_mem(CurPos,f(Oracle,OraclePos,NewPos,Path,Cost)) :-
  setof(f(TCost,o(O),OPos,NPos,TPath),
        (memory(o(O),OPos),
         \+ agent_check_oracle(oscar,o(O)),
         solve_task_3_path(CurPos,goAdj(OPos),TCost,TPath,NPos)),
        [f(BCost,BOID,BOPos,BNPos,BTPath)|_]),
  Oracle = BOID,
  OraclePos = BOPos,
  NewPos = BNPos,
  Path = BTPath,
  Cost = BCost.

get_closest_station_from_mem(CurPos,f(Station,StationPos,NewPos,Path,Cost)) :-
  setof(f(TCost,c(C),CPos,NPos,TPath),
        (memory(c(C),CPos),
         solve_task_3_path(CurPos,goAdj(CPos),TCost,TPath,NPos)),
        [f(BCost,BCID,BCPos,BNPos,BTPath)|_]),
  Station = BCID,
  StationPos = BCPos,
  NewPos = BNPos,
  Path = BTPath,
  Cost = BCost.

get_closest_from_mem(CurPos,OID,Pos,FinalPos,Path,Cost) :-
  ( OID = o(A) -> get_closest_oracle_from_mem(CurPos,f(o(A),Pos,FinalPos,Path,Cost))
  ; OID = c(A) -> get_closest_station_from_mem(CurPos,f(c(A),Pos,FinalPos,Path,Cost))
  ).

get_good_object(CurPos,CurEnergy,c(A),Path,Pos,Cost) :-
  get_closest_from_mem(CurPos,c(A),_,Pos,Path,Cost),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy,!.
get_good_object(CurPos,CurEnergy,c(A),Path,Pos,Cost) :-
  solve_task_3_path(CurPos,find(c(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.

get_good_object(CurPos,CurEnergy,o(A),Path,Pos,Cost,ForceBreadthFirst) :-
  ForceBreadthFirst = false,
  get_closest_from_mem(CurPos,o(A),_,Pos,Path,Cost),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy,!.
get_good_object(CurPos,CurEnergy,o(A),Path,Pos,Cost,_ForceBreadthFirst) :-
  solve_task_3_path(CurPos,find(o(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.

%%Go to oracle with station within reach.(different name to avoid infinite recursion)
go_to_next_oracle2(Agent,o(O)) :-
  agent_current_position(Agent, CurPos),
  agent_current_energy(Agent, CurEnergy),
  get_good_object(CurPos,CurEnergy,o(O),OPath,OPosAdj,OCost,false),
  OCost = [cost(OC),depth(_OD)],
  PredictedEnergy is CurEnergy - OC - 10,
  get_good_object(OPosAdj,PredictedEnergy,c(_C),_,_,_),
  agent_do_moves(Agent,OPath).

%%Go to oracle with station within reach.
go_to_next_oracle(Agent,OID) :-
  go_to_next_oracle2(Agent,OID),!.
%%Topup and try again
go_to_next_oracle(Agent,OID) :-
  topup(Agent,_,_),
  go_to_next_oracle2(Agent,OID),!.
%%Go to closest oracle. Do not check for station
%%If we are running low on energy and we can't find
%%an oracle with station nearby and we can't directly go
%%to the station, then use this last move to ask an oracle
go_to_next_oracle(Agent,o(O)) :-
  agent_current_position(Agent, CurPos),
  agent_current_energy(Agent, CurEnergy),
  get_good_object(CurPos,CurEnergy,o(O),OPath,_OPosAdj,_OCost,true), %%last chance - force breadth first search
  agent_do_moves(Agent,OPath).

topup(Agent,CurPos,CurEnergy):- %check energy and if less than ~30 go to nearest charging station and recharge
  agent_current_position(Agent, CurPos),
  agent_current_energy(Agent, CurEnergy),
  get_good_object(CurPos,CurEnergy,c(C),Path,_Pos,_Cost),
  agent_do_moves(Agent,Path),
  agent_topup_energy(Agent,c(C)).
