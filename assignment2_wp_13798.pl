
:- dynamic memory/2.
:- dynamic discarded/2.

memory(o(_),p(_,_)).
memory(c(_),p(_,_)).
discarded(o(_),p(_,_)).
discarded(c(_),p(_,_)).

init_memory :-
  retractall(memory(_A,_B)),
  retractall(discarded(_C,_D)).

remember(o(ID), p(X,Y)) :-
  memory(o(ID), p(X,Y)),!.
remember(o(ID), p(X,Y)) :-
  ( memory(o(ID),P) -> retract( memory(o(ID),P) ),
    otherwise -> true
  ),
  \+ check_oracle(o(ID)),
  \+ discarded(o(ID), _),
  assertz(memory(o(ID),p(X,Y))),!.
remember(o(_ID), p(_X,_Y)) :- !.

remember(c(ID), p(X,Y)) :-
  memory(c(ID), p(X,Y)),!.
remember(c(ID), p(X,Y)) :-
  ( memory(c(ID),P) -> retract( memory(c(ID),P) ),
    otherwise -> true
  ),
  \+ discarded(c(ID), _),
  assertz(memory(c(ID),p(X,Y))),!.
remember(c(_ID), p(_X,_Y)) :- !.

%%Always succeed.
remember(_A,_B).

forget(o(ID)) :-
  memory(o(ID),P),
  retract( memory(o(ID),P) ),
  assertz(discarded(o(ID),P)),!.
forget(o(_ID)).

forget(c(ID)) :-
  memory(c(ID),P),
  retract( memory(c(ID),P) ),
  assertz( discarded(c(ID),P) ),!.
forget(c(_ID)).

look_around(Pos, AdjPos, OID) :-
  findall(f(ID,Adj),map_adjacent(Pos,Adj,ID),Adjacent),
  look_around(Adjacent,AdjPos,OID,[],Solutions),
  look_around_get_solution(Solutions,AdjPos,OID).

look_around([],_AdjPos,_OID,Solutions,Solutions).
look_around([First|Rest],AdjPos,OID,Solutions,Result) :-
  First = f(ID,Adj),
  remember(ID,Adj),
  ( unifiable(Adj,AdjPos,_),unifiable(ID,OID,_) -> NewSolutions = [First|Solutions]
  ; otherwise -> NewSolutions = Solutions
  ),
  look_around(Rest,AdjPos,OID,NewSolutions,Result).

look_around_get_solution([Solution|_Rest],AdjPos,OID) :-
  Solution = f(OID,AdjPos).
look_around_get_solution([_Solution|Rest],AdjPos,OID) :-
  look_around_get_solution(Rest,AdjPos,OID).

% Find hidden identity by repeatedly calling ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A) :-
  findall(Ac,actor(Ac),List),
  ( part(2) -> nextLink_part2(List,A)
  ; otherwise -> nextLink(List,A)
  ).

nextLink_part2([A],A).
nextLink_part2([ActorA,ActorB|Rest], A) :-
  ask_oracle(o(1),link,L,_),
  findall(Actor, (member(Actor,[ActorA,ActorB|Rest]),wp(Actor,WT),wt_link(WT,L)), List2 ),
  sort(List2, List3),
  nextLink_part2(List3, A).

nextLink([A],A).
nextLink([ActorA,ActorB|Rest], A) :-
  %solve_task_3(find(o(O),Cost),
  go_to_next_oracle(OID),
  ask_oracle(OID,link,L),
  findall(Actor, (member(Actor,[ActorA,ActorB|Rest]),wp(Actor,WT),wt_link(WT,L)), List2 ),
  sort(List2, List3),
  forget(OID),
  nextLink(List3, A).

get_closest_oracle_from_mem(CurPos,f(Oracle,OraclePos,NewPos,Path,Cost)) :-
  setof(f(TCost,o(O),OPos,NPos,TPath),
        (memory(o(O),OPos),
         \+ check_oracle(o(O)),
         solve_task_path(CurPos,goAdj(OPos),TCost,TPath,NPos)),
        [f(BCost,BOID,BOPos,BNPos,BTPath)|_]),
  Oracle = BOID,
  OraclePos = BOPos,
  NewPos = BNPos,
  Path = BTPath,
  Cost = BCost.

get_closest_station_from_mem(CurPos,f(Station,StationPos,NewPos,Path,Cost)) :-
  setof(f(TCost,c(C),CPos,NPos,TPath),
        (memory(c(C),CPos),
         solve_task_path(CurPos,goAdj(CPos),TCost,TPath,NPos)),
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
  solve_task_path(CurPos,find(c(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.

get_good_object(CurPos,CurEnergy,o(A),Path,Pos,Cost,ForceBreadthFirst) :-
  ForceBreadthFirst = false,
  get_closest_from_mem(CurPos,o(A),_,Pos,Path,Cost),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy,!.
get_good_object(CurPos,CurEnergy,o(A),Path,Pos,Cost,_ForceBreadthFirst) :-
  solve_task_path(CurPos,find(o(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.


go_to_next_oracle(OID) :-
  go_to_next_oracle(OID2,Succ),
  ( Succ == false -> writeln("Trying again.."),go_to_next_oracle(OID)
  ; otherwise ->    OID = OID2
  ).

ask_oracle(OID,A,B) :-
  ask_oracle(OID,A,B2,Succ),
  ( Succ == false -> write("Oracle trying again"),go_to_next_oracle(OID), ask_oracle(OID,A,B)
  ; otherwise -> B = B2
  ).

%%Go to oracle with station within reach.(different name to avoid infinite recursion)
go_to_next_oracle2(o(O), Succ) :-
  current_position(CurPos),
  current_energy(CurEnergy),
  get_good_object(CurPos,CurEnergy,o(O),OPath,OPosAdj,OCost,false),
  OCost = [cost(OC),depth(_OD)],
  PredictedEnergy is CurEnergy - OC - 10,
  get_good_object(OPosAdj,PredictedEnergy,c(_C),_,_,_),
  do_moves_recharging(OPath, Succ).

%%Go to oracle with station within reach.
go_to_next_oracle(OID, Succ) :-
  go_to_next_oracle2(OID2, Succ),!,
  ( Succ == true -> OID = OID2
  ; otherwise -> true
  ).
%%Topup and try again
go_to_next_oracle(OID, Succ) :-
  topup(Succ1),
  ( Succ1 == true -> go_to_next_oracle2(OID2, Succ),!,
                     ( Succ == true -> OID = OID2
                     ; otherwise -> true
                     )
  ; otherwise -> Succ = Succ1
  ).
%%Go to closest oracle. Do not check for station
%%If we are running low on energy and we can't find
%%an oracle with station nearby and we can't directly go
%%to the station, then use this last move to ask an oracle
go_to_next_oracle(o(O), Succ) :-
  current_position(CurPos),
  current_energy(CurEnergy),
  get_good_object(CurPos,CurEnergy,o(O),OPath,_OPosAdj,_OCost,true), %%last chance - force breadth first search
  do_moves_recharging(OPath, Succ).

topup(Succ):- %check energy and if less than ~30 go to nearest charging station and recharge
  current_position(CurPos),
  current_energy(CurEnergy),
  get_good_object(CurPos,CurEnergy,c(C),Path,_Pos,_Cost),
  do_moves_recharging(Path,Succ1),
  ( Succ1 == true -> topup_energy(c(C),Succ)
  ; otherwise -> write("Topup trying again"),Succ=Succ1
  ).
