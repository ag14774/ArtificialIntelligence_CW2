
:- dynamic memory/2.

memory(o(_),p(_,_)).
memory(c(_),p(_,_)).

init_oscar_memory :-
  retractall(memory(_A,_B)).

remember(o(ID), p(_X,_Y)) :-
  memory(o(ID), p(_,_)),!.
remember(o(ID), p(X,Y)) :-
  ( agent_check_oracle(oscar,o(ID)) -> true
  ; otherwise -> assertz(memory(o(ID),p(X,Y)))
  ).

remember(c(ID), p(_X,_Y)) :-
  memory(c(ID), p(_,_)),!.
remember(c(ID), p(X,Y)) :-
  assertz(memory(c(ID),p(X,Y))).

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
  agent_current_position(oscar, CurPos),
  agent_current_energy(oscar, CurEnergy),
  ( get_good_station(CurPos,CurEnergy,o(O),false,Path,Pos,[cost(C),depth(D)]) ->
      NewEnergy is CurEnergy - C - 10,
      ( get_good_station(Pos,NewEnergy,c(B),PathToC,PosToC,[cost(ChargeC),depth(ChargeD)]) ->
          agent_do_moves(oscar, Path),
          agent_ask_oracle(oscar,o(O),link,L),
          findall(Actor, (member(Actor,[ActorA,ActorB|Rest]),wp(Actor,WT),wt_link(WT,L)), List2 ),
          sort(List2, List3)
      ; otherwise -> topup(oscar,CurPos,CurEnergy),
                     List3 = [ActorA,ActorB|Rest]
      )
  ; otherwise -> topup(oscar,CurPos,CurEnergy),
                 List3 = [ActorA,ActorB|Rest]
  ),
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

get_closest_oracle_from_mem(CurPos,f(O,P)) :-
  setof(f(Dist,o(A),Pos),(memory(o(A),Pos),
                          \+ agent_check_oracle(oscar,o(A)),
                          map_distance(CurPos,Pos,Dist)
                         ),
                        [f(_,BOID,BPos)|_]),
  O = BOID, P = BPos.

get_closest_station_from_mem(CurPos,f(C,P)) :-
  setof(f(Dist,c(A),Pos),(memory(c(A),Pos),map_distance(CurPos,Pos,Dist)),[f(_,BOID,BPos)|_]),
  C = BOID, P = BPos.

get_closest_from_mem(CurPos,OID,Pos) :-
  ( OID = o(A) -> get_closest_oracle_from_mem(CurPos,f(o(A),Pos))
  ; OID = c(A) -> get_closest_station_from_mem(CurPos,f(c(A),Pos))
  ).

get_closest_empty_adjacent(CurPos,Pos,New) :-
  setof(f(D,P),(map_adjacent(Pos,P,empty),map_distance(CurPos,P,D)),[f(_,New)|_]).

get_good_station(CurPos,CurEnergy,c(A),Path,Pos,Cost) :-
  get_closest_from_mem(CurPos,c(C1),P1),
  get_closest_empty_adjacent(CurPos,P1,NewP1),
  solve_task_3_path(CurPos,go(NewP1),Cost1,Path1,Pos1),
  get_closest_from_mem(CurPos,c(C2),P2), %%Anything better found?
  get_closest_empty_adjacent(CurPos,P2,NewP2),
  ( NewP1 == NewP2 -> Cost = Cost1,Path = Path1,Pos = Pos1,A = C1
  ; otherwise -> solve_task_3_path(CurPos,go(NewP2),Cost2,Path2,Pos2),
                 ( Cost2 @< Cost1 -> Cost = Cost2,Path = Path2,Pos = Pos2,A = C2
                 ; otherwise      -> Cost = Cost1,Path = Path1,Pos = Pos1,A = C1
                 )
  ),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy,!.
get_good_station(CurPos,CurEnergy,c(A),Path,Pos,Cost) :-
  solve_task_3_path(CurPos,find(c(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.

get_good_station(CurPos,CurEnergy,o(A),ForceSearch,Path,Pos,Cost) :-
  ForceSearch = false,
  get_closest_from_mem(CurPos,o(C1),P1),
  get_closest_empty_adjacent(CurPos,P1,NewP1),
  solve_task_3_path(CurPos,go(NewP1),Cost1,Path1,Pos1),
  get_closest_from_mem(CurPos,o(C2),P2), %%Anything better found?
  get_closest_empty_adjacent(CurPos,P2,NewP2),
  ( NewP1 == NewP2 -> Cost = Cost1,Path = Path1,Pos = Pos1,A = C1
  ; otherwise -> solve_task_3_path(CurPos,go(NewP2),Cost2,Path2,Pos2),
                 ( Cost2 @< Cost1 -> Cost = Cost2,Path = Path2,Pos = Pos2,A = C2
                 ; otherwise      -> Cost = Cost1,Path = Path1,Pos = Pos1,A = C1
                 )
  ),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy,!.
get_good_station(CurPos,CurEnergy,o(A),_ForceSearch,Path,Pos,Cost) :-
  solve_task_3_path(CurPos,find(o(A)),Cost,Path,Pos),
  Cost = [cost(C),depth(_D)],
  C < CurEnergy.

topup(Agent,CurPos,CurEnergy):- %check energy and if less than ~30 go to nearest charging station and recharge
  get_good_station(CurPos,CurEnergy,c(A),Path,_Pos,_), agent_do_moves(Agent,Path), agent_topup_energy(Agent,c(A)).
