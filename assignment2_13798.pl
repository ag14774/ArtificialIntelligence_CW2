candidate_number(13798).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1(Task, Cost)
  ; part(3) -> solve_task_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

solve_task_path(CurPos,Task,Cost,Path,NewPos) :-
  solve_task_path(CurPos,Task,Cost,Path,NewPos,true).
solve_task_1(Task,Cost) :-
  solve_task(Task,Cost,false).
solve_task_3(Task,Cost) :-
  solve_task(Task,Cost,true).
solve_task_4(Task,Cost) :-
  solve_task(Task,Cost,true).

solve_task(Task,Cost,LookAround) :-
  current_position(P),
  solve_task_path(P,Task,Cost,Path,_NewPos,LookAround),
  do_moves(Path, _).

solve_task_path(CurPos,Task,Cost,Path,NewPos,LookAround) :-
  eval(Task,CurPos,C),
  insert_to_dict(visited{},CurPos,C,VisitedInit),
  % c(total, heuristic, depth, pos)
  solve_task_bf(Task,[[c(C,C,0,CurPos),CurPos]],VisitedInit,R,Cost,NewPos,LookAround),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]).


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% A* search %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_bf(Task,[Current|_Rest],_Visited,RPath,[cost(Cost),depth(Depth)],NewPos,Remember) :-
  achieved(Task,Current,RPath,Cost,Depth,NewPos,Remember).
solve_task_bf(Task,[Current|Rest],Visited,RR,Cost,NewPos,Remember) :-
  children(Task,Current,Children,Remember),
  filter_children(Children,Visited,NewVisited,NewChildren),
  merge(NewChildren,Rest,NewAgenda),
  solve_task_bf(Task,NewAgenda,NewVisited,RR,Cost,NewPos,Remember).  % backtrack search

children(Task,[c(_OldCost,_,Depth,P)|RPath],Children,Remember) :-
  DepthNew is Depth + 1,
  setof0( [c(Cost,H,DepthNew,Next),Next|RPath],
          (search(P,Next,Remember),
           \+ memberchk(Next,RPath),
           eval(Task,Next,H),
           Cost is H + DepthNew ),
          Children ).

%% Heuristic %%
eval(find(_O),_Pos,Cost) :-
  Cost = 0.

eval(go(P),Pos,Cost) :-
  map_distance(P,Pos,Cost).

eval(goAdj(P),Pos,Cost) :-
  get_closest_empty_adjacent(Pos,P,New),
  map_distance(New,Pos,Cost).

get_closest_empty_adjacent(CurPos,Pos,New) :-
  setof(f(D,P),(map_adjacent(Pos,P,empty),map_distance(CurPos,P,D)),[f(_,New)|_]).
%%%%%%%%%%%%%%%

search(F,N,false) :-
  map_adjacent(F,N,empty).
search(F,N,true) :-
  look_around(F,N,empty).

insert_to_dict(Dict,Point,Value,NewDict) :-
  term_to_atom(Point,Key),
  put_dict(Key,Dict,Value,NewDict).

get_from_dict(Dict,Point,1,Value) :-
  term_to_atom(Point,Key),
  get_dict(Key,Dict,Value),!.
get_from_dict(_Dict,_Point,0,_Value).

filter_children(Children,Dict,NewDict,NewChildren) :-
  filter_children(Children,[],Dict,NewDict,Children2),
  reverse(Children2,NewChildren).
filter_children([],Children,Dict,Dict,Children).
filter_children([Child|Rest],Children,Dict,NewDict,NewChildren) :-
  Child = [c(Cost,_,_,Pos)|_],
  get_from_dict(Dict,Pos,Found,Value),
  ( Found = 1, Cost <  Value -> insert_to_dict(Dict,Pos,Cost,Dict2),
                                Child2 = [Child|Children]
  ; Found = 1, Cost >= Value -> Dict2 = Dict,
                                Child2 = Children
  ; otherwise -> insert_to_dict(Dict,Pos,Cost,Dict2),
                 Child2 = [Child|Children]
  ),
  filter_children(Rest,Child2,Dict2,NewDict,NewChildren).


achieved(go(Exit),Current,RPath,Cost,Depth,NewPos,_) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(goAdj(Exit),Current,RPath,Cost,Depth,NewPos,true) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Last|_],look_around(Exit,Last,empty)
  ).
achieved(goAdj(Exit),Current,RPath,Cost,Depth,NewPos,false) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Exit,Last,empty)
  ).
achieved(find(O),Current,RPath,Cost,Depth,NewPos,false) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

achieved(find(o(O)),Current,RPath,Cost,Depth,NewPos,true) :- !,
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  RPath = [Last|_],
  look_around(Last,_,o(O)),
  \+ check_oracle(o(O)).
achieved(find(O),Current,RPath,Cost,Depth,NewPos,true) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],look_around(Last,_,O)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge([], Agenda, Agenda).
merge(Children, [], Children).
merge([Child|Siblings],[First|Rest],[Child|NewAgenda]) :-
  Child@<First,
  merge(Siblings, [First|Rest], NewAgenda).
merge([Child|Siblings],[First|Rest],[First|NewAgenda]) :-
  Child@>=First,
  merge([Child|Siblings], Rest, NewAgenda).

setof0(X,G,L) :-
  setof(X,G,L),!.
setof0(_X,_G,[]).

check_oracle(o(A)) :-
  ( part(4) -> my_agent(Agent),query_world(agent_check_oracle,[Agent,o(A)])
  ; otherwise -> agent_check_oracle(oscar,o(A))
  ).

current_position(P) :-
  ( part(4) -> my_agent(Agent),query_world(agent_current_position,[Agent,P])
  ; otherwise -> agent_current_position(oscar,P)
  ).

do_moves(Path, Succ) :-
  ( part(4) -> my_agent(Agent),
               ( query_world(agent_do_moves,[Agent,Path]) -> Succ=true
               ; otherwise -> Succ=false
               )
  ; otherwise -> agent_do_moves(oscar,Path), Succ=true
  ).

%failure driven loop
do_moves_recharging(Path, Succ) :-
  member(Point,Path),
  do_moves([Point],Succ1),
  ( Succ1 == true -> ( map_adjacent(Point, _, c(ID)) -> topup_energy(c(ID),_)
                     ; otherwise -> true
                     ),
                     fail
  ; otherwise -> Succ = Succ1,!
  ).
do_moves_recharging(_, Succ) :- Succ = true.

ask_oracle(OID,A,B,Succ) :-
  ( part(4) -> my_agent(Agent),
               ( query_world(agent_ask_oracle,[Agent,OID,A,B]) -> Succ=true
               ; otherwise -> Succ=false
               )
  ; otherwise -> agent_ask_oracle(oscar,OID,A,B), Succ=true
  ).

current_energy(CurEnergy) :-
  ( part(4) -> my_agent(Agent),query_world(agent_current_energy,[Agent,CurEnergy])
  ; otherwise -> agent_current_energy(oscar,CurEnergy)
  ).

topup_energy(c(C),Succ) :-
  ( part(4) -> my_agent(Agent),
               ( query_world(agent_topup_energy,[Agent,c(C)]) -> Succ=true
               ; otherwise -> Succ=false
               )
  ; otherwise -> agent_topup_energy(oscar,c(C)), Succ=true
  ).
