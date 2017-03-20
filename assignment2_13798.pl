candidate_number(13798).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1(Task, Cost)
  ; part(3) -> solve_task_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1(Task,Cost) :-
  agent_current_position(oscar,P),
  eval(Task,P,C),
  insert_to_dict(visited{},P,C,VisitedInit),
  % c(total, heuristic, depth, pos)
  solve_task_bf(Task,[[c(C,C,0,P),P]],VisitedInit,R,Cost,_NewPos,false),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path).
%%%%%%%%%% Part 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_3(Task,Cost) :-
  agent_current_position(oscar,P),
  eval(Task,P,C),
  insert_to_dict(visited{},P,C,VisitedInit),
  % c(total, heuristic, depth, pos)
  solve_task_bf(Task,[[c(C,C,0,P),P]],VisitedInit,R,Cost,_NewPos,true),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path).

solve_task_3_path(CurPos,Task,Cost,Path,NewPos) :-
  eval(Task,CurPos,C),
  insert_to_dict(visited{},CurPos,C,VisitedInit),
  % c(total, heuristic, depth, pos)
  solve_task_bf(Task,[[c(C,C,0,CurPos),CurPos]],VisitedInit,R,Cost,NewPos,true),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

%%%%%%%%% Part 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
achieved(find(O),Current,RPath,Cost,Depth,NewPos,false) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).
achieved(find(o(O)),Current,RPath,Cost,Depth,NewPos,true) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  RPath = [Last|_],
  look_around(Last,_,o(O)),
  agent_check_oracle(oscar,o(O)),!.
achieved(find(O),Current,RPath,Cost,Depth,NewPos,true) :-
  Current = [c(Cost,_,Depth,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],look_around(Last,_,O)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

achieved(go(Exit),Current,RPath,Cost,NewPos,_) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos,false) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).
achieved(find(O),Current,RPath,Cost,NewPos,true) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],look_around(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).

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
