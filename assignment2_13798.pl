candidate_number(12345).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1_3(Task, Cost)
  ; part(3) -> solve_task_1_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task,Cost) :-
  agent_current_position(oscar,P),
  eval(Task,P,C),
  solve_task_bf(Task,[[c(C,0,P),P]],R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path).
%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
solve_task_bf(Task,[Current|Rest],RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,Depth,NewPos).
solve_task_bf(Task,[Current|Rest],RR,Cost,NewPos) :-
  children(Task,Current,Children),
  merge(Children,Rest,NewAgenda),
  solve_task_bf(Task,NewAgenda,RR,Cost,NewPos).  % backtrack search

children(Task,[c(OldCost,Depth,P)|RPath],Children) :-
  DepthNew is Depth + 1,
  setof0( [c(Cost,DepthNew,Next),Next|RPath],
          (search(P,Next),
           \+ memberchk(Next,RPath),
           eval(Task,Next,H),
           Cost is H + DepthNew ),
          Children ).

%% Heuristic %%
eval(find(O),Pos,Cost) :-
  Cost = 0.

eval(go(P),Pos,Cost) :-
  map_distance(P,Pos,Cost).
%%%%%%%%%%%%%%%

search(F,N) :-
  map_adjacent(F,N,empty).

achieved(go(Exit),Current,RPath,Cost,Depth,NewPos) :-
  Current = [c(Cost,Depth,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,Depth,NewPos) :-
  Current = [c(Cost,Depth,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
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
