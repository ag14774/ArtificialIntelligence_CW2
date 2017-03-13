
:- dynamic memory/2.

memory(o(_),p(_,_)).
memory(c(_),p(_,_)).

init_oscar_memory :-
  retractall(memory(_A,_B)).

remember(o(ID), p(X,Y)) :-
  memory(o(ID), p(X,Y)),!.
remember(o(ID), p(X,Y)) :-
  assertz(memory(o(ID),p(X,Y))).

remember(c(ID), p(X,Y)) :-
  memory(c(ID), p(X,Y)),!.
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
  ( unifiable(Adj,AdjPos,[_|_]),unifiable(ID,OID,[_|_]) -> NewSolutions = [First|Solutions]
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
  agent_ask_oracle(oscar,o(1),link,L),
  findall(Actor, (member(Actor,[ActorA,ActorB|Rest]),wp(Actor,WT),wt_link(WT,L)), List2 ),
  sort(List2, List3),
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
