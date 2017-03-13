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
