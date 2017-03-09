% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  findall(Ac,actor(Ac),List),
  nextLink(List,A).

  nextLink([],_):-fail.

  nextLink([A|[]],A):-!.

  nextLink(List, A):-
  agent_ask_oracle(oscar,o(1),link,L),
  findall(Actor, (member(Actor, List),wp(Actor,WT),wt_link(WT,L) ), List2 ),
  sort(List2, List3),
  nextLink(List3, A).
