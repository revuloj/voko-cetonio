% phrase(amb2,L,[Len]).

amb(L1,L2) --> lin, [L1], lin1, [L2].
amb2 --> lin, lin1.

% phrase(amb3,[0|L],[Len]).

amb3 --> lin2, lin1.

% phrase(amb4(Y),[0|L],[]).
% phrase(amb4(Y),[0|L]).

amb4(Len) --> lin2, lin1, [Len].


% phrase(amb5,[0|L],[Len]).

amb5, [Len] --> lin2, lin1, [Len].


lin, [L1] --> "x y z", {L1 = 5}.

lin1, [L2] --> [L1], "abc", {L2 is L1 + 3}.
%does not work: lin1, [L2] -->  [L1], "abc", len(L), {L2 is L1 + L}.
lin2, [L1] --> [L0], "x y z", {L1 is L0 + 5}.

len(Len,List,Rest) :-
  append(Diff,Rest,List),
  length(Diff,Len).


foo(Bar) -->  
      "mep",
      Bar,
      lemons.

lemons --> "lemons".

blk(Tag,At) --> [element(Tag,At)].

art --> [swi].
hom --> [pl].

det(_) --> [the].

