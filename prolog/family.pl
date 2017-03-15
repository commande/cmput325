grandparent(P1, P3) :- parent(P1, P2), parent(P2, P3).
parent(P1, P2) :- mother(P1, P2).
parent(P1, P2) :- father(P1 ,P2).

father(ken, mary).
mother(lily, mary).
mother(mary, john).
