gender(yuri, male).
gender(steve, male).
gender(brett, male).
gender(alex, female).

loves(brett, alex).
loves(brett, yuri).
loves(brett, steve).
loves(steve, brett).
loves(alex, brett).

lovesMale(P1) :- loves(P1, P2), gender(P2, male).
lovesFemale(P1) :- loves(P1, P2), gender(P2, female).
lovesBoth(P1) :- lovesFemale(P1), lovesMale(P1).
