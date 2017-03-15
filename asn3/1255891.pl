
% | ————————————————————————————————————————————————————————————————————————————
% | # 1 - xreverse
% |
% | ## References
% | http://learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse25
% |
% | ## Notes
% | Common pattern used in solution, detailed comments included to
% | demonstrate thorough understanding as per academic integrity.
% | ————————————————————————————————————————————————————————————————————————————

% Use accumulator pattern, acummulating the solution's elements into empty list.
xreverse(L,R):- xreverseAcc(L,[],R).

% Deconstruct the given list into head and tail, then recursively take the head
% of each successive tail as the head of the provided accumulator list.
xreverseAcc([H|T],A,R):- xreverseAcc(T,[H|A],R).

% Base case is true if accumulated solution list (A) matches reversed list (R).
% If R is not provided, this will set the value of R to A so as to return R = A.
xreverseAcc([],A,A).
