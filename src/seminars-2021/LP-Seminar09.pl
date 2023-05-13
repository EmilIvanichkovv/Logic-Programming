% Seminar 9

% insert(X, L, R) -> X=1, L=[a,b] -> [1,a,b]; [a,1,b]; [a,b,1]

insert(X, L, R):- append(A, B, L), append(A, [X], C), append(C, B, R).
insert1(X, List, Result):- append(List1, List2, List), append(List1, [X|List2], Result).

% remove(X, L, R) -> X=a, L=[a,b,a] -> [b,a]; [a,b
remove(X, List, Result):- append(List1, [X|List2], List), append(List1, List2, Result).

% remove1(X, List, Result) with Recursion
remove1(X, [X|Tail], Tail).
remove1(X, [Head|Tail], [Head|NewTail]):- remove1(X, Tail, NewTail).

% isSorted(L) 
isSorted([]).
isSorted([_]).
isSorted([X, Y|Tail]):-lessOrEqual(X, Y), isSorted([Y|Tail]).
% isSorted(L) (forall x \in L) (forall y \in L after x)  lessOrEqual(x, y)
isSorted1(L):- not(( append(_, [X, Y|_], L), not(lessOrEqual(X, Y)) )).

% ALWAYS CHANGE FORALL TO NOT EXIST !!!
/* 
AxF-> \forall x F
ExF -> \exists x F
~F -> \neg F
AxF |=| ~Ex~F
|= AxF <-> ~Ex~F
*/

% Task1:


% Analysis and Ideas for realization:
% p1: EaEb member(a, b)
% p2: EaAb member(a, b) |=| Ea~Eb~member(a, b)
% p3: AaEb member(a, b) |=| ~Ea~Eb member(a, b)
% p4: AaAb member(a, b) |=| ~Ea~~Eb~ member(a, b) |=| ~EaEb~member(a, b)

% Salution:
p1(X, Y):-member(A, X), member(B, Y), member(A, B).
p2(X, Y):-member(A, X), not((member(B, Y), not(member(A, B)))).
p3(X, Y):-not((member(A, X), not((member(B, Y), member(A, B))))).
p4(X, Y):-not((member(A, X), member(B, Y), not(member(A, B)))).

% Prefix | Suffix | Infix:
prefix(L, P):- append(P, _, L).
suffix(L, S):- append(_, S, L).
% Infix in thow ways:
% [1, 2, a, s, d] -> [a,s,d] ->  [a, s]
infix(L, SubL):- suffix(Suff, L), prefix(SubL, Suff).
% [1, 2, a, s, d] -> [1,2,a,s] -> [a, s]
infix1(L, SubL):- prefix(Pref, L), suffix(SubL, Pref).
% A SubL B
% append(A, SubL, C), append(C, B, L)

% SubSequence:
% [a,b,c] -> [1,1,1]
% [a,c] -> [1,0,1] ->>> [X|T] -> ([X|Res] || Res)
subsequence([], []).
subsequence([_|T], Res):- subsequence(T, Res).
subsequence([H|T], [H|Res]):- subsequence(T, Res).

% Subest check:
% (Ax \in L1)(x \in L2) |=| Ax (x \in L1 -> x \in L2).
isSubset(L1, L2):- not(( member(X, L1), not(member(X, L2)) )).

isSubset1([], _).
isSubset1([H|T], L):- member(H, L), isSubset1(T, L).

% Task2:
% m(L, M) Да се генерират в М всички списъци, чиито елементи са елементи на L.
% [a, b] [], [a], [a, a], [a, b, a, b, b, b]

m(_, []).
m(L, [H|T]):- member(H, L), m(L, T).
% [], [a], [a, a]

m1(_, []).
m1(L, [H|T]):- m1(L, T), member(H, L).
% [], [a], [b], [c] ... [a,b], [a,c], ... [b,a], [b, c], ... [a, b, c] ....