/* Променливи: Maria, X, Y, This_lalalalalallala, _ 
Константи: атоми и числа:
атом: 'нещо такова', this_atom, a, +-*>; спец: [], {}, ;, !
числа: integer, floating point
Съставни термове: foo(h(0),baz(0)), foo(a)
Списъци (специални термове): [], (X-element, Y-list -> [X|Y])
Съждителни връзки: (, -> LogicAnd); (; -> LogicOr); (not(unary) -> LogicNot).
Предикати: започват с малки букви p(x, y), lessEq(X, Y).
*Всички клаузи завършват на точка!!!!!!!
S |= f 
 X=f(maria)
3 вида клаузи(хорнови): 
- факти: p(x, y). // <x, y> \in p; p(X,Y).
- правила: p(   ):-p1(), p2(), ..., pn(). //p1 , p2 ,... pn -> p
- цели: ?-p1(), p2(), ...,pn(). */

% Parent | Grandparent | Sibling | Ancestor examples:
parent(maria, ivan).
parent(ivan, peter).
parent(ivan, stoyan).

grandparent(X, Y):- parent(X, Z), parent(Z, Y).

sibling(X, Y):- parent(Z, X), parent(Z, Y), X \= Y.

% X Z1--------Zn Y
% ancestor(X, X):-parent(X, _); not(parent(_, X)).
% ancestor(X, X):-parent(_, X); not(parent(_, X)). Ако може Х да е прародиелн на себе си
% ancestor(X, X):-not(parent(_, X)); parent(X, _). 
ancestor(X, Y):-parent(X, Y).                    % Ако не може Х да е прародиелн на себе си
ancestor(X, Y):-parent(X, Z), ancestor(Z, Y).

% Derevative:
% d(X, DX). x 2*x*x + x
d(x, 1).
d(X, 0):-number(X).
d(X+Y, DX+DY):-d(X, DX), d(Y, DY).
d(X*Y, DX*Y+X*DY):-d(X, DX), d(Y, DY).
d(sin(X), cos(X)*DX):-d(X, DX).

% List alike structure:
empty(c).
add(X, C, f(X, C)).


% Usefull List Functions:

% Append Implementation:
myAppend([], B, B).
myAppend([X|A], B, [X|C]):-myAppend(A, B, C).

% First | Second | Last | Member with append implementation:
firstAppendVersion(X, L):-myAppend([X], _, L).
secondAppendVersion(X,L):-myAppend([_],[X|_],L).
thirdAppendVersion(X,L):-myAppend([_,_],[X|_],L).
lastAppendVersion(X,L):-myAppend(_,[X],L).
memberAppendVersion(X, L):-myAppend(_, [X|_], L).

% Index Function - Not working:
myIndex([X|_], X,  0).
myIndex([_|T], X, N):-
    N1 is N-1
    myIndex(T, X, N1). 

% Working Index Function form StackOverflow:
indexOf([Element|_], Element, 0). % We found the element
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), % Check in the tail of the list
  Index is Index1+1.  % and increment the resulting index
  
% Sum of elements of List:
listsum(0, []).
listsum(Result, [Head|Tail]) :-
    listsum(SumOfTail, Tail),
    Result is Head +  SumOfTail.

% Insert:
% insert(X, L, R) -> X=1, L=[a,b] -> [1,a,b]; [a,1,b]; [a,b,1]
myInsert(X, L, R):- myAppend(A, B, L), myAppend(A, [X], C), myAppend(C, B, R).

% Remove:
% remove(X, L, R) -> X=a, L=[a,b,a] -> [b,a]; [a,b]
nikiRemove(X,List,Result):- append(A,B,List), append([X],D,B), append(A,D,Result).

myRemove(X, List, Result):- append(List1, [X|List2], List), append(List1, List2, Result).

ourRemove(X, [X|Tail], Tail).
ourRemove(X, [Head|Tail], [Head|NewTail]):- ourRemove(X, Tail, NewTail).