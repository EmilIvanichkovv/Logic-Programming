% Help predicates:

% Remove:
remove(List,Elem,Res):- append(X,[Elem|Y],List), append(X,Y,Res).

% Append:
myAppend([],X,X).
myAppend([H|T],List2,[H|Res]):- myAppend(T,List2,Res).

% Min element of two:
takeLess(X,Y,X):- X =< Y.
takeLess(X,Y,Y):- X > Y.

%  Min element of list:
minElementInList([X],X).
minElementInList([Head|Tail], Res) :-minElementInList(Tail,Res1),  takeLess(Head,Res1,Res).

% Selection sort:
% Find the Minimal element fo the lists
% Add it on the begining of the rezult and remove it form the list
% Selection Sort for the rest of the List:
selectionSort([],[]).
selectionSort(List, [Min|Res]) :- minElementInList(List, Min), remove(List, Min, NewList), selectionSort(NewList, Res).

% Quick Sort:
% Split List to two new lists with larger and smaller numbers than pivot
% Choose for pivot X, split the list to larger/smaller value lists and make Quick Sort for them :
quickSortSplit(_, [], [], []).
quickSortSplit(Pivot, [H|T], [H|Larger], Smaller) :- H > Pivot, quickSortSplit(Pivot, T, Larger, Smaller).
quickSortSplit(Pivot, [H|T], Larger, [H|Smaller]) :- H =< Pivot, quickSortSplit(Pivot, T, Larger, Smaller).

quickSort([],[]).
quickSort([H|Tail],Res) :- quickSortSplit(H,Tail,L,S), quickSort(L,LRes), quickSort(S,SRes), myAppend(SRes,[H|LRes],Res).

% Merge Sort:
% Split List to two lists with same size
% Merge already sorted lists
% Do it for beggining list and every half along
mergeSortSplit([],[],[]).
mergeSortSplit([X],[X],[]).
mergeSortSplit([X,Y|Tail],[X|FirsHalf],[Y|SecondHalf]):- mergeSortSplit(Tail,FirsHalf,SecondHalf).

merge([],[],[]).
merge(List,[],List).
merge([],List,List).
merge([Head1|Tail1], [Head2|Tail2], [Head1|Res]) :- Head1 =< Head2, merge(Tail1, [Head2|Tail2], Res).
merge([Head1|Tail1], [Head2|Tail2], [Head2|Res]) :- Head1 > Head2, merge([Head1|Tail1], Tail2, Res).

mergeSort([],[]).
mergeSort([X],[X]).
mergeSort(List,Res) :- mergeSortSplit(List,Left,Right), mergeSort(Left,Res1), mergeSort(Right,Res2), merge(Res1,Res2,Res).

% Sort Using Binary Ordered Tree.
% - Add Elements to the Binary Ordered Tree 
% - Transform List to Binary Ordered Tree 
% - Add the Elements of Binary Ordered Tree to List using Left-Root-Right Traversal
% - Make predicate that orders List using Binary Ordered Tree.

% Трее structure - Tree Has Left-Tree, Root, Right-Tree.
% tree(LT, R, RT)
% empty

% BOT = Binary Ordered Tree:
addElToBOT(X, empty, tree(empty,X,empty)).
addElToBOT(X, tree(LT,R,RT), tree(LT1,R,RT)):- X =< R, addElToBOT(X,LT,LT1).
addElToBOT(X, tree(LT,R,RT), tree(LT,R,RT1)):- X > R, addElToBOT(X,RT,RT1).

listToBOT([],empty). 
listToBOT([H|T],Tree) :- listToBOT(T,Tree1), addElToBOT(H,Tree1,Tree).

% BOT to Ordered List using Left-Root-Right Traversal
leftRootRight(empty, []).
leftRootRight(tree(LT,R,RT), List) :- leftRootRight(LT, List1), leftRootRight(RT,List2), append(List1,[R|List2],List). 

bOTSort([],[]).
bOTSort(List,Res) :- listToBOT(List,Tree), leftRootRight(Tree,Res).

% Cartesian Product:
d([],[]).
d(List, [A,B]):- member(A,List), member(B,List).
 
% Length of List:
myLength([],0).
myLength([_|T],Res):- length(T,Res1), Res is Res1+1.

% Sum  of elements of List with numbers:
mySum([],0).
mySum([H|T],Res) :- mySum(T,Res1), Res is Res1+H.

% N-th element in List:
% nthEl(List,N,El)
nthEl([H|_], 1, H).
nthEl([_|T], N, El) :- nthEl(T, M, El), N is M+1.

% Natural numbers:
natural(0).
natural(X) :- natural(Y), X is Y+1.

% Even numbers:
even(X):- natural(X), X mod 2 =:= 0.

% Odd numbers:
odd(X):- natural(X), X mod 2 =\= 0.

% Integer numbers:
myInt(0).
myInt(X):- natural(Y), Y>0, member(Z,[1,-1]), X is Y*Z.

% Between
% between(A, B, X). X in [A, B]
between(A, B, A):-A =< B.
between(A, B, X):- A<B, A1 is A + 1, between(A1, B, X).

% Range
% range(A, B, R). R = [x1,x2..xn]
range(A,B,[]) :- A>B.
range(A,B,[A|Res]) :- A1 is A+1, range(A1,B,Res).

% Fibonacci number
% fib(X)
fib(X):- fib(X, _).
fib(0, 1).
fib(Y,Z):- fib(X, Y), Z is X + Y.

% Flatten for List:
isList([]).
isList([_|_]).
flatten([],[]).
flatten([Head|Tail], Res) :- isList(Head), flatten(Head,ResHead), flatten(Tail,ResTail), append(ResHead, ResTail, Res).
flatten([Head|Tail], Res) :- not(isList(Head)), flatten(Tail,ResTail), append([Head], ResTail, Res).

% Граф:
% G=<V,E> - върхове и ребра
% V=[a,b,...] - върховете
% E=[[a,b], ....] - ребрата
% Примерен граф:
graph([[a, b, c, d], [[a, b], [b, c], [c, d], [c, a]]]).

% Проверка за ребро
edge([_, E], X, Y):- member([X, Y], E); member([Y, X], E). 

% Път между два върха в граф:
% path([V, E], A, B, Path).
path(G, A, B, Path):-path(G, A, B, [], Path).

% path(G, A, B, V, Path).
path(_, B, B, V, R):- reverse([B|V], R).
path(G, A, B, V, Path):-A\=B, edge(G, A, C), not(member(C, V)), path(G, C, B, [A|V], Path).



% Писмен изпит Информатика 2019-2020 Лятна сесия 1
% 1 Задача на пролог
task1(1,0,1,0).
task1(Curr,Prev,N,N1):-  task1(Prev,PrevPrev,N1,N2), N1>0, N2 is N1-1, N is N1+1,
                                             Curr is 5*Prev*Prev + 3*PrevPrev*PrevPrev*PrevPrev.
p(A):- not(task1(A,_,_,_)).


% Писмен изпит Информатика 2019-2020 Лятна сесия 1
% 2 Задача на пролог
% X - Списък от числа
% У - стписък от списъзци от числа
% Вариант 1:
% Всяко у от У съдържа елемент който се дели точно от Всяко х от Х
part1(X,Y) :-not((  member(YList,Y),not(( member(ElY,YList), not(( member(ElX,X), not( ElY mod ElX =:=0) )) )) )) .
% Всяко х от Х се дели точно от някой ел от у на У
part2(X,Y) :- not(( member(ElX,X), not(( member(ListY,Y), member(ElY, ListY), ElX mod ElY =:=0 )) )).
kontracular(X,Y) :- part1(X,Y), part2(X,Y).
% Followi not working
genP([],[]).
genP(X,Y):- part1(X,Y), part2(X,Y).

% Вариант 2:
% всеки у от У съдържа елУ който дели без ост някой елемент на Х
part1vol2(X,Y):-not(( member(ListY,Y),not(( member(ElY,ListY), member(ElX, X), ElX mod ElY =:= 0 )) )).
% всеки х на Х дели без остатък елУ на у от У
part2vol2(X,Y):- not(( member(Mece,X), not(( member(Agudi,Y), member(Agoda,Agodi), Agoda mod Mece =:= 0)) )) .
kentracular(X,Y) :- part1vol2(X,Y),part2vol2(X,Y).