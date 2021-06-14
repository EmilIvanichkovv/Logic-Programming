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
split(_, [], [], []).
split(Pivot, [H|T], [H|Larger], Smaller) :- H > Pivot, split(Pivot, T, Larger, Smaller).
split(Pivot, [H|T], Larger, [H|Smaller]) :- H =< Pivot, split(Pivot, T, Larger, Smaller).

quickSort([],[]).
quickSort([H|Tail],Res) :- split(H,Tail,L,S), quickSort(L,LRes), quickSort(S,SRes), append(SRes,[H|LRes],Res).
