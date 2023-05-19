:- use_module(library(clpfd)).

member_m(A, [A|_]).
member_m(A, [_|X]) :- member_m(A, X).

concat_m([], Y, Y).
concat_m([A|X2], Y, [A|Z]) :- concat_m(X2, Y, Z).

len_m([], 0).
len_m([_|X], N) :- N #= M + 1, len_m(X, M).

reverse_m([A],[A]).
reverse_m([A|X], Y) :- reverse_m(X,R), concat_m(R,[A],Y).

insert_random(List, El, Res) :- concat_m(A, [El|B], Res), concat_m(A,B,List).

insert_m(A, X, 1, Y):- concat_m([A], X, Y).
insert_m(A, [B|X], N, Y) :- M #= N-1, insert_m(A, X, M, Z), concat_m([B],Z,Y).

insert_n(Elem, List, 1, Res) :- concat_m([Elem], List, Res).
insert_n(Elem, [H|T], N, Res) :- M #= N - 1, insert_n(Elem, T, M, Res1), concat_m([H], Res1, Res).

is_palindrom([]).
is_palindrom([_]).
is_palindrom([A,A]).
is_palindrom([A,_,A]).
is_palindrom([H|X]) :- len_m([H|X], Len), Len #>2, H #= L, reverse_m([H|X], [L|_]) , trimmed([H|X], Y), is_palindrom(Y).

is_palindrom_m(X) :- reverse_m(X, Rev), X = Rev. %true
trimmed([_|X], Res) :- reverse_m(X, [_|Y]), reverse_m(Y, Res).

isList([]).
isList([_|_]).

flatten_m([E], E).
flatten_m([H|T], Res):- isList(H), flatten_m(H, HFlat), concat_m(HFlat, T, Res1),  flatten_m(Res1, Res).
flatten_m([H|T], Res):- not((isList(H))), flatten_m(T, Res1), concat_m([H], Res1, Res).

flatten_niki([Element], Element) :- not((isList(Element))).

allMemberAreDiferent([]).
allMemberAreDiferent([A]).
allMemberAreDiferent([Head|Tail]) :- not((member_m(Head,Tail))), allMemberAreDiferent(Tail).

compress_m([H],[H]).
% compress_m(List, List) :- allMemberAreDiferent(List).
compress_m([H|T], Res) :- member_m(H, T), compress_m(T, Res).
compress_m([H|T], Res) :- not((member_m(H, T) )), compress_m(T, Res1), concat_m([H], Res1, Res).

compress_d([], X, Res) :- Res = X.
compress_d([A|X], Temp, Res) :- member_m(A, Temp), compress_d(X, Temp, Res).
compress_d([A|X], Temp, Res) :- not(( member_m(A, Temp) )), concat_m(Temp, [A], ATemp), compress_d(X, ATemp, Res).

compress_d([], []).
compress_d(List, Res) :- compress_d(List, [], Res).

compress_internet([H], [H]).
compress_internet([H,H|T], X) :- compress_internet([H|T],X).
compress_internet([H|T], [H|X]) :- compress_internet(T, X).

% pack(X, Res) - [a,a,b,c,a,b,b] -> [[a,a,a],[b,b,b],[c]]
producer(El, 1, [El]).
producer(El, N, Res) :- M#=N-1, N #>1, concat_m([El], Cur, Res), producer(El, M, Cur).

count_of_el(_, [], 0).
count_of_el(El, [El|T], Res):- count_of_el(El, T,Res1), Res #= Res1 + 1.
count_of_el(El, [B|T], Res):- not(( El #= B)), count_of_el(El, T, Res).

% coint_of_el - function that find all occurences of element in list using findall
count_of_el(_, [], 0).
count_of_el_(El, List, Res) :- findall(El, member_m(El, List), Res1), len_m(Res1, Res).

pack([], []).
pack(X, Res) :- compress_d(X, Compressed), findall(Subs, (member_m(El,Compressed), count_of_el_(El,X,Count),producer(El, Count, Subs)), Res ).

prime_m(P) :- P #>=2, not(( M #>= 2, M #< P, P #= M * N, label([M,N]))).
even(A) :- A #= 2*_.
nat(N) :- N #= 0 ; nat(N - 1).
prime_generator(X):- nat(X), prime_m(X).

permutation([],[]).
permutation([H|T], Res) :- permutation(T,Res1), insert_random(Res1,H,Res).