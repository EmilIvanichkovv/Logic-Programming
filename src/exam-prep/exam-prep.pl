:- use_module(library(clpfd)).

concat_m([], X, X).
concat_m([A|X2], Y, [A|Z]) :- concat_m(X2, Y, Z).

elem_m(A,[B|X]) :- A = B ; elem_m(A, X).

member_m(A, [A|_]).
member_m(A, [_|X]) :- member_m(A, X).

len_m([], 0).
len_m([_|T], Len) :- len_m(T, NLen), Len is NLen + 1.

first_m([A|_], A).

last_m([A], A).
last_m([_|X], L) :- last_m(X, L).

last_with_concat(X, Last) :- concat_m(_, [Last], X).

remove_last([_], []).
remove_last([A|X], [A|WL]) :- remove_last(X, WL).

remove_first([_|X], Res) :- reverse_m(X, ResTmp), reverse_m(ResTmp, Res).

remove_first_and_last([_|X], Res) :- reverse_m(X, [_|Y]), reverse_m(Y, Res).


rotation_m([], []).
rotation_m([A|X], Y) :- concat_m(X, [A], Y).

rotation_m_2([], []).
rotation_m_2(X, Y) :- last_m(X, Last), concat_m([Last], X, N), remove_last(N, Y).

reverse_help([], Curr, Curr).
reverse_help(X, Curr, Res) :-last_m(X, Last),
                            concat_m(Rest, [Last], X),
                            concat_m(Curr, [Last], Curr2),
                            reverse_help(Rest, Curr2, Res).
reverse_m(X, Result) :- reverse_help(X, [], Result).

reverse_danny([], []).
reverse_danny([A|X], Res) :-
    reverse_danny(X, Reversed), concat(Reversed, [A], Res).

insert_on_nth(List, El, 1, Res):- concat_m([El], List, Res).
insert_on_nth([H|T], El, Pos, Res) :-
    Pos > 1,
    NewPos is Pos - 1,
    insert_on_nth(T, El, NewPos, CurrRes),
    concat_m([H], CurrRes, Res).

insert_random([], El, El).
insert_random(List, El, Res) :- concat_m(A,B,List), concat_m(A, [El|B], Res).

is_palindrom([]).
is_palindrom([_]).
is_palindrom([A,A]).
is_palindrom([A,_,A]).
is_palindrom([H|T]) :-
    len_m([H|T], Len),
    Len > 2,
    H #= Last,
    last_m(T, Last),
    remove_last(T, Rest),
    is_palindrom(Rest).

is_palindrom2(X) :- reverse_m(X, Reverse), X = Reverse.

isList([]).
how_many_apearsisList([X|T]).

flatten_m([], []).
flatten_m([H|T], Res):- isList(H), flatten_m(H, HFlat), concat_m(HFlat, T, Res1),  flatten_m(Res1, Res).
flatten_m([H|T], Res):- not((isList(H))), flatten_m(T, Res1), concat_m([H], Res1, Res).

how_many_apears([], _, 0).
how_many_apears([El|T], El, Res):- how_many_apears(T, El, NewRes), Res #= NewRes + 1.
how_many_apears([H|T], El, Res):- not((H = El)), how_many_apears(T, El, Res).

index_of_first_appearance([El|_], El, 0).
index_of_first_appearance([H|T], El, Index):- not((H = El)), Index #= CurIndex + 1, index_of_first_appearance(T, El, CurIndex).

remove_on_index([_|T], 0, T).
remove_on_index([H|T], Index, Res) :-
    Index > 0,
    NewIndex is Index - 1,
    remove_on_index(T, NewIndex, CurRes),
    concat_m([H], CurRes, Res).

remove_first_appearance(List, El, Res) :-
    index_of_first_appearance(List, El, Index),
    remove_on_index(List, Index, Res).

permutation_m([], []).
permutation_m([H|T], Res) :- permutation_m(T, Res1), insert_random(Res1, H, Res).

uniques([H],[H]).
uniques([H|T], Res) :- member_m(H, T), uniques(T, Res).
uniques([H|T], Res) :- not((member_m(H, T))), uniques(T, Res1), concat_m([H], Res1, Res).

generate(El, 1, [El]).
generate(El, N, [El|Res]) :- M #= N - 1, N #> 1, generate(El, M, Res).

pack([], []).
pack(List, Res) :-
    uniques(List, Uniques),
    findall(SubLists,
            (
                member_m(El, Uniques),
                how_many_apears(List, El, Times),
                generate(El, Times, SubLists)
            ),
            Res).

% 20 05 2023 Task2
member_([A|_], A).
member_([H|T], A) :- not((H=A)), member_(T, A).

member_pairs([A|_],[B|_], [A,B]).
member_pairs([_|T1],[_|T2], [A1,B1]) :-
    member_pairs(T1, T2, [A1,B1]).

element(A, X) :- concat_m(_, [A|_], X).

element_pairs(A, B, [A1,B1]) :- concat_m(_, [A1|_], A), concat_m(_, [B1|_], B).
% element_pairs([A|_],[B|_], [A,B]).
% element_pairs([H1|T1],[H2|T2], [A1,B1]) :-
%     not((H1=A1)), not((H2=B2)), element_pairs(T1, T2, [A1,B1]).

sum_([L], L).
sum_([H|T], Res):- sum_(T, Res2), Res #= Res2 + H.

abs_(X, X):- X >=0.
abs_(X, MinusX):- X < 0 , MinusX #= 0 - X.

is_even(X):- X #= 2*_.

p(X, Y, Res):- len_m(X, LenX), len_m(Y, LenY), LenX = LenY,
                findall(Smt,
                        (member_pairs(X,Y,[X1, Y1]),
                         D #= X1 - Y1,
                         abs_(D, Smt)
                        ),
                        ToSumThem),
                sum_(ToSumThem, Res).

q(Vectus, D, Count) :-
    findall(ElD,
            (
                element(ElD, D),
                p(ElD, Vectus, ResP),
                is_even(ResP)
            ),
            Found),
    len_m(Found, Count).

get_0_or_1(1).
get_0_or_1(0).

generator_vectus(0, []).
generator_vectus(Len, Res):-
    Len #> 0,
    NewLen #= Len - 1,
    generator_vectus(NewLen, TempRes),
    get_0_or_1(X),
    concat_m([X], TempRes, Res).

d(D, K, V) :-generator_vectus(3, V), q(V, D, Count), Count #< K.

% Danny, Task 1
is_prime(P) :- P #=2.
is_prime(P) :- not(( M #>= 2, M #< P, P #= M * N)).

nat(N) :- N #= 0.
nat(N) :- nat(N-1).

k6_plus_1(X) :- nat(N), X #= N*6+1, is_prime(X).
k6_plus_5(X) :- nat(N), X #= N*6+5, is_prime(X).

upTo(1,[1]).
upTo(Top, [Top|Res]):- Top #>=0, L #= Top - 1, upTo(L, Res).

epsilon(I, Count):- upTo(I, UpToI),
                    findall(
                       P,
                        (
                            member_m(P, UpToI),
                            P #= _*6+1,
                            is_prime(P)
                        ),
                        EpsilonElems),
                len_m(EpsilonElems, Count).

nue(I, Count):- upTo(I, UpToI),
                findall(
                   P,
                    (
                        member_m(P, UpToI),
                        P #= _*6+5,
                        is_prime(P)
                    ),
                    EpsilonElems),
            len_m(EpsilonElems, Count).

su(X) :- upTo(X, Is),
        findall(
            Sat,
            (
                member_m(Sat, Is),
                epsilon(Sat, Count),
                X #= Sat + Count
                ),
            Sats),
        len_m(Sats, Len), Len #> 0.

mu(X) :- upTo(X, Is),
        findall(
            Sat,
            (
                member_m(Sat, Is),
                nue(Sat, Count),
                X #= Sat - Count
                ),
            Sats),
        len_m(Sats, Len), Len #> 0.

% Danny, Task 2
pair_n([A,B]) :- nat(N), A in 0..N, B in 0..N, label([A,B]).
pair3_n([A,B,C]) :- nat(N), A in 0..N, B in 0..N, C in 0..N, label([A,B]).

do(L, K, Res) :- findall(
                    Sat,
                    (
                        member_m(Sat, L),
                        Sat = [K,_]
                    ),
                    Sats),
                len_m(Sats, Res).

di(L, K, Res) :- findall(
                    Sat,
                    (
                        member_m(Sat, L),
                        Sat = [_,K]
                    ),
                    Sats),
                len_m(Sats, Res).

e1g(L):- pair_n(L),
         do(L, 3, ResDo),
         di(L, 3, ResDi),
         Dif #= ResDo - ResDi,
         abs_(Dif, Res),
         Res #< 1.

% Graphs
vertex((VV, _EE), V) :- member_m(V, VV).
vertex(G,V) :- member_m([V,_], G) ; member_m([_,V], G).

edge((_VV, EE), U, V) :- member_m((U,V), EE).
edge(G, U, V):- member_m([U,V], G).


% Конкретни графи G1 и G2

vertex(g1, a).  vertex(g1, b).  vertex(g1, c).  vertex(g1, d).
vertex(g1, e).  vertex(g1, f).  vertex(g1, g).  vertex(g1, h).

edge(g1, a, b).  edge(g1, a, c).  edge(g1, b, d).
edge(g1, c, d).  edge(g1, c, e).  edge(g1, d, f).
edge(g1, e, f).  edge(g1, e, g).  edge(g1, f, h).
edge(g1, g, h).

vertex(g2, a).  vertex(g2, b).  vertex(g2, c).  vertex(g2, d).
vertex(g2, e).  vertex(g2, f).  vertex(g2, g).  vertex(g2, h).

edge(g2, a, a).  edge(g2, a, e).  edge(g2, a, b).
edge(g2, b, d).  edge(g2, c, b).  edge(g2, d, c).
edge(g2, d, e).  edge(g2, e, f).  edge(g2, f, d).
edge(g2, g, h).  edge(g2, g, e).  edge(g2, h, g).
edge(g2, h, f).

path(G, U, V, [U,V]) :- edge(G, U, V).
path(G, U, V, [U|Res]) :- edge(G, U, R), path(G, R, V, Res).
path_no_loop(G, U, V, Res) :- len_m(Res, N), path(G, U, V, Res).

path_with_len(G, U, V, Len, Res) :- len_m(Res, Len), path(G, U, V, Res).
