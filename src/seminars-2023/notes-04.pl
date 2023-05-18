
member_m(A, [A|_]).
member_m(A, [_|Y]) :- member_m(A, Y).

:- use_module(library(clpfd)).

len_m([],N) :- N #= 0.
len_m([_|X],N) :- N #= 1 + M, len_m(X,M).

% ------------------------------------------------------------------------
% ------------------------------ Graphs ----------------------------------
% ------------------------------------------------------------------------

% Представяния на графи на пролог

% edge ще бъде двойка (u, v), кьдето u и у са върхове.

% I начин: графьт е двойка (VV, EE), където VV е списьк от върховете, EE е
%          списьк от ребрата. (Трябва да се доуточни дали позволяваме повторения във VV и ЕЕ.

% II начин: графт е списьк списьци G. Елемент [ve, v1, v2, ...vn] на G
%           означава, че от врьх v0 излизат ребра кьм вьрхове v1, v2, ... ,vn.

% Пример. Граф с един врьх у и без ребра. При І начин: ([v], []). При
% II начин: ([v]].

% vertex(G, V) - V е vertex в G
%
vertex( (VV,_), V) :- member_m(V, VV).
vertex(G,V) :- member_m([V|_], G).

edge((_,EE), V, W) :- member_m((V,W), EE).
edge(G, V, W) :- member_m([V|L], G), member_m(W, L).

% vertexes(G, VV) :- VV is the list of vertexes of G
vertexes(G, VV) :- findall(V, vertex(G, V), X),
                   sort(X, VV). %used to remove duplicates

% III начин (специфичен за пролог): с факти.
% Конкретни графи G1 и G2
:- discontiguous vertex/2.
:- discontiguous edge/3.

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

% Дефиниция:
% Път в граф G наричаме списък  от върхове с дължина поне 1, такъв че
% всеки два последователни върха са свързани с edge в G.

% Task 1
% path_from_to(G, X, V, W):- X is path in G, starting from V and ending in W.
path_from_to(G, [V], V, V) :- vertex(G, V).
path_from_to(G, [V,V1|X], V, W) :- edge(G, V, V1), path_from_to(G, [V1|X], V1, W).

% Taks 2
% some_path_from_to(G, X, V, W) - In G there is path from V to W.
%                                 In X is saved example path From V to W

some_path_from_to(G, X, V, W) :-
    M #=< N,
    vertexes(G, VV),
    len_m(VV, N),
    len_m(X, M),
    path_from_to(G, X, V, W).

% Task 3
% path_lenght(G, V, W, N) :- shortest path from V to W in G is with lenght is N.
min_m(A, B, C) :- A #< B, C #= A; B#=< A, C #= B.

min_el_list([N], N).
min_el_list([A|X], N):- min_m(A, T, N), min_el_list(X, T).

path_length(_, V, V, 0).
path_length(G, V, W, N) :- once((some_path_from_to(G, X, V, W), len_m(X, R), N#=R-1)).

% path_length_z(G, V, W, M) :-
%     M #=< N,
%     vertexes(G, VV),
%     len_m(VV, N),
%     len_m(X, M+1),
%     path_from_to(G, X, V, W),
%     not(( K #< M,
%         len_m(Y, K+1),
%         path_from_to(G,Y,V,W))).

% Task 4
% diameter(G, N) - N is the longest path in G
max_m(A,B,A) :- A #>= B.
max_m(A,B,B) :- A #< B.

max_el_list([N],N).
max_el_list([A|X], N) :- max_m(A, T, N), max_el_list(X, T).

diameter(G,N) :- findall( LL, (path_from_to(G, X, _, _),
                               len_m(X, LL+1)), LS), max_el_list(LS, N).

% Task 4 - Separator ( see 5th record -30:00 for task description)
% 1.
generate_partition_m(G,A,S,B) :-
    vertexes(G, VV),
    member_m(Ax,VV),
    member_m(Bx,VV),
    member_m(Sx,VV),
    member_m(Ax,A),
    member_m(Bx,B),
    not((edge(G, Ax, Bx))),
    member_m(Sx, S), not((member_m(Sx, A))), not((member_m(Sx, B))),
    label([A,S,B]).


% izvadka(X, Y, N, Z) - Z е списък от онези елемнти X[i], за които Y[i] = N
izvadka([],[], _, []).
izvadka([A|X], [B|Y], N, [A|Z]) :- N #= B, izvadka(X,Y,N,Z).
izvadka([A|X], [B|Y], N, Z) :- N #\= B, izvadka(X,Y,N,Z).


generate_partition_z(G, A, S, B) :-
    vertexes(G, VV),
    len_m(VV, N),
    len_m(Colors, N),
    Colors ins 0..2,
    label(Colors),
    izvadka(VV, Colors, 0, A),
    izvadka(VV, Colors, 1, B),
    izvadka(VV, Colors, 2, S).
