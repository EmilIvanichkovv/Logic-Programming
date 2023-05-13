%  = е равенство между изрази

% ------------------------------------------------------------------------
% ----------- Многозначно функционално програмиране на пролог. -----------
% ------------------------------------------------------------------------

% dx(F,G) - ако F е функция G става производната на F по х
dx(0, 0).
dx(1, 0).
dx(2, 0).
dx(3, 0).

dx(x, 1). % x - за нас е променлива, за пролог константа
% dx(X,1) казваме че за всяко Х производната е 1
dx(y, 0).
dx(z, 0).
dx(t, 0).
dx(u, 0).

dx(- F, - G ) :- dx(F,G).
dx(F1+F2, G1+G2) :- dx(F1, G1),  dx(F2, G2).
dx(F1-F2, G1-G2) :- dx(F1, G1),  dx(F2, G2).
dx(F1*F2, G1*F2+G2*F1) :- dx(F1, G1), dx(F2, G2).
dx(F1/F2, (G1*F2-G2*F1)/(F2*F2)) :- dx(F1, G1), dx(F2, G2).
dx(sin(F), cos(F)*G) :- dx(F, G).
dx(cos(F), -sin(F)*G) :- dx(F, G).

% Task 2:
% step(F, G) - G се получава от F чрез едностъпково
%              опростяване на повърхността

step(-(0), 0).
step(0+F, F).
step(F+0, F).
step(0-F, -F).
step(0*_, 0).
step(_*0, 0).
step(0/_, 0).
step(1*F, F).
step(F*1, F).
step(F/1, F).
step((-F)*G, -(F*G)).
step(F+(-G), F-G).

% deep_step(F,G) - G се получава от F чрез едностъпково опростяване( може и на подизраз)

deep_step(F,G) :-
    step(F, G); % една от възможните стойности на G;
    ( F1+F2=F, deep_step(F1, G1), G = G1+F2);
    ( F1+F2=F, deep_step(F2, G2), G = F1+G2);
    ( F1-F2=F, deep_step(F1, G1), G = G1-F2);
    ( F1-F2=F, deep_step(F2, G2), G = F1-G2);
    ( F1*F2=F, deep_step(F1, G1), G = G1*F2);
    ( F1*F2=F, deep_step(F2, G2), G = F1*G2);
    ( F1/F2=F, deep_step(F1, G1), G = G1/F2);
    ( F1/F2=F, deep_step(F2, G2), G = F1/G2);
    ( sin(F1)=F1, deep_step(F1, G1), G=sin(G1));
    ( cos(F1)=F1, deep_step(F1, G1), G=cos(G1)).

% reduce(F, G) - self note: skip for now -> see one-file-to-rule-them-all.pl

% ------------------------------------------------------------------------
% ------------------------------ Lists -----------------------------------
% ------------------------------------------------------------------------

% Having expressions we dont need lists:
% f(1, f(2, f(3 ,c))) can be interpretated like [1, 2, 3]
% f(X, Y) X is called `HEAD` of the list - the first element
%         Y is called `TAIL` of the list - all elements but first

% In prolog instad of f(X|Y) we use [X|Y]

% Convention:
    % or lists: X, Y, Z
    % for elemets: A, B, C
    % lists of lists: XX, YY, ZZ

% Struct induction (recursion): two cases [] and [A|X]

% Task 1
% even_positions(X,Y) - Y is the list of elements of X on even positions
% condition: at least one of the lists is with known length
even_positions_m([], []).
even_positions_m([_], []).
even_positions_m([_,B], [B]).
even_positions_m([_,B|X], [B|Y]) :- even_positions_m(X, Y).

% Task 2
% odd_positions(X,Y) - Y is the list of elements of X on odd positions
odd_positions_m([], []).
odd_positions_m([_], [_]).
odd_positions_m([A,_], [A]).
odd_positions_m([A,_|X], [A|Y]) :- odd_positions_m(X,Y ).

% Seminar like:
even_positions([], []).
even_positions([_|X], Y) :- odd_positions(X,Y).

odd_positions([], []).
odd_positions([A|X], [A|Y]) :- even_positions(X,Y).

% When logic programs are correct theer are two rules:
    % Rule 1: If we tell the proram correct stuff, it will give us
    %  correct answers
    % Rule 2: If the program says there are no more answers,
    %  then there realy arent any more answers

% Task 3
% What is the result of the following predicate:
p(X, Y) :-
    even_positions(X, X2),
    odd_positions(X, X3),
    even_positions(Y, X3),
    odd_positions(Y, X2).

% Answer: p([1,2,3,4], X). -> X = [2, 1, 4, 3]
% This program might not terminate if we want next result,
%   which is not acceptable behaviour.