
% Правило 1: предикатите за ограничение се пишат први, веднага след :-
%            Предикатите = #= #> #>= #<  #=< са предикати за ограничение.

% Правило 2: Предикат е написан „правилно", ако отляво на знака :-няма
%            аритметични изрази (в частност няма константи като 0, 2, -5)
%            и повтарящи се променливи.

% Правило 3: Когато предикатите са „неправилни", не бива да им даваме като
%            аргументи аритметични изрази.

% Правило 4: Предикатите от библиотеката на пролог не са написани „правилно".
%            Т.е. не е спазено „Правило 2" и затова трябва да се собразяваме с
%            Правило 3

member_m(A, [A|_]).
member_m(A, [_|Y]) :- member_m(A, Y).

concat_m([], Y, Y).
concat_m([A|X2], Y, [A|Z]) :- concat_m(X2, Y, Z).

:- use_module(library(clpfd)).

len_m([],N) :- N #= 0.
len_m([_|X],N) :- N #= 1 + M, len_m(X,M).

% Task 1
% sum_(X, N) - N is the sum of the elements of the list X
% Condition: known upper bound of the length of X
sum_m([], N) :- N #= 0.
sum_m([A|X], N) :- N #= N1 + A, sum_m(X, N1).

% Task 2
% nth_(X,N,A) - A is the Nth element of a
% Condition: known upper bound of the length of X or upper bound of N
nth_m([B|_], N, B) :- N #= 1.
nth_m([_|X], N, A) :- K #= N-1, N #> 1, nth_m(X, K, A).

% once(Goal) - Satisfy Goal only once

% Task 3
% max_(A, B, C) - C is the maximum of A and B
% Note: If we want to use `<=` we have to write it like `=<`
max_m(A, B, C) :- A #> B, C #= A; B#>= A, C #= B.

% Note:
% (A -> B ; C) - If A then B, else C
% Condition: In A there are no unknown variables

% Condition: A anb B are known
max_2(A, B, C) :- ( A #=< B -> C #= B ; C #= A).

% Note:
% `\+` or `not` is negation

% Note:
% forall(P, Q) - Q is true if P is true for all possible values of its variables
% To all unknown variables in P, the predicate will place quantor `for all`.
% To all unknown variables in Q, the predicate will place quantor `exists`.

% Task 4
% all_even(X) - all elements of X are even
all_even([]).
all_even([A|X]) :- even(A), all_even(X).

even(A) :- A #= 2*_.

all_even_2(X) :- forall(element(A, X), A #= 2*_).

element(A, X) :- append(_, [A|_], X).

% Note:
% label([A, B]) - Find all possible values for A and B


% Task 5
% elips(A,B,X,Y) - To generate the X and Y coordinates of all points with
%                  integer coordinates that are in the interior of
%                  ellipse with center center of the coordinate system and
%                  semi-axes A along Ox and B along 0y.

elipse(A, B, X, Y) :-
    -A #=< X, X #=< A,
    -B #=< Y, Y #=< B,
    % (x/a)^2 + (y/b)^2 <= 1
    X*X*B*B + Y*Y*A*A #=< A*A*B*B,
    label([X, Y]). % To generate all possible values for X and Y

% Task 6
% r(X, Y) - every element of X that is even is an element of Y
r(X,Y) :- forall( (member_m(A, X), even(A)), member_m(A, Y) ).

% Note:
% findall(A, P, X) - X is the list of all values of A for which P is true

% Requirements for using findall similar to those of not and forall.
% On all things in P that do not occur in A and when findall is called
% have not yet received a value, will be a complex quantifier ∃. After
% implementation of P all variables in P and A (not just arithmetic ones)
% must have been assigned specific values.

% Example:

% X is a list of the even elements of [1,3,4,2,9,22]:
%% ?- findall(A, (member_m(A,[1,3,4,2,9,22]), A #= 2*K), X).
%% X = [4, 2, 22].

% Task 7
% rr(X,Y) - Fill Y with all even elements of X
rr(X,Y) :- findall( A, (member_m(A, X), even(A)), Y).

% Task 8
% p(X) -  X is list of lists. p(X) checks if each second to last element of
%         each list which is on even position in X is prime

second_to_last_element_m(X,A) :- concat_m(_,[A,_],X).
prime_m(P) :- P #>=2, not(( M #>= 2, M #< P, P #= M * N, label([M,N]))).

even_positions([], []).
even_positions([_], []).
even_positions([_, B|Y], [B|Rest]) :- even_positions(Y, Rest).

find_each_firs_to_last(X, R) :- findall(A, (member_m(B, X), second_to_last_element_m(B, A)), R).
second_to_last_on_each_even_position(X, S) :- findall(S1, (even_positions(X, A), find_each_firs_to_last(A, S1)), S).

p_m(X) :- forall(( second_to_last_on_each_even_position(X, S), member_m(A,S), member_m(P,A)), prime_m(P)).
% Example:
% ?- p([[1,2,3,4],[12,12,5,11], [1,1,1,1,1], [1,2,19,8]]).
% true.

% Zinoviev's approach:
abs(X, Result) :-
    X #>= 0,
    Result #= X.
abs(X, Result) :-
    X #< 0,
    Result #= -X.

primes_z(A) :- not((N #= A * B, abs(A) #>= 2, abs(B) #>= 2, label([A,B]))).
prime_d(N) :- not(( N #= A*B, abs(A, A1), A1 #>= 2, abs(B, B1), B1 #>= 2, label([A, B]) )).
p(XX) :- forall(( % X is element of XX on even position
                 nth_m(XX,N,X), N #=2*_,
                 % A is second to last element of X
                 concat_m(_,[A,_],X)),
                prime_d(A)).

% Task 9
% q(X, N) - X is list of lists. q(X) finds the sum of each second to last element
%            of each list which is on even position in X

q_m(XX,N) :- findall(A, ( nth_m(XX,N,X), N #=2*_, concat_m(_,[A,_],X)), S), sum_m(S,N).

% Task 10
% qq(X, N) - X is list of lists. q(X) finds the sum of each second to last element
%            of each list which is on even position in X and all of it's elements are prime

q_m(XX,N) :- findall(A, (
                         nth_m(XX,N,X),
                         N #=2*_,
                         forall(member_m(B,X), even(B)),  concat_m(_,[A,_],X)
                        ), S),sum_m(S,N).

% Note:
% Generator of integers:
nat(N) :- N #= 0 ; nat(N - 1).

even_generatpor(N) :- nat(N), even(N).

% infinite_generator(X) :- nat(N), finite_generator(N, X)

% Task 11
% g(X) - Generates in X all two element lists with integers
% Wrong approach:
% g(X):- nat(A), nat(B), X = [A, B]

g([A,B]) :- nat(N), 0 #< A, A #=< N, 0 #< B, B #=< N, label([A,B]).

% Task 12
% s(X) - Generetes in X all lists of integers
s(X) :-
    nat(N),
    M #< N,
    len_m(X, M),
    X ins 0..N, % all elems of X are between 0 and N
    label(X).