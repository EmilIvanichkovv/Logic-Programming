
% ------------------------------------------------------------------------
% ------------------------------ Lists -----------------------------------
% ------------------------------------------------------------------------

% Task 1
% member_(A,X) - A is member of list X
member_m(A, [A|_]).
member_m(A, [_|Y]) :- member_m(A, Y).

% Case 1: X = [] No elements. We can say this to prolog by stating nothing
% Case 2: X = [A|Y]
member_(A, [B|Y]) :- A = B ; member_(A, Y).
% On each step the length of the second elemnt decreases -> so it is enough to
% know some upper bound of the length of the second element,
% to be sure the program will terminate.

% Task 2
% concat_(X, Y, Z) - List Z is result` of concatenation of lists X and Y
concat_m([], Y, Y).
concat_m([A|X2], Y, [A|Z]) :- concat_m(X2, Y, Z).

% Rule: Predicates for restriction ( e.g. = ), must be stated first
% After solving the problem we can define the condition:
% Condition: It is known restriction of the length ofthe 1st and 3rd arguments

% Task 3
% last_elemnt_m(A, X) - A is last element of list X
% Condition: It is known restriction of the length of the 2nd argument
last_element_m(A, X) :- concat_m(_,[A],X).

% Task 4
% first_to_last_element_m(A, X) - A is the first to last element of list X
first_to_last_element_m(A,X) :- concat_m(_,[A,_],X).

element_m2(A,X) :- concat_m(_,[A|_], X).

% Task 5.1
% prefix_m(X,Y): X is prefix of Y
% Condition: For at least one of the arguments is known restriction for its length
prefix_m(X,Y) :- concat_m(X, _, Y).

% Task 5.2
% suffix_m(X,Y) - X is suffix of Y
% Condition: For at least one of the arguments is known restriction for its length
suffix_m(X,Y) :- concat_m(_, X, Y).

% Task 5.3
% infix_m(X,Y) :- X is infix of Y.
infix_m(X,Y) :- concat_m(_,X,Z), concat_m(Z,_,Y).
% This does not terminate

% Task 6
rotation_m(X,Y) :- concat_m(X1,X2,X), concat_m(X2,X1,Y).

% Task 7
% permutation_m(X, Y) - Y is permutation of the list X
% Condition:  restriction for length of X is known
permutation_m_incorrect(X,Y) :- prefix_m(Z,X), concat_m(Z,T,X), rotation_m(Z,P), concat_m(P,T,Y).
% -> this is bullshit :/

% Induction on 1st element:
permutation_([],[]).
permutation_([A|X], Z):- permutation_(X,Y), insert_(A,Y,Z).

% So we need insert
% insert_(A,X,Y) - Insert elem A on random position in X to create Y
% Condition: restriction for length of X is known
insert_m(A,[],[A]).
insert_m(A,X,Y) :- concat_m(X1,X2,X), concat_m(X1,[A],Z), concat_m(Z,X2,Y).

insert_(A, X, Y) :- concat_m(X1, X2, X), concat_m(X1, [A|X2], Y).

insert_2(A, X, [A|X]).
insert_2(A, [B|X], [B|Y]) :- insert_2(A, X, Y).

% Induction on 2nd element:
permutation_2([],[]).
permutation_2(X, [A|Y]) :- remove_(X,A,Z), permutation_2(Z,Y).

% So we need remove_
remove_(A,X,Y) :- concat_m(X1, [A|X2], X), concat_m(X1, X2, Y).
remove_2(A,X,Y) :- insert_(A,Y,X).

% Task 8
% eq_length_m(X,Y) - lengths of X and Y are equal
eq_length_m([], []).
eq_length_m([_|X], [_|Y]) :- eq_length_m(X, Y).

% ------------------------------------------------------------------------
% ------------------------ Integer Arithmetic ----------------------------
% ------------------------------------------------------------------------
:- use_module(library(clpfd)).

% In prolog by the name of the predicate we can see the type of the arguments:
% terms or numbers
    % = - for terms
    % #= - for integers

% Rule: On the left side of :- there should not have any arithmetic expressions

% Task 1
% len_(X, N) - N is the length of the list X
% Conditions: restriction for length of X is known or is know or upper limit for N is known

len_m([],N) :- N #=0.
len_m([_|X],N) :- N #= 1 + M, len_m(X,M).

len_([], N) :- N #= 0.
len_([_|X], N) :- N #>= 0, len_(X, N-1).

% Task 2
% sum_(X,N) - N is the sum of elemtns of X
sum_m([],N) :- N #= 0.
sum_m([A|X],N) :- N #= A + M, sum_m(X,M).