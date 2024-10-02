transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

len_row(X, N) :-
    length(X, N).
len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).
within_domain([], _).
within_domain([HD | TL], N) :-
    fd_domain(HD, 1, N),
    within_domain(TL, N).

value_t(Grid, [X|Y], Val) :-
  nth(Y, Grid, Column),
  nth(X, Column, Val).

%define constraint predicates
constraint(H, +(S,L)) :-
  addition(H, L, S).
constraint(H, *(P,L)) :-
multiplication(H, L, P).
constraint(H, -(D,J,K)) :-
  subtraction(H, J, K, D).
constraint(H, /(Q,J,K)) :-
division(H, J, K, Q).

addition(_, [], 0).
addition(H, [Term|Remaining], Sum) :-
  value_t(H, Term, Val),
  addition(H, Remaining, TrailingSum),
  Sum #= Val + TrailingSum.

multiplication(_, [], 1).
multiplication(H, [Term|Remaining], Product) :-
  value_t(H, Term, Val),
  multiplication(H, Remaining, TrailingProduct),
  Product #= TrailingProduct * Val.

subtraction(H, Pos_A, Pos_B, Difference) :-
  value_t(H, Pos_A, J),
  value_t(H, Pos_B, K),
  (Difference #= J - K; Difference #= K - J).

division(H, Pos_A, Pos_B, Quotient) :-
  value_t(H, Pos_A, J),
  value_t(H, Pos_B, K),
  (Quotient #= J / K; Quotient #= K / J).

kenken(N, C, T) :-
  len_row(T, N),               %create NxN grid
  len_col(T, N),
  within_domain(T, N),         % domain limits
  maplist(fd_all_different, T),
  transpose(T, H),             % do the same for columns
  maplist(fd_all_different, H),
  maplist(constraint(H), C),   % enforce constraints
  maplist(fd_labeling, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_unique([]).
check_unique([H|T]) :-
    row_all_unique(H),
    check_unique(T).

row_all_unique(Row) :-
	sort(Row, Sorted),
	length(Row, N_elements),
	length(Sorted, N_unique_elements),
	N_elements == N_unique_elements.

unique_list(N, List) :-
    length(List, N),
    elements_between(List, 1, N),
    all_unique(List).

all_unique([]).
all_unique([H|T]) :-
    member(H,T), !, fail.
all_unique([H|T]) :-
    all_unique(T).

elements_between(List, Min, Max) :-
    maplist(between(Min, Max), List).


plain_constraint(H, +(S,L)) :-
    plain_addition(H, L, S).
plain_constraint(H, *(P,L)) :-
    plain_multiplication(H, L, P).
plain_constraint(H, -(D,J,K)) :-
    plain_subtraction(H, J, K, D).
plain_constraint(H, /(Q,J,K)) :-
    plain_division(H, J, K, Q).

plain_addition(_, [], 0).
plain_addition(H, [Term|Remainder], Sum) :-
  value_t(H, Term, Val),
  Target is Sum - Val,
  plain_addition(H, Remainder, Target).

plain_multiplication(_, [], 1).
plain_multiplication(H, [Term|Remainder], Product) :-
  value_t(H, Term, Val),
  Target is Product//Val,
  plain_multiplication(H, Remainder, Target).



plain_subtraction(H, Pos_A, Pos_B, Difference) :-
  value_t(H, Pos_A, J),
  value_t(H, Pos_B, K),
  (Difference is J - K; Difference is K - J).

plain_division(H, Pos_A, Pos_B, Quotient) :-
  value_t(H, Pos_A, J),
  value_t(H, Pos_B, K),
  (J is K * Quotient; K is J * Quotient).

plain_kenken(N, C, T) :-
  len_row(T, N),               %create NxN grid
  len_col(T, N),
  maplist(unique_list(N), T),
  transpose(T, H),
  check_unique(H),
  maplist(plain_constraint(H), C).

/* Performance
Example test case used to measure performance:
test_B(
    4,
    [
    -(2, [1|1], [1|2]),
    *(24,[[2|3], [2|4], [1|3], [1|4]]),
    /(2,[2|1], [2|2]),
    -(1,[3|1], [4|1]),
    -(3,[3|2], [3|3]),
    +(7,[[3|4], [4|4]]),
    /(2,[4|2], [4|3])
    ]
).

kenken(using finite domain solver.) results:

| ?- fd_set_vector_max(255), test_B(N,C), kenken(N,C,T).

C = [-(2,[1|1],[1|2]),24*[[2|3],[2|4],[1|3],[1|4]],/(2,[2|1],[2|2]),-(1,[3|1],[4|1]),-(3,[3|2],[3|3]),7+[[3|4],[4|4]],/(2,[4|2],[4|3])]
N = 4
T = [[1,3,4,2],[4,2,3,1],[2,4,1,3],[3,1,2,4]] ? ;

no
| ?- statistics.
Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            2 Kb        32765 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1810 atoms     30958 atoms

Times              since start      since last

   user   time       0.006 sec       0.006 sec
   system time       0.004 sec       0.004 sec
   cpu    time       0.010 sec       0.010 sec
   real   time      12.604 sec      12.604 sec

yes

plain kenken results:

| ?- test_B(N,C), plain_kenken(N,C,T).

C = [-(2,[1|1],[1|2]),24*[[2|3],[2|4],[1|3],[1|4]],/(2,[2|1],[2|2]),-(1,[3|1],[4|1]),-(3,[3|2],[3|3]),7+[[3|4],[4|4]],/(2,[4|2],[4|3])]
N = 4
T = [[1,3,4,2],[4,2,3,1],[2,4,1,3],[3,1,2,4]] ? ;


(2195 ms) no
| ?-
statistics.
Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            2 Kb        32765 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1810 atoms     30958 atoms

Times              since start      since last

   user   time       2.199 sec       2.199 sec
   system time       0.006 sec       0.006 sec
   cpu    time       2.205 sec       2.205 sec
   real   time      34.823 sec      34.823 sec

yes

As we can see the plain kenken has a far worse performance when you
look at the user_time.  kenken using the FD solver executed in 6ms while
plain kenken took approximately 2.2 seconds.  This is orders of magnitude
slower.


 */

test_A(
    6,
     [
      +(11, [[1|1], [2|1]]),
      /(2, [1|2], [1|3]),
      *(20, [[1|4], [2|4]]),
      *(6, [[1|5], [1|6], [2|6], [3|6]]),
      -(3, [2|2], [2|3]),
      /(3, [2|5], [3|5]),
      *(240, [[3|1], [3|2], [4|1], [4|2]]),
      *(6, [[3|3], [3|4]]),
      *(6, [[4|3], [5|3]]),
      +(7, [[4|4], [5|4], [5|5]]),
      *(30, [[4|5], [4|6]]),
      *(6, [[5|1], [5|2]]),
      +(9, [[5|6], [6|6]]),
      +(8, [[6|1], [6|2], [6|3]]),
      /(2, [6|4], [6|5])
     ]
).

test_B(
    4,
    [
    -(2, [1|1], [1|2]),
    *(24,[[2|3], [2|4], [1|3], [1|4]]),
    /(2,[2|1], [2|2]),
    -(1,[3|1], [4|1]),
    -(3,[3|2], [3|3]),
    +(7,[[3|4], [4|4]]),
    /(2,[4|2], [4|3])
    ]
).
