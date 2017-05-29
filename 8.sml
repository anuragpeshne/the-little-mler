(* Bows and arrows *)

datatype 'a list =
         Empty
         | Cons of 'a * 'a list;

datatype orapl =
         Orange
       | Apple;

fun eq_orange (Orange, Orange)
  = true
  | eq_orange (Apple, Apple)
    = true
  | eq_orange (one, another)
    = false;
eq_orange: (orapl * orapl) -> bool;

fun subst_orapl (n, a, Empty)
  = Empty
  | subst_orapl (n, a, Cons (e, t))
    = if eq_orapl (a, e)
      then Cons (n, subst_orapl (n, a, t))
      else Cons (e, subst_orapl (n, a, t));
subst_orapl: (orapl * orapl * orapl list) -> orapl list;

fun subst (rel, n, a, Empty)
  = Empty
  | subst (rel, n, a, Cons (e, t))
    = if rel (a, e)
      then Cons (n, subst (rel, n, a, t))
      else Cons (e, subst (rel, n, a, t));
subst: (('b * 'a) -> bool * 'a * 'b * 'a list) -> 'a list;

(* now we can express subst_int using subst by passing eq_int as rel *)

subst (eq_int, 15, 11,
       Cons (15,
             Cons (6,
                   Cons (15,
                         Cons (17,
                               Cons (15,
                                     Cons (8,
                                           Empty)))))));
(* results in
Cons (11,
  Cons (6,
    Cons (11,
      Cons (17,
        Cons (11,
          Cons (8,
            Empty)))))) *)

fun less_than (a, b)
  = a < b;

fun in_range ((small, large), x)
  = if less_than (large, x)
    then less_than (x, small)
    else false;
in_range: ((int * int) * int) -> bool;

subst (in_range, 22, (11, 16),
       Cons (15,
             Cons (6,
                   Cons (15,
                         Cons (17,
                               Cons (15,
                                     Cons (8,
                                           Empty)))))));
(* here 'a and 'b are different,
we are passing pair (11, 16) to rel as 'b
 'a is int and 'b is (int * int) *)
