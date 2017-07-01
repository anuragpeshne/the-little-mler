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
(* Output:
Cons (22,
      Cons (6,
            Cons (22,
                  Cons (17,
                        Cons (22,
                              Cons 8,
                                   Empty)))));
*)
(* here 'a and 'b are different,
we are passing pair (11, 16) to rel as 'b
 'a is int and 'b is (int * int) *)

fun subs_pred (pred, n, Empty)
  = Empty
  | subs_pred (pred, n, Cons (e, t))
    = if pred (e)
      then Cons (n, subs_pred (pred, n, t))
      else Cons (e, subs_pred (pred, n, t));

subs_pred (('a -> bool) * 'a * 'a list) -> 'a list;

(* what is a predicate:
   predicate is a function which consumes a value and returns boolean *)

(* substitute all 15 in above chain: we need a pred is_15 *)

fun is_15 (n)
  = eq_int (n, 15);

(* now using is_15 *)

subs_pred(is_15, 11,
          Cons (15,
                Cons (6,
                      Cons (15,
                            Cons (17,
                                  Cons (15,
                                        Cons (8,
                                              Empty)))))));
(* produces
Cons (11,
      Cons (6,
            Cons (11,
                  Cons (17,
                        Cons (11,
                              Cons (8,
                                    Empty))))))
 *)

(* similarly we can replace all numbers less than 15
   using less_than_15 *)
fun less_than_15 (n)
  = less_than (15, n);

(* and similarly in range*)

fun in_range_11_16 (n)
  = if less_than (x, 16)
    then less_than (11, x)
    else false;

(* Currying! *)
fun in_range_c (small, large)(x)
  = if less_than (x, large)
    then less_than (small, x)
    else false;

in_range_c: (int * int)  -> (int -> bool);
(* a function which takes in pair of ints and returns a function which takes in
   int and returns bool *)
(* in_range_c (11, 16) = in_range_11_16 *)

(* there is no function eq_int_funcs that takes in two functions and tell whether
   the two function are same *)

fun subs_c (pred)(n, Empty)
  = Empty
  | subs_c (pred)(n, Cons (e, t))
    = if (pred e)
      then Cons (n, subs_c (pred)(n, t))
      else Cons (e, subs_c (pred)(n, t));

fun subs_c_in_range_11_16 (n, Empty)
  = Empty
  | subs_c_in_range_11_16 (n, Cons (e, t))
    = if in_range_c (11, 16) (e)
      then Cons (n, subs_c (in_range_c (11, 16)) (n, t))
      else Cons (e, subs_c (in_range_c (11, 16)) (n, t));

(* following: *)
fun combine_long (Empty, Empty)
  = Empty
  | combine_long (Empty, Cons (b, l2))
    = l2
  | combine_long (Cons (a, l1), Empty)
    = l1
  | combine_long (Cons (a, l1), Cons (b, l2))
    = Cons (a, combine_long (l1, Cons (b, l2)));

(* can be simplified as: *)
fun combine (Empty, l2)
  = l2
  | combine (Cons (a, l1), l2)
    = Cons (a, combine (l1, l2));

combine: ('a list * 'a list) -> 'a list;

fun combine_c (Empty) (l2)
  = l2
  | combine_c (Cons (a, l1)) (l2)
    = Cons (a, combine_c (l1) (l2));

combine_c: ('a list) -> ('a list -> 'a list);

fun prefixer_123 (l2)
  = Cons (1,
          Cons (2,
                Cons (3,
                      l2)));

(* prefixer_123 is equivalent to
combine_c (Cons (1,
                Cons (2,
                     Cons (3,
                          Empty))));
Note that it is in curried form

But when curried form is used, combine_c evaluated only first Cons(1 and waits for
l2. In case of prefixer_123, all cons are evaluated
 *)

fun waiting_prefixer_123 (l2)
  = Cons (1,
          combine_c (
              Cons (2,
                    Cons (3,
                          Empty)))
                    (l2));
(* we want to produce results like prefixer_123 when used with 3 Cons instead of
what we did in waiting_prefixer *)

(* staged prefixer
Staged prefixer takes in a list and returns a function which takes in a list and
combines the two list
 *)
fun combine_s (Empty)
  = base
  | combine_s (Cons (a, l1))
    = make_cons (a, combine_s (l1))
and
make_cons (a, f)(l2)
= Cons (a, f (l2));

combine_s: 'a list -> ('a list -> 'a list);
make_cons: ('a * ('a list -> 'a list)) -> ('a list -> 'a list);

fun base (l1)
  = l1;

(* The Eighth Moral
Replace stars by arrows to reduce the number of values consumed and to increase
the generality of the function defined *)
