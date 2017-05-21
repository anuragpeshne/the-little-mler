datatype fruit =
         Peach
       | Apple
       | Pear
       | Lemon
       | Fig;

datatype tree =
         Bud
         | Flat of fruit * tree
         | Split of tree * tree;

fun flat_only (Bud)
  = true
  | flat_only (Flat (f, t))
    = flat_only (t)
  | flat_only (Split (t1, t2))
    = false;
flat_only: tree -> bool;

fun split_only (Bud)
  = true
  | split_only (Flat (f, t))
    = false
  | split_only (Split (s, t))
    = if split_only (s)
      then split_only (t)
      else false;
split_only: tree -> bool;
(* we could have also use andalso
split_only (s) andalso split_only (t) *)

Split (
    Split (
        Bud,
        Split (
            Bud,
            Bud)),
    Bud);

fun contains_fruit (Bud)
  = false
  | contains_fruit (Flat (f, t))
    = true
  | contains_fruit (Split (s, t))
    = if contains_fruit (s)
      then true
      else contains_fruit (t);
contains_fruit: tree -> bool;
(* we could have also used orelse
contains_fruit (s) orelse contains_fruit (t) *)

fun contains_fruit_short (t)
  = if split_only (t)
    then false
    else true;

fun less_than (n:int, m:int)
  = (n < m);

fun larger_of (n:int, m:int)
  = if less_than (n, m)
    then m
    else n;

fun height (Bud)
  = 0
  | height (Flat (f, t))
    = height (t) + 1
  | height (Split (s, t))
    = larger_of (height (s), height (t)) + 1;
fun height: tree -> int;

fun eq_fruit (Peach, Peach)
  = true
  | eq_fruit (Apple, Apple)
    = true
  | eq_fruit (Pear, Pear)
    = true
  | eq_fruit (Lemon, Lemon)
    = true
  | eq_fruit (Fig, Fig)
    = true
  | eq_fruit (a_fruit, another_fruit)
    = false;
fun eq_fruit: (fruit * fruit) -> bool;

fun subs_in_tree (n, a, Bud)
  = Bud
  | subs_in_tree (n, a, Flat (f, t))
    = if eq_fruit (a, f)
      then Flat (n, subs_in_tree (n, a, t))
      else Flat (f, subs_in_tree (n, a, t))
  | subs_in_tree (n, a, Split (t1, t2))
    = Split (subs_in_tree (t1), subs_in_tree (t2));
fun subs_in_tree: (fruit * fruit * tree) -> tree;

fun occurs (a, Bud)
  = 0
  | occurs (a, Flat (f, t))
    = if eq_fruit (a, f)
      then 1 + occurs (t)
      else occurs (a, t)
  | occurs (a, Split (t1, t2))
    = occurs (a, t1) + occurs (a, t2);

datatype
a' slist =
Empty
| Scons of ((a' sexp) * (a' slist))
and
a' sexp =
An_atom of 'a
| A_slist of ('a slist);
(* mutually self referential functions datatypes lead to mutually self referential
functions *)

fun
occurs_in_slist (a, Empty)
  = 0
| occurs_in_slist (a, Scons (s, y))
  = occurs_in_sexp (a, s) + occurs_in_slist (a, y)
and
occurs_in_sexp (a, An_atom (b))
= if eq_fruit (a, b)
  then 1
  else 0
| occurs_in_sexp (a, A_slist (y))
  = occurs_in_slist (a, y);

occurs_in_slist: (fruit * (fruit slist)) -> int;
occurs_in_sexp: (fruit * (fruit sexp)) -> int;

fun
subs_in_slist (a, b, Empty)
  = Empty
| subs_in_slist (a, b, Scons (s, y))
  = Scons (subs_in_sexp (a, b, s), subs_in_slist (a, b, y))
and
subs_in_sexp (a, b, An_atom (m))
= if eq_fruit (a, m)
  then An_atom (b)
  else An_atom (m)
| subs_in_sexp (a, b, A_slist (s))
  = A_slist (subs_in_slist (a, b, s));

fun
rem_from_slist (a, Empty)
  = Empty
| rem_from_slist (a, Scons (s, y))
  = if eq_fruit_in_atom (a, s)
    then rem_from_slist (a, y)
    else Scons (rem_from_sexp (a, s), rem_from_slist (a, y))
and
rem_from_sexp (a, An_atom (b))
= An_atom (b)
| rem_from_sexp (a, A_slist (l))
  = A_slist (rem_from_slist (a, l));

fun eq_fruit_in_atom (a, An_atom (b))
  = eq_fruit (a, b)
  | eq_fruit_in_atom (a, A_slist (y))
    = false;
eq_fruit_in_atom: (fruit * (fruit sexp)) -> bool;

fun
rem_from_slist_simpler (a, Empty)
  = Empty
| rem_from_slist_simpler (a, Scons (An_atom (b), y))
  = if eq_fruit (a, b)
    then rem_from_slist_simpler (a, y)
    else Scons (An_atom (b), rem_from_slist_simpler (y))
| rem_from_slist_simpler (a, Scons (A_slist (x), y))
  = Scons (rem_from_slist_simpler (a, x), rem_from_slist_simpler (a, y))
and
rem_from_sexp_simpler (a, An_atom (b))
= An_atom (b)
| rem_from_sexp_simpler (a, A_slist (y))
  = A_slist (rem_from_slist_simpler (a, y));

rem_from_sexp_simpler: (fruit * (fruit sexp)) -> (fruit sexp);
rem_from_slist_simpler: (fruit * (fruit slist)) -> (fruit slist);

(* The Sixth Moral
As datatype definitions get more complicated,
so do the functions over them *)
