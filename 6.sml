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
