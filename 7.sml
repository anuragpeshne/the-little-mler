(* Functions Are People, Too *)

fun identity (x)
  = x;
identity: 'a -> 'a;
(* identity is a function that consumes and produces values of the same type, no
matter what the type is *)

fun true_maker (x)
  = true;
true_maker: 'a -> bool;
(* true maker consumes value of any kind and produces a bool *)

datatype bool_or_int =
         Hot of bool
         | Cold of int;

(* Hot (true): bool_or_int;
   Cold (10): bool_or_int; *)

fun hot_maker (x)
  = Hot;
hot_maker: 'a -> (bool -> bool_or_int);

fun help (f)
  = Hot (
      true_maker (
          if true_maker (5)
          then f
          else true_maker));
help: ('a -> bool) -> bool_or_int;
(*
it doesn't matter if we replace the argument to true_maker from 5 to true,
true_maker always produces true

'a -> (bool -> bool_or_int): is a function that takes in parameters of any
kind and produces a function of type (bool -> bool_or_int);

functions can take in functions and produce them
functions are values, too;

In
if exp1
then exp2
else exp3;

exp2 and exp3 need to be of the same type

that means, f and true_maker are of same type
f: 'a -> bool

help: ('a -> bool) -> bool_or_int;
*)
datatype chain =
         Link of (int * (int -> chain));
(* to create variable of type chain, we need a function of type (int -> chain) *)

fun ints (n)
  = Link (n + 1, ints);
ints: int -> chain;
(* ints has type int -> chain, so it can be used in definition of ints *)

ints (0);
(* it = Link (1, ints) *)

fun skips (n)
  = Link (n + 2, skips);
skips (8);
(* Link (10, skips) *)

fun divides_evenly (n, c)
  = eq_int ((n mod c), 0);
divides_evenly: (int * int) -> bool;

is_mod_5_or_7 (n)
= if divides_evenly (5)
  then true
  else divides_evenly (7);
is_mod_5_or_7: int -> bool;

fun some_ints (n)
  = if is_mod_5_or_7 (n + 1)
    then Link (n + 1, some_ints)
    else some_ints (n + 1);
some_ints: int -> chain;
