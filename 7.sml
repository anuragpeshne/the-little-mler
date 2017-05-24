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
(* it doesn't matter if we replace the argument to true_maker from 5 to true,
* true maker always produces true *)

(*
* 'a -> (bool -> bool_or_int): is a function that takes in parameters of any
* kind and produces a function of type bool -> bool_or_int
*
* ('a -> bool) -> bool_or_int: is a function which takes in a function of type
* ('a -> bool) and produces value of type  bool_or_int 
*
* functions can take in functions and produce them
* functions are values, too
*
* help: 'a -> bool_or_int *)
