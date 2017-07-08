(* Building on Blocks *)

fun plus (n, m)
  = if is_zero (n)
    then m
    else succ (plus (pred (n), m));

fun is_zero (n)
  = eq_int (n, 0);

exception too_small;

fun pred (n)
  = if eq_int ( n, 0)
    then raise too_small
    else n - 1;

fun succ (n)
  = n + 1;

datatype num =
         Zero
         | One_more_than of num;

fun is_zero (Zero)
  = true
  | is_zero (not_zero)
    = false;

fun pred (Zero)
  = raise Too_small
  | pred (One_more_than (n))
    = n;

fun succ (n)
  = One_more_than (n);

fun plus (n, m)
  = if is_zero (n)
    then m
    else succ (plus (pred (n), m));

(* Both definitions of plus for ints and nums are exactly the same,
both have building blocks with same names but which consume and produce values of
different type *)

(* Basic blocks needed to make plus:
   1. the type (in above case int and num)
   2. the exception: Too_small
   3. the function succ
   4. the function pred
   5. the function is_zero
*)

(* Peano Numbers *)
signature N =
sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
end;
(* sig defines signature (collection of things) between sig and end
signature keyword just links the signature and name N *)

(* A signature is like a type int -> int. Each elements of this type must be a function
   and furthermore each element must consume and produce an int *)

functor NumberAsNum()
        :>
        N
=
struct
datatype num =
         Zero
         | One_more_than of num
type number = num
exception Too_small
fun succ (n)
  = One_more_than (n)
fun pred (Zero)
  = raise Too_small
  | pred (One_more_than (n))
    = n
fun is_zero (Zero)
  = true
  | is_zero (a_num)
    = false
end;

functor NumberAsInt()
        :>
        N
= struct
type number = int
exception Too_small
fun succ (n)
  = n + 1
fun pred (n)
         if eq_int (0, n)
         then raise Too_small
         else n - 1
fun is_zero (n)
  = eq_int (n, 0)
end;

(* a functor makes a name stand for something that produces structures
() means that the functor does not depend on anything else
:> means result of using the functor is a structure with signature N
 *)
