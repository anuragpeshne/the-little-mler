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

structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();

(* a functor makes a name stand for something that produces structures.
open close brackets: means that the functor does not depend on anything else;
colon gr than: means result of using the functor is a structure with signature N; *)

signature P =
sig
    type number
    val plus :
        (number * number) -> number
end;


(* PlusOverNumber *)
functor PON (stucture a_N : N):
        :>
        P
=
struct
type number = a_N.number
fun plus (n, m)
  = if a_N.is_zero (n)
    then m
    else a_N.succ (plus (a_N.pred (n), m))
end;

structure IntArith = PON (structure a_N = IntStruct);
structure NumArith = PON (structure a_N = NumStruct);

(* These structures cannot directly take raw integers as input, we need some kind
   of conversion to make integers consumable. *)

(* N_C_R: Numbers_With_Conceal_Reveal *)
signature N_C_R =
sig
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
end;
(* reveal (conceal (x)) = x *)

functor NumberAsInt()
        :>
        N_C_R
=
struct
type number = int
exception Too_small
fun conceal (n)
  = n
fun succ (n)
  = n + 1
fun pred (n)
  = if eq_int (n, 0)
    then raise Too_small
    else n - 1
fun reveal (n)
  = n
end;

functor NumberAsNum()
        :>
        N_C_R
=
struct
datatype num =
         Zero
         | One_more_than of num
type number = num
exception Too_small
fun conceal (n)
            if eq_int (n, 0)
            then Zero
            else One_more_than (conceal (n - 1))
fun succ (n)
  = One_more_than (n)
fun pred (Zero)
         raise Too_small
  | pred (One_more_than (n))
    = n
fun is_zero (Zero)
            true
  | is_zero (One_more_than (n))
    = false
fun reveal (n)
           if is_zero (n)
           then 0
           else 1 + reveal (pred (n))
end;

structure IntStruct = NumberAsInt();
structure IntArith = PON (structure a_N = IntStruct);

structure NumStruct = NumberAsNum();
structure NumArith = PON (structure a_N = NumStruct);

(* PlusOverNumber refined to use proper number *)
functor PON (stucture a_N : N):
        :>
        P where type number = a_N.number
=
struct
type number = a_N.number
fun plus (n, m)
  = if a_N.is_zero (n)
    then m
    else a_N.succ (plus (a_N.pred (n), m))
end;

signature S =
sig
    type number1
    type number2
    val similar: (number1 * number2) -> bool
end;

functor Same (structure a_N : N
              structure b_N : N)
        :>
        S where type number1 = a_N.number
          where type number2 = b_N.number
=
struct
type number1 = a_N.number
type number2 = b_N.number
fun sim (n, m)
        if a_N.is_zero (n)
        then b_N.is_zero (m)
        else sim (a_N.pred (n),
                  b_N.pred (m))
fun similar (n, m)
  = ((sim (n, m)
      handle a_N.Too_small => false)
     handle b_N.Too_small => false)
end;

functor NP (structure a_N : N_C_R
            structure a_P : P
            sharing type a_N.number = a_P.number)
        :>
        J
=
struct
fun new_plus (x, y)
  = a_N.reveal (a_P.plus (a_N.conceal (x),
                          a_N.conceal (y)))
end;

(* structure to multiply any number *)
structure NPStruct =
NP (structure a_N = NumberAsNum ()
    structure a_P = PON (structure a_N = a_N));

signature T =
sig
    type number
    val times : (number * number) -> number
end;

functor TON (structure a_N : N
             structure a_P : P
             sharing type a_N.number = a_P.number)
        :>
        T where type number = a_N.number
=
struct
type number = a_N.number
fun times (n, m)
  = if a_N.is_zero (m)
    then m
    else a_P.plus (n, times (n, a_N.pred (m)))
end;

(* The Tenth Moral
Real programs consist of many components. Specify the dependencies among these
components using signatures and functors. *)
