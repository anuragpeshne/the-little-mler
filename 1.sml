5;
(* type: A type is a name for a collection *)

datatype seasoning =
         Salt
       | Pepper;

Salt;

datatype num =
         Zero
         | One_more_than of num;

Zero; (* a num *)
One_more_than(Zero); (* gives back a num *)

One_more_than(
    One_more_than(
        Zero));  (* gives back a num *)

(* One_more_than(
       One_more_than(
           0));
gives an error, because 0 is of type int and Zero is of type num *)

(*
there are more nums than bools, (because of the constructor?)
the num of nums is equal to number of int (coutable set)
*)
