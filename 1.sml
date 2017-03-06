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

(* A type is a name for a collection of values, and there is no overlap for any
distinct type, two things belonging to different types cannot be same *)

(*
there are more nums than bools, (because of the constructor?)
the num of nums is equal to number of int (coutable set)
*)

datatype 'a open_faced_sandwitch =
         Bread of 'a
         | Slice of 'a open_faced_sandwitch;

Bread(0); (* int open_faced_sandwitch *)
Bread(Zero); (* num open_faced_sandwitch *)
Bread(One_more_than(Zero)); (* num open_faced_sandwitch *)

Slice(Bread(0)); (* int open_faced_sandwitch *)

(*
'a open_faced_sandwitch defines not a type but a shape which represents many
different types (like generic?)

int open_faced_sandwitch is an instance of a' open_faced_sandwitch where 'a
stands for int
*)

Bread(Bread(0)); (* is a instance of (int open_faced_sandwitch) open_faced_sandwitch *)

Bread(
    Bread(
        One_more_than(
            Zero)));
(* is an instance of (num open_faced_sandwitch) open_faced_sandwitch *)

(*
The First Moral
---------------
Use datatype to describe types. (User defined types)
When a type contains lots of values, the datatype definition refers to itself.
(use self to create new values, function from one value of a type to another value
of the same type)
Use 'a with datatype to define shapes.
*)
