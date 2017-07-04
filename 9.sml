(* Oh No! *)

datatype 'a list =
         Empty
       | 'a * 'a list;

datatype box =
         Bacon
         | lx of int;

fun is_bacon (Bacon)
  = true
  | is_bacon (lx (n))
    = false;

is_bacon: box -> bool;

exception No_bacon of int;

fun where_is (Empty)
  = raise No_bacon (0)
  | where_is (Cons (a_box, rest))
    = if is_bacon (a_box)
      then 1
      else 1 + where_is (rest);
