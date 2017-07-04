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

(* handling exception *)
(where_is (
      Cons (lx (5),
            Cons (lx (13),
                  Cons (lx (8),
                        Empty))))
 handle
 No_bacon (an_int)
 => an_int);

(* handle has syntax:
   exp1 handle pattern => exp2

Both parts of handle expression must produce values of the same type
 *)

fun list_item (n, Empty)
  = exception Out_of_range
            | list_item (n, Cons (abox, rest))
              = if eq_int (n, 1)
                then abox
                else list_item (n - 1, rest);
list_item: (int * box list) -> box;

fun find (n, boxes)
  = (check (n, boxes, list_item (n, boxes))
     handle
     Out_of_range
     => find (n div 2, boxes))
and
check (n, boxes, Bacon)
= n
| check (n, boxes, lx (i))
  = find (i, boxes);

fun path (n, boxes)
  = Cons (n,
          (check (n, boxes, list_item (n, boxes))
           handle
           Out_of_range
           => path (n div 2, boxes)))
and
check (boxes, Bacon)
= Empty
| check (boxes, lx (i))
  = path (i, boxes);

path: (int * (box list)) -> (int list);
check: ((box list) * box) -> (int list);

(* The Ninth Moral
   Some functions produce exceptions instead of values;
   some don't produce anything. Handle raised exceptions carefully.
*)
