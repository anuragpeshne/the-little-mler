(* Couples are Magnificent, too *)

datatype 'a pizza =
         Bottom
         | Topping of ('a * ('a pizza));

datatype fish =
         Anchovy
       | Lox
       | Tuna;

(*
datatype fish pizza =
         Bottom
         | Topping of (fish * (fish pizza));
*)

Topping (Anchovy,
         Topping (Tuna,
                  Topping (Anchovy,
                           Bottom)));
(* it: fish pizza *)

fun rem_anchovy (Bottom)
  = Bottom
  | rem_anchovy (Topping (Anchovy, p))
    = rem_anchovy (p)
  | rem_anchovy (Topping (Tuna, p))
    = Topping (Tuna, rem_anchovy (p))
  | rem_anchovy (Topping (Lox, p))
    = Topping (Lox, rem_anchovy (p));
rem_anchovy: (fish pizza) -> (fish pizza);

fun rem_anchovy_abridge (Bottom)
  = Bottom
  | rem_anchovy_abridge (Topping (Anchovy, p))
    = rem_anchovy_abridge (p)
  | rem_anchovy_abridge (Topping (f:fish, p))
    = Topping (f, rem_anchovy_abridge (p));
rem_anchovy_abridge: (fish pizza) -> (fish pizza);

fun rem_tuna (Bottom)
  = Bottom
  | rem_tuna (Topping (Anchovy, p))
    = Topping (Anchovy, rem_tuna (p))
  | rem_tuna (Topping (Lox, p))
    = Topping (Lox, rem_tuna (p))
  | rem_tuna (Topping (tuna, p))
    = rem_tuna (p);

fun rem_tuna_abridge (Bottom)
  = Bottom
  | rem_tuna_abridge (Topping (Tuna, p))
    = rem_tuna_abridge (p)
  | rem_tuna_abridge (Topping (f:fish, p))
    = Topping (f, rem_tuna_abridge (p));

fun rem_fish (x, Bottom)
  = Bottom
  | rem_fish (Tuna, Topping (Tuna, p))
    = rem_fish (Tuna, p)
  | rem_fish (Tuna, Topping (f:fish, p))
    = Topping (f, rem_fish (Tuna, p))
  | rem_fish (Anchovy, Topping (Anchovy, p))
    = rem_fish (Anchovy, p)
  | rem_fish (Anchovy, Topping (f:fish, p))
    = Topping (f, rem_fish (Anchovy, p))
  | rem_fish (Lox, Topping (Lox, p))
    = rem_fish (Lox, p)
  | rem_fish (Lox, Topping (f:fish, p))
    = Topping (f, rem_fish (Lox, p));
rem_fish: (fish * (fish pizza)) -> (fish pizza);

fun eq_fish (Anchovy, Anchovy)
  = true
  | eq_fish (Lox, Lox)
    = true
  | eq_fish (Tuna, Tuna)
    = true
  | eq_fish (a_fish:fish, another_fish:fish)
    = false;

eq_fish: (fish * fish) -> bool;

fun rem_fish_short (x:fish, Bottom)
  = Bottom
  | rem_fish_short (f:fish, Topping (g:fish, p))
    = if eq_fish (f, g)
      then rem_fish (f, p)
      else Topping (g, rem_fish (f, p));


fun subs_fish (sf, uf, Bottom)
  = Bottom
  | subs_fish (sf, uf, Topping (t:fish, p))
    = if eq_fish (sf, t)
      then Topping (uf, subs_fish (sf, uf, p))
      else Topping (t, subs_fish (sf, uf, p));

subs_fish: (fish * fish * (fish pizza)) -> (fish pizza);

use "./1.sml";

fun eq_num (Zero, Zero)
  = true
  | eq_num (One_more_than (n), One_more_than (m))
    = eq_num (n, m)
  | eq_num (n, m)
    = false;
(* The fifth Moral:
Write the first draft of function following all the morals.
When it is correct and no sooner, simplify.
*)
