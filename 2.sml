datatype shish_kebab =
         Skewer
         | Onion of shish_kebab
         | Lamb of shish_kebab
         | Tomato of shish_kebab;

Skewer; (* shish_kebab *)

Onion( Skewer );

Onion(
    Lamb(
        Onion(
            Skewer))); (* shish_kebab *)

Skewer; (* shish_kebab with no onion or tomato or lamb *)

(* only onions *)
(* takes in shish_kebab and gives back true/false depending upon if onion is present *)
fun only_onions (Skewer)
  = true
  | only_onions (Onion(x))
    = only_onions(x)
  | only_onions (Lamb(x))
    = false
  | only_onions (Tomato(x))
    = false;
(* fn : shish_kebab -> bool is type of only_onions just as int is type of 6 *)
(* note the order of Skewer, onion, ... in datatype and function definition *)

(only_onions: shish_kebab -> bool);
(* we can verify our thoughts by asserting as above
things is consume -> things it produces

shish_kebab -> bool
is type of onions_only
 *)

only_onions(
    Onion(
        Onion(
            Skewer))); (* it = true : bool *)

fun is_vegetarian (Skewer)
  = true
  | is_vegetarian (Onion (x))
    = is_vegetarian (x)
  | is_vegetarian (Tomato (x))
    = is_vegetarian (x)
  | is_vegetarian (Lamb (x))
    = false;

is_vegetarian: shish_kebab -> bool;

datatype 'a shish =
         Bottom of 'a
         | Onion of 'a shish
         | Lamb of 'a shish
         | Tomato of 'a shish;

datatype rod =
         Dagger
       | Fork
       | Sword;

datatype plate =
         Gold_plate
       | Silver_plate
       | Brass_plate;

Onion(
    Tomato(
        Bottom(Dagger)));
(* rod shish *)

(* let's define is_veggie which checks if a shish kebab contains only vegetarian
foods, regardless of what Bottom it is in *)

fun is_veggie (Bottom (x))
  = true
  | is_veggie (Onion (x))
    = is_veggie (x)
  | is_veggie (Tomato (x))
    = is_veggie (x)
  | is_veggie (Lamb (x))
    = false;

is_veggie: 'a shish -> bool;
(* is_veggie matches arbitrary bottoms, whereas is_vegetarian matches only Skewers *)


fun what_bottom (Bottom (x))
  = x
  | what_bottom (Onion (x))
    = what_bottom (x)
  | what_bottom (Tomato (x))
    = what_bottom (x)
  | what_bottom (Lamb (x))
    = what_bottom (x);

what_bottom: 'a shish -> 'a;

(* The Second Moral
The number and order of the patterns in the definition of a function should
match that of the definition of the consumed datatype
*)
