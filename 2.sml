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
fun only_onions(Skewer)
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
