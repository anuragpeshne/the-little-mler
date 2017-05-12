(* Look to the Starts *)

datatype meza =
         Shrimp
       | Calamari
       | Escargots
       | Hummus;

datatype main =
         Steak
       | Ravioli
       | Chicken
       | Eggplant;

datatype salad =
         Green
       | Cucumber
       | Greek;

datatype dessert =
         Sundae
       | Mousse
       | Torte;

(Hummus, Steak, Green, Torte);
(* (meza * main * salad * dessert)
order matters *)

fun add_a_steak (Shrimp)
  = (Shrimp, Steak)
  | add_a_steak (Calamari)
    = (Calamari, Steak)
  | add_a_steak (Escargots)
    = (Escargots, Steak)
  | add_a_steak (Hummus)
    = (Hummus, Steak);
add_a_steak: meza -> (meza * main);

fun add_a_steak_abridged (x) = (x, Steak);
add_a_steak_abridged: 'a -> ('a * main);

fun eq_main (Steak, Steak)
  = true
  | eq_main (Steak, Ravioli)
    = false
  | eq_main (Steak, Chicken)
    = false
  | eq_main (Steak, Eggplant)
    = false
  | eq_main (Ravioli, Steak)
    = false
  | eq_main (Ravioli, Ravioli)
    = true
  | eq_main (Ravioli, Chicken)
    = false
  | eq_main (Ravioli, Eggplant)
    = false
  | eq_main (Chicken, Steak)
    = false
  | eq_main (Chicken, Ravioli)
    = false
  | eq_main (Chicken, Chicken)
    = true
  | eq_main (Chicken, Eggplant)
    = false
  | eq_main (Eggplant, Steak)
    = false
  | eq_main (Eggplant, Ravioli)
    = false
  | eq_main (Eggplant, Chicken)
    = false
  | eq_main (Eggplant, Eggplant)
    = true;
eq_main: (main * main) -> bool;
