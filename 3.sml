(* Cons is still magnificent *)

datatype pizza =
         Crust
         | Cheese of pizza
         | Onion of pizza
         | Anchovy of pizza
         | Sausage of pizza;


Anchovy(
    Onion(
        Anchovy(
            Anchovy(
                Cheese(
                    Crust)))));

fun remove_anchovy (Crust)
  = Crust
  | remove_anchovy (Cheese(x))
    = Cheese(remove_anchovy(x))
  | remove_anchovy (Onion(x))
    = Onion(remove_anchovy(x))
  | remove_anchovy (Anchovy(x))
    = remove_anchovy(x)
  | remove_anchovy (Sausage(x))
    = Sausage(remove_anchovy(x));

remove_anchovy: pizza -> pizza;

remove_anchovy(
    Cheese(
        Anchovy(
            Cheese(
                Crust))));

(* double cheese pizza
some like lots of cheese *)

fun top_anchoy_with_cheese (Crust)
  = Crust
  | top_anchoy_with_cheese (Cheese(x))
    = Cheese(top_anchoy_with_cheese(x))
  | top_anchoy_with_cheese (Onion(x))
    = Onion(top_anchoy_with_cheese(x))
  | top_anchoy_with_cheese (Anchovy(x))
    = Cheese(Anchovy(top_anchoy_with_cheese(x)))
  | top_anchoy_with_cheese (Sausage(x))
    = Sausage(top_anchoy_with_cheese(x));

top_anchoy_with_cheese: pizza -> pizza;

top_anchoy_with_cheese(
    remove_anchovy(
        Onion(
            Anchovy(
                Cheese(
                    Anchovy(
                        Crust))))));
(* Onion (Cheese (Crust)) *)

remove_anchovy(
    top_anchoy_with_cheese(
        Onion(
            Anchovy(
                Cheese(
                    Anchovy(
                        Crust))))));
(* Onion( Cheese( Cheese (Cheese (Crust)))) *)

fun subst_anchovy_by_cheese (x)
  = remove_anchovy (
      top_anchoy_with_cheese (x));

subst_anchovy_by_cheese: pizza -> pizza;

fun subst_anchovy_by_cheese2 (Crust)
  = Crust
  | subst_anchovy_by_cheese2 (Cheese (x))
    = Cheese (subst_anchovy_by_cheese2 (x))
  | subst_anchovy_by_cheese2 (Onion (x))
    = Onion (subst_anchovy_by_cheese2 (x))
  | subst_anchovy_by_cheese2 (Anchovy (x))
    = Cheese (subst_anchovy_by_cheese2 (x))
  | subst_anchovy_by_cheese2 (Sausage (x))
    = Sausage (subst_anchovy_by_cheese2 (x));

subst_anchovy_by_cheese2: pizza -> pizza;

(* Third Moral
Functions that produce values of a datatype must use the associated
constructors to build data of that type *)
