datatype fruit =
         Peach
       | Apple
       | Pear
       | Lemon
       | Fig;

datatype tree =
         Bud
         | Flat of fruit * tree
         | Split of tree * tree;

fun flat_only (Bud)
  = true
  | flat_only (Flat (f, t))
    = flat_only (t)
  | flat_only (Split (t1, t2))
    = false;
