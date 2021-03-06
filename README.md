# the-little-mler
examples from the book: https://mitpress.mit.edu/books/little-mler

### The First Moral
Use `datatype` to describe types. When a type contains lots of values, the
datatype definition refers to itself. Use 'a with datatype to define shapes.

### The Second Moral
The number and order of the patterns in the definition of a function should match
that of the definition of the consumed datatype.

### The Third Moral
Functions that produce values of a datatype must use the associated constructors
to build data of that type.

### The Fourth Moral
Some functions consume values of star type; some produce of star type.

### The Fifth Moral
Write the first draft of a function following all the morals. Write it is correct
and no sooner, simplify.

### The Sixth Moral
As datatype definitions get more complicated, so do the functions over them.

### The Seventh Moral
Some functions consume values of arrow type; some produce values of arrow type

### The Eighth Moral
Replace stars by arrows to reduce the number of values consumed and to increase
the generality of the function defined

### The Ninth Moral
Some functions produce exceptions instead of values; some don't produce anything.
Handle raised exceptions carefully.

### The Tenth Moral
Real programs consist of many components. Specify the dependencies among these using
signatures and functors.
