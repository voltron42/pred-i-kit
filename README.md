# pred-i-kit

A Clojure library of functions for building predicates.

## Usage

With the implementation of spec in clojure 1.9.0, predicate helper functions can be particularly useful.

*pred-i-kit* is a small collection of functions which build predicates

### Value & Count Checks

The value and count check functions (*exact-value*, *min-value*, *max-value*, 
*value-range*, *exact-count*, *min-count*, *max-count*, *count-range*) build 
predicates to match their conditions. Exact, Min, and Max take a single value 
to compare with the value or count of the item in question. Range takes two or
three arguments to specify the range of the value in question.

### Matches?

The *matches?* predicate function not only will match the value in question to 
a specified value, a regular expression, or the values in a set, but is also 
extensible by the type of the given match.  

### Named As

The *named-as* predicate function is meant to be used on values which fulfill 
the "Named" interface, namely Symbol and Keyword. This function can be given 
either one or two arguments which are then used by the *matches?* function 
against the name and namespace of the Symbol or Keyword in question.    

## License

Copyright © 2018 Daniel Allen Johnson

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
