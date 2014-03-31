Convert-Macro
==========

Annotate a trait with @convert to transform it during compilation

consumes all traits and case classes and creates the
corresponding fixed Points and Variants respectively
All other elements of the trait, like vals, functions or type definitions 
are passed through unchanged

multiple traits can be annotated and combined using mixin composition

compiled version at: http://dl.bintray.com/bjoern-mueller/Generic-Macro/

