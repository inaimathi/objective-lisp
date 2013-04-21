# Objective Lisp

*An effort to ease syntactic headaches across the Common Lisp standard without starting a new language*

## Major Goals

###### Better Method Support

It should be possible for the user to easily specialize arithmetic and sequence methods (such as `length` or `+`) on newly defined classes. It's not possible to do this in CL because those functions are all `defun`ed rather than `defmethod`ed.

###### Polymorphic comparisons

We should be able to call a single comparison operator (such as `=` or `>`) across all available types (while retaining the ability to call the default `cl:` comparison functions when we need to explicitly compare pointers), and these should also be specializable.


###### Polymorphic sequence operators

We should be able to call sequence operators (such as `memberp`, `map` or `concatenate`) without specifying an explicit return-type or comparison operator. These functions should likewise be specializable.

###### Interoperability

It should still be possible for us to call into `cl:` whenever we need to, and `:ol`-based projects should be able to interoperate with `:cl`-based projects with no more trickery than a `:shadow` or `:shadowing-import-from` call during package definition.

## Low Hanging Fruit

###### Function renaming

Where necessary, functions should be renamed in line with the standard CL naming conventions for marking predicates/side-effect functions. So far this includes the functions `null` and `member` being renamed to `nullp` and `memberp`.

###### Symbol freeing

Where appropriate, liberate reserved symbols. So far this includes the designated truth symbol `t` (`:ol` exports `true` and `false`, both of which do exactly what you think they do)

###### General utility

There are some functions and macros missing from the CL standard library that are general enough to be called for in many different types of project. Where possible, we should define and export those. So far this includes `list-all-symbols` and `with-gensyms`.

## Non-Goals

###### Hyperspec acceptance

By definition, these are additions to the language that I think are worthwhile, but that will likely never make it into the CL spec proper. Hopefully, you'll find them useful too.

###### Language branching

I'm not interested in starting Yet Another Lisp Clone a-la Clojure or Arc or Dylan. This module is meant to make Common Lisp more comfortable to work in, not to replace it.
