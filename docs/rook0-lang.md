The Rook[0] Language
====================

_Rook[0]_ is an evaluated language with minimal primitives,
expressed in standard S-expressions.

White space is relevant only to separate tokens that would
otherwise be interpreted (together) as a different token without.

Comments begin with the semi-colon character, `;` (ASCII code
`0x3b`, decimal 59), and continue to the next newline character
(ASCII code `0x0a`, decimal 10).  The newline character will _not_
be included in the (ignored) comment body, but does count as
white space, so it is effectively ignored.

The `NUL` byte (ASCII code `0x00`, decimal 0) in the input stream
will halt the lexer and any subsequent input will be ignored.
This is considered an unusual circumstance, and may cause the rest
of the _Rook[0]_ compiler / interpreter to balk.

Microsyntax
-----------

_Rook[0]_'s lexer recognizes the following token types:

  1. **T_OPENP** - A single opening parenthesis, `(` (ASCII code
     `0x28`, decimal 40).

     No token value is returned.

  2. **T_CLOSEP** - A single closing parenthesis, `)` (ASCII code
     `0x29`, decimal 41).

     No token value is returned.

  3. **T_QUOTE** - A single apostrophe, `'` (ASCII code `0x27`,
     decimal 39).

     No token value is returned.

  4. **T_IDENT** - A successive run of one or more non-space
     characters, excluding **T_CLOSEP**.

     The full lexeme (in string form) is returned as the token
     value, to allow the parser to de-reference as it sees fit.

  5. **T_STRING** - A double-quoted run of characters.  A string
     starts with the double-quote character, `"` (ASCII code
     `0x22`, decimal 34), and continues until the next _unescaped_
     double-quote character.  Internal double-quote characters are
     escape by preceding them (immediately) with a backslash
     character (`\\`, ASCII `0x5c`, decimal 92).  Literal
     backslash characters are likewise escaped.

     A partial lexeme, excluding the delimiting quotation
     characters, is returned as the token value.

  6. **T_NUMBER** - A successive run of one or more numeric, base
     10 digits, `0` - `9` (ASCII codes `0x30` through `0x39`,
     decimal 48 through 57).

     The full lexeme (in string form) is returned as the token
     value, to allow the parser to determine numeric value.

  7. **T_TRUE** - The two characters `#` (ASCII code `0x23`,
     decimal 35) and `t` (ASCII code `0x74`, decimal 116), in immediate
     succession.

     No token value is returned.

  8. **T_FALSE** - The two characters `#` (ASCII code `0x23`, decimal
     35) and `f` (ASCII code `0x66`, decimal 102), in immediate
     succession.

     No token value is returned.

There is a special token, **T_OOPS** that the lexical parser uses
to signal an unexpected token state.  This token type is not
semantically valid in any circumstance.

Syntax
------

_Rook[0]_'s syntax consists of two types of expressions: _atoms_
and _lists_.

An atom is a singular, scalar reference or representation.  The
following are scalars:

  - Identifiers (**T_IDENT**)
  - Strings (**T_STRING**)
  - Numbers (**T_NUMBER**)
  - Booleans (**T_TRUE** and **T_FALSE**)

A list is really a _construct_ (abbreviated _cons_ since the
structure shows up so regularly in discussion of Rook programs),
which is a two-value pair consisting of a `CAR` slot and a `CDR`
slot.  By convention, a list stores the first element in the `CAR`
slot, and the remainder of the list in the `CDR` slot.  The empty
list is denoted by `#f` (Boolean false).

Conceptually, a list can contain any other data type, to any any
other degree.  A simple list consisting of nothing but atoms is a
_flat list_ and is semi-analogous to arrays in other languages
like C, or linked-lists in higher-level abstractions.

Evaluation & Semantics
----------------------

The _Rook[0]_ evaluation rule is as follows:

Atoms come in two semantic varieties: _literal atoms_ (strings,
numbers, and booleans), and _symbols_ (all other identifiers).

 1. Literal atoms evaluate to themselves.

 2. Symbols evaluate, through their current binding, to a value
    stored in memory.  This value may be another symbol, a literal
    atom, or a list.

Lists are interpreted according to _head-position call format_.
If the first value (the `CAR` slot) is a symbol, it is evaluated
according to the following rules:

 1. The symbol `quote` causes the evaluation process to return the
    second expression of the form, without evaluation.

    The syntax `'foo` is shorthand for `(quote foo)`.

 2. The symbol `progn` causes the evaluation process to evaluate
    each of the remaining items in the list (recursively) and
    return the value of the last such evaluation as the value of
    the `(progn ...)` form itself.

 3. The symbol `cond` causes the evaluation process to evaluate
    every other item in the list (recursively) starting with the
    second item, until such a sub-evaluation returns a true value
    (anything except `#f`), at which point the succeeding list
    item is evaluated and returned as the value of the form
    itself.

 4. The symbol `let` introduces new bindings.  The second item in
    the form is treated as a list of these bindings.  The third
    item in the form is evaluated in a new environment, with the
    prescribed bindings.

    Each binding is itself a list of two items.  The first is the
    symbol to bind (colloquially referred to as "the variable").
    The second is evaluated to produce the value to bind.

    FIXME: Do the bindings take effect iteratively or atomically?
    i.e. what is the value of this:

        (let ((x 2))
          (let ((x 0)
                (y (* x 2)))
            x))

    is it 0 or 4?

 5. The symbol `functions` introduces new functional bindings,
    and allows the language to avoid allowing bare `(lambda ...)`
    forms everywhere (and subsequently dealing with invalid
    appearances).

    It works like this:

        (functions
            ((double (x) (+ x x))
             (triple (y) (+ y y y)))
          (double (triple 2)))

    Evaluating this form would result in the value `12`.

 6. Otherwise, the symbol is taken as the name of a defined
    procedure, either a built-in primitive op (a _primop_), or a
    user-defined function.  A new environment will be allocated,
    wherein the functions formal parameters will be bound to the
    caller-supplied arguments (each evaluated in an unspecified
    order).  Then the function will be applied, and its return
    value(s) will be taken as the value of the application.

If the first value of a list is not a symbol, it may be the
following special form: `((lambda (...) ...) ...)`.  This allows
us to more efficiently handle an inline-lambda in the calling
position of a normal form.

Primitive Operations
--------------------

The following _primops_ are built into the _Rook[0]_ language:

 1. `(eq a b)` - Returns truish if the value a is equivalent to
    the value b.  Two symbols are _eq_ if they are the same
    symbol.  Two numbers _eq_ if they are arithmetically
    equivalent.  Two strings are _eq_ if they contain if they are
    set-wise equivalent.

 2. `(atom x)` - Returns truish if the value `x` is an atom (as
    opposed to a list).  The empty list is never an atom, but `#f`
    (the Boolean false value) is.

 3. `(null x)` - Returns truish if `x` is the nil value (the empty
    list).

 4. `(car l)` - Returns the value in the CAR slot of the cons `l`.

 5. `(cdr l)` - Returns the value in the CDR slot of the cons `l`.

 6. `(print m)` - Prints the message `m` (which must be a string)
    to standard output (followed by a newline), and then returns
    truish.

 7. `(env v)` - Returns the value of the environment variable
    named `v`, or the empty string if that environment variable is
    not set in the current (UNIX) environment.

 8. `(printf f ...)` - Prints a formatted string, `f`, given
    positional parameters in `...`.

 9. `(syscall name ...)` - Issue a UNIX system call to the kernel.
    Only specific system calls (see the section entitled _System
    Calls_, below) are supported.

10. `(open f)` - Opens a file for reading, and returns a file
    descriptor, which can be passed to `(read ...)`.

11. `(read fd)` - Reads a single S-expression from the given file
    descriptor (as returned by `(open ...)`).

12. `(list ...)` - Returns its arguments as a list of values.

13. `(append lst v)` - Appends `v` onto the end of `lst` (which
    must be a list).

14. `(+1 n)` - Increments the numeric value `n` and returns the
    incremented value.

15. `(concat ...)` - Combines multiple values into a single
    string, coercing and reformatting values as necessary.
    Only operates on strings and numbers.

16. `(args)` - Returns the number of arguments given to the
    _Rook[0]_ program, via the command-line.

17. `(argn n)` - Returns the `n`th program argument, as a string.

System Calls
------------

The following system calls are supported:

 1. `(syscall 'exit n)` - Exits the current process, returning the
    given exit code to the calling program.

Yeah, that's it.  Remember that _Rook[0]_ is intended solely to
implement _Rook[1]_, and so it doesn't need any more complication.
