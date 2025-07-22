LTD: A Lisp To Dylan Conversion Tool
====================================

The program ``LTD`` is a Common Lisp program to help convert Common Lisp
programs to Dylan. It does as much as is reasonably possible, aided by
user-settable preferences, but there are often constructs that cannot be
translated directly to Dylan. These are left alone, but are flagged by
comments so that they can be changed by hand.

Installing LTD
--------------

The installation process is as follows:

#. ``git clone https://github.com/dylan-lang/lisp-to-dylan``

#. Start Common Lisp in whatever way your normally do.

#. In the REPL::

     (setf *default-pathname-defaults* #P"/path/to/lisp-to-dylan/code/")
     (load "code/load.lisp")
     (load-ltd)
     (in-package "LTD")

Using LTD
---------

First, either start Lisp and load LTD as described above.

The main entry point in `LTD
<https://github.com/dylan-lang/lisp-to-dylan/tree/master/code/ltd.lisp>`__ is the function
``ltd-files``. It operates on a list of Lisp files, translating each one to a Dylan
file. A simple call is ``(ltd-files "~user/dir/*.lisp")``.  This will convert each
".lisp" file, creating a corresponding ".dylan" file. A more complex call is ``(ltd-files
'("a.lisp" "b.lisp") :output "dylan-dir/*.dylan")``. This converts the two lisp files "a"
and "b", placing the results in the "dylan-dir" subdirectory.

To test your installation fully, run ``(test-ltd)`` to convert all 39 files in the `test
<https://github.com/dylan-lang/lisp-to-dylan/tree/master/test/>`__ directory. As a simpler
test example, run ``(ltd-files "test/TEST.lisp")`` and see the output in the file
:file:`test/TEST.dylan`.

.. _conversion-options:

Conversion Options
------------------

There are a number of `options
<https://github.com/dylan-lang/lisp-to-dylan/tree/master/code/options.lisp>`__ that the
user can set to change the way conversion is done. Each option has a name (e.g.,
``:tab-stop``), s default value (e.g., ``2``), a type (e.g., ``integer``), and a
description (e.g., ``"Number of spaces to indent for each block"``. Each option must take
on either a value of the specified type, or the constant ``:?``, which means that the
Option values can be changed with the functions ``set-option`` or ``new-options``. Here
are some examples:

.. code:: common-lisp

   ;; Make each new block indent 4 spaces:
   (set-option :tab-stop 4)

   ;; Change several options at once:
   (new-options :tab-stop 4 :comments '/* :end-name nil)

   ;; Prompt the user for the correct way to convert each NIL:
   (set-option :nil-as :?)

   ;; Prompt the user for every decision except tab stop
   (new-options :? t :tab-stop 2)

The complete list of default options is in
`options.lisp <https://github.com/dylan-lang/lisp-to-dylan/tree/master/code/options.lisp>`__.

Inside LTD, the Lisp to Dylan converter
---------------------------------------

LTD works by first reading a Lisp s-expression with ``ltd-read``, then
converting it to an internal s-expression representation which
represents a Dylan parse tree with ``cvt-exp``, and then printing the
internal representation with ``dpp-exp`` (DPP stands for Dylan
Pretty-Print). For example, calling ``ltd-read`` on the fragment of
source file

.. code:: common-lisp

   ;; Compute sign of x
   (cond ((< x 0) -1)
         ((> x 0) 1)
         (t 0))

results in the expression

.. code:: common-lisp

   #S(com :comment " Compute sign of x"
          :code (cond ((< x 0) -1)
                      ((> x 0) 1)
                      (t 0)))

which gets translated by ``cvt-exp`` to the internal form

.. code:: common-lisp

   #S(com :comment " Compute sign of x"
          :code (IF (< X 0) -1 (:ELSIF (> X 0) 1) (:ELSE 0)))

which then gets printed by ``dpp-exp`` as something like

.. code:: dylan

   // Compute sign of x
   if (x < 0) -1; elsif (x > 0) 1; else  0; end

or perhaps as

.. code:: dylan

     /* Compute sign of x */
     if (x < 0)
       -1;
     elsif (x > 0)
       1;
     else
       0;
     end

We cover the three main functions (``ltd-read``, ``cvt-exp`` and
``dpp-exp``) in turn.

LTD-READ
--------

The function ``ltd-read`` is just like ``read``, except that

- The file positions of each non-atomic expression within the file is
  retained in a table, ``*file-position-table*``, for use in printing
  error messages.
- The empty list ``()`` is read as the symbol ``|()|``, while ``nil`` is
  read as ``nil``.
- Comments in the source input are retained by wrapping a
  ``#S(com ...)`` form around the following expression.

Note that both comments delimited by semicolon and by ``#| ... |#`` are
supported. However, the strategy of attaching comments to the following
expression has a problem: sometimes there is no following expression.
Comments that appear just before a right paren or a dot in a dotted list
are silently ignored.

Note that ``ltd-read`` uses the readtable ``ltd-readtable``. If you want
to convert a program that uses its own readmacros in its source files,
then you will have to make sure that those readmacros are defined within
``*ltd-readtable*``. *(We should probably provide an easy way to do
this.)*

CVT-EXP
-------

After a form has been read, it is converted by ``cvt-exp`` as follows:

- Constant variables are converted according to Dylan conventions; for
  example, ``pi`` becomes ``$pi``, and ``*global*`` (if it is defined as
  a constant) becomes ``$global``. Note that LTD will only know that
  user-defined variables are defined as constants if the source code is
  *loaded* before it is converted to Dylan. For this and other reasons,
  we recommend loading your source code before you convert it, but good
  results can also be obtained without pre-loading the code.
- Symbols that violate Dylan lexical rules are converted. For example,
  ``~3`` is a valid symbol in Lisp; we translate that to ``%3`` in
  Dylan.
- Other atoms are left unchanged.
- Lists that start with a symbol that has been given a conversion
  function via ``ltd-fn`` are converted by that function.
- Lists that start with a symbol that has a macro definition are
  macroexpanded, then converted. (This is another reason why it can be a
  good idea to load the code first, to get the macro definitions.
  However, you may prefer to write ``ltd-fn`` specifications for the
  macros, or to leave them unexpanded.)
- Lists that start with a non-symbol are converted by converting each
  element of the list.

.. _ltd-fn:

The bulk of the work is in defining conversion functions via ``ltd-fn``.
Calls to this macro are of the form
``(ltd-fn  *Lisp* *Dylan*)``. There are three
formats that this macro accepts. Examples are:

.. code:: common-lisp

   (ltd-fn  car                     head)
   (ltd-fn  ecase                   #'cvt-ecase)
   (ltd-fn (some f . x*)            `(any? ,f . ,x*))

The first example says that the Lisp function ``car`` corresponds to the
Dylan function ``head``. This means both that ``#'car`` should translate
to ``head``, and that ``(car x)`` should translate to
``(head x')``, where *x'* is the result of converting *x*.

The second example says that to translate an expression of the form
``(ecase ...)``, call the Lisp function ``cvt-ecase`` with the
expression as its sole argument, and use the result as the translation.

The third form is more complex. It says the following:

- Given a call such as ``(some 'evenp (rest numbers))``, match (via
  destructuring-bind) ``f`` against the first argument, ``'evenp`` and
  ``x*`` against the rest, ``((rest numbers))``.
- Convert the arguments to Dylan, based on their name. For example, the
  variable name ``f`` is always converted as a function, so it becomes
  ``even?``. The variable ``x*`` is converted to the list
  ``((tail numbers))``. The exact conversion rules are below.
- Execute the right hand side (i.e. ```(any? ,f . ,x*)`` in
  this case) with the variables ``f`` and ``x*`` bound. In addition, the
  variable ``exp`` is bound to the whole expression. Return the result.
  In this case it would be ``(any? even? (tail numbers))``, which is
  printed as the Dylan expression\ ``any?(even?, tail(numbers))``.
- If the result is of the form ``(let ...)``, then convert it to
  ``(begin (let ...))``. This is because Dylan ``let`` must appear at
  the top level of a body, unlike Lisp ``let``. (The
  function ``cvt-body`` looks for and expunges unneeded ``begin`` tokens.
  It was more succint and less error-prone to make this happen
  automatically for every call to ``ltd-fn``, rather than relying on the
  programmer to get each one right.)
- If the original call to the conversion function is of the form
  ``#'some``, then something quite different happens. In this case, *[FIXME:
  incomplete sentence occurs in the original document.]*

The automatic conversion of variable names on the left-hand side of
``ltd-fn`` is tricky, and takes some getting used to, but it makes for
very compact code. The rules are:

- A name ending in ``*``, such as ``x*`` in the example above,
  represents a list; it is expanded with ``cvt-exps``.
- The name ``keys`` is left unconverted, except that if
  ``:test-not fn`` appears in the list, it is converted to
  ``:test (complement fn)``.
- The names ``f`` and ``pred`` are converted via ``cvt-fn``. That is
  ``'car`` would convert to ``head``, not ``#"car"`` if it were bound to
  a variable named ``f``.
- The names ``name``, ``ignore``, and ``asis`` are not converted.
- The name ``body`` is converted by ``cvt-body``, which handles
  declarations followed by a list of expressions.
- The names ``type`` and ``class`` are converted by ``cvt-type``. That
  is, ``'list`` is converted to ``<list>``, not ``#"list"`` when it is
  bound to a variable named ``type``.
- The names ``stdin`` and ``stdout`` convert via ``cvt-exp``, except
  that they have default values ``*standard-input*`` and
  ``*standard-output*``, respectively.

Dylan Parse Tree Language
-------------------------

The complete internal parse tree language is as follows:

::

   exp :=
     (BEGIN exp*)
     (BLOCK name exp*)
     (CASE branch*)
     (DEFINE-CLASS name args class-slot)
     (DEFINE-CONSTANT bindings exp)
     (DEFINE-GENERIC name args)
     (DEFINE-FUNCTION name args exp*)
     (DEFINE-METHOD name args exp*)
     (DEFINE-VARIABLE bindings exp)
     (DEFINE-MODULE name exp*)
     (FLUID-BIND bindings exp)
     (FOR for-clause* exp*)
     (IF test exp*)
     (LET bindings init)
     (METHOD args exp*)
     (LET-HANDLER exp exp exp*)
     (LOCAL exp*)
     (QUOTE exp)
     (SELECT exp branch*)
     (UNLESS test exp*)
     (UNTIL test exp*)
     (WHILE test exp*)

     (:CLEANUP exp*)                Allowed only in BLOCK
     (:ELSEIF test exp*)            Allowed only in IF
     (:ELSE exp*)                   Allowed only in IF
     (:EXCEPTION exp*)              Allowed only in BLOCK
     (:FINALLY exp*)                Allowed only in FOR
     (:LOCAL-METHOD name args exp*) Allowed only in :LOCAL
     (:CLAUSE [export|use] name*)   Allowed only in DEFINE-MODULE
     (:RETURN exp*)                 Allowed only as first exp in METHOD
                                    E.g. (:RETURN y x)

   test :=
     exp

   branch :=
     (:BRANCH branch-lhs exp*)

   branch-lhs :=
     exp
     (:LIST-BARE exp*)     This is like :LIST, but doesn't print parens.

   for-clause :=
     (:FOR-CLAUSE exp*)        E.g. (:for-clause until (done? thing))
     (:LIST-BARE for-clause*)

   args :=
     (:LIST exp)           E.g. (:LIST x (|::| y <number>))

   bindings :=
     exp               E.g. x
     (:LIST exp*)          E.g. (:LIST x (|::| y <number>))

   class-slot :=
     (:SLOT name exp*)
     (:SLOT-KEYWORD name exp*)

Note that this is quite distinct from the old prefix-Dylan syntax
defined in the 1992 Dylan manual. The rationale is that we wanted a
representation that would be easy to print, with only local knowledge of
each expression. So a representation like ``(cond (test val) ...)`` is a
bad one, because you have to know that the ``(test val)`` list is
treated differently than other lists; this requires non-local knowledge.
The representation ``(case (:branch test val) ...)`` is a good one,
because the ``:branch`` identifies the constituent.

Note that the choice of a good local representation means that the
printing routines never have to "parse" the representation. One pleasant
result of this is that the printing routines don't have to worry about
comments. That is, they never have to say "if the following expression
is a symbol, or a ``comment`` expression wrapped around a symbol, then
...". There is some of this kind of logic inside the conversion
routines, but none in the printing routines.

Actually, the LTD program and its author may have been inconsistent in
the use of ``:LIST`` vs ``:ARGS``; I should go back and staighten that out. The
idea is that a ``:LIST`` expression is printed (by ``dpp-list``) as a comma
separated list, while a ``:ARGS`` expression is printed (by ``dpp-args``)
the same, except that there is no comma between a keyword and a
following expression. ``:LIST-BARE`` and ``:ARGS-BARE`` are analagous, except
they do not print the parentheses around the arguments.

To get a better understanding of this, try converting some Common Lisp
forms with ``cvt-exp`` and looking at the output.

.. _pretty-printing:

Inside DPP, the Dylan pretty-printer
------------------------------------

.. note:: Not to be confused with the `Dylan pretty-printer
          <https://github.com/dylan-lang/opendylan/blob/master/sources/io/pprint.dylan>`__.

The actual printing is handled by DPP (Dylan Pretty-Printer), in the
file `dpp.lisp <https://github.com/dylan-lang/lisp-to-dylan/tree/master/code/dpp.lisp>`__. It has the main entry point
``dpp-exp``. The call ``(dpp-exp x :stream s)``
prints the internal s-expression *x* on the stream *s*. The function
*dpp-exp* accepts all the keyword arguments that ``write`` does.

There should be less need to fiddle with DPP than with LTD. Most of the
customization you will want to do (such as changing the number of spaces
to indent) can be done with the printing options. However, if you really
need more control over the output, you can make changes to the ``dpp``
entries in the file `dpp.lisp <https://github.com/dylan-lang/lisp-to-dylan/tree/master/code/dpp.lisp>`__. Here are four
example entries to give you an idea of how ``dpp`` works:

::

   (dpp 'symbol   dpp-symbol)
   (dpp (if)      dpp-conditional)
   (dpp (:else)   dpp-unindented)
   (dpp (begin)   (format s "~@<begin~W~:>" `(:body (end) ,@(rest x))))

The first example says that all expressions of type ``symbol`` will be
handled by calling the function ``dpp-symbol`` with two arguments: the
stream to print on and the symbol to print. In place of ``'symbol``, you
can use any (quoted) Common Lisp type specifier.

In the second example, ``(if)`` is used to indicate a list whose first
element is ``if``. The example says that all such lists are printed by a
function named ``dpp-conditional``. Again, the arguments are the stream
and the expression.

The third example is very similar; its purpose is to show the convention
that elements of the Dylan parse tree that represent parts of statements
start with keyword symbols (such as ``:else``), whereas those that
represent complete statements (such as ``if``) are non-keyword symbols.

The fourth example says that all lists that start with the symbol
``begin`` will be printed by calling the function

::

   (lambda (s x) (format s "~@<begin~W~:>" `(:body (end) ,@(rest x))))

That is, the expression is called with ``s`` bound to the stream and
``x`` bound to the expression to print. There are two things worth
discussing in this example.

First, the expression ``(:body (end) ,@(rest x))`` is the
internal parse tree representation for a body that should be followed by
just an "end" (rather than, say, "end method" or "end method name").

Second, the format string ``"~@<begin~W~:>"`` makes use of
pretty-printing format directives from `Chapter
27 <https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node253.html#SECTION003100000000000000000>`__
of *Common Lisp the Language: 2nd Edition*. You will need to understand
this chapter before you can make changes to DPP.

Please observe the convention that printing blocks surround a whole
statement; the ``dpp-body`` function does *not* start a new printing
block. Sub-parts may be put in blocks.

To Do List
----------

LTD has been tested on about 35,000 lines (1.1MB in 39 files) of Lisp
code (in the `test <https://github.com/dylan-lang/lisp-to-dylan/tree/master/test/>`__ directory) taken from an archive of
`Software from AI
books <https://web.archive.org/web/20030803095450/http://yoda.cis.temple.edu:8080/UGAIWWW/books/index.html>`__ and
several other sources. By "tested", I mean that LTD runs without
crashing on two platforms (Lispworks and Lucid Common Lisp, on a Sparc
20 running SunOS 4.1.4), and that a quick look at the generated Dylan
code reveals no completely obvious errors, other than the functions that
are known to be not implemented. It takes about 3 minutes to translate
the code. The resulting Lisp is 42,000 lines and 1.5MB. The "code bloat"
seems to generally run around 20% to 30% for Lisp to Dylan, but note
that the Dylan code does not include the body of macros (since we don't
know how to automatically translate them), so it is actually somewhat
worse.

The major problems (and potential risks) in using ``LTD`` appear to be:

#. Unimplemented Common Lisp functions. DEFMACRO, READ, LOAD,
   SET-MACRO-CHARACTER and EVAL seem to be the most important ones.
#. Incomplete specification of some Common Lisp functions. We know that
   FORMAT and LOOP are not completely specified, and some others may be
   too.
#. Possible failure to translate sub-pieces. For "regular" functions,
   all the arguments should be translated to Dylan, and I think this has
   been done properly. However, the translation of special forms and
   macros need to be pieced together bit by bit, and its possible we may
   have spliced in a raw Lisp portion where we should have spliced in a
   translated Dylan portion. This is hard to test because trivial
   expressions like x or 1 are the same in Lisp and Dylan, so something
   that seems to work on a simple case may in fact be buggy.
#. Run-time errors due to comments and/or the \|()\| symbol showing up
   in unexpected places. An effort was made to find these, but some more
   may be lurking.

You may be interested in seeing the counts of unimplemented functions
over the 1.1MB of Lisp test files:

::

   > (ltd-test)
   ...
   Counts of unimplmented functions:
    221 DEFMACRO
     51 READ
     34 SET-MACRO-CHARACTER
     22 LOAD
     22 EVAL
     13 REQUIRE
      8 SHADOW
      8 READ-FROM-STRING
      6 GET-INTERNAL-RUN-TIME
      5 DEFSETF
      5 PROVIDE
      5 GET-SETF-METHOD
      5 MERGE-PATHNAMES
      4 FBOUNDP
      4 DIRECTORY
      4 PATHNAME-NAME
      4 COMPILE
      3 TRACE
      3 RASSOC
      3 MAKE-STRING-OUTPUT-STREAM
      3 FIND-PACKAGE
      3 GET-OUTPUT-STREAM-STRING
      2 READ-PRESERVING-WHITESPACE
      2 MAKE-STRING-INPUT-STREAM
      2 BOUNDP
      2 COPY-READTABLE
      2 COPY-TREE
      2 INTEGER-LENGTH
      2 SET-EXCLUSIVE-OR
      2 Y-OR-N-P
      2 SYMBOL-PACKAGE
      1 PROBE-FILE
      1 USE-PACKAGE
      1 PRINC-TO-STRING
      1 GET-MACRO-CHARACTER
      1 PROGV
      1 PATHNAME-TYPE
      1 MAKE-RANDOM-STATE
      1 COMPLEX
      1 MAKE-SYNONYM-STREAM
      1 MAKE-PACKAGE
      1 READ-DELIMITED-LIST
      1 IMPORT
      1 UNINTERN
      1 SET
      1 MACROEXPAND
      1 TREE-EQUAL

Below is a list of enhancements that should be made; please let `Peter
Norvig <mailto:norvig@harlequin.com>`__ know of additional problems you
encounter or functionality you desire.

#. Add support for the over 200 unimplemented Common Lisp functions,
   especially the ones listed above. These are mostly little-used
   functions, but when *you* use the function, its no longer
   little-used.

#. Add an interface to the LispWorks editor and/or GNU/Xemacs to make it
   easier to take the warnings and edit the resulting code.

#. Make clear what library and module some of the CL functions come
   from. Right now, we assume you will just somehow know which
   `libraries <https://github.com/dylan-lang/lisp-to-dylan/tree/master/lib/>`__ to include.

#. There should be a more straightforward way to load a system or set of
   files, remember all the macro definitions, constant declarations and
   read macros, and then call ``ltd-files`` on the files.

#. The lexical environment should be tracked. If you define a local
   function with ``flet`` that happens to have the same name as a global
   Lisp function, you should get the local function when you call it
   from inside the body, not the Dylan translation of the global
   function.

#. We should handle name clashes that result from translation. E.g., we
   translate ``car`` to ``head``, but if the Lisp program defines a
   function named ``head``, that should be translated to something else.

#. We still don't handle the two/one name space problem. That is,

   ::

      (let ((list '())) (list list))

   is legal Lisp, but

   ::

      begin let list = #(); list(list) end

   which is what we would translate to, would give an error in Dylan.

#. We need to deal with packages (in-package, export, import) better.
   Currently, each EXPORT or DEFPACKAGE translates to a
   ``define module``, but we really should gather information from
   everyehwere and combine them into one big definition.

#. Support for ``loop`` is incomplete. Jeremy would like all except
   package iteration. We're close to that, but not quite. In the 1.1MB
   of Lisp code tested, there was only one LOOP clause not handled: use
   of destructuring-bind in an IN clause
   ``(loop for (literal-list label) in literal-lists ...)``.

#. Support for ``format`` is incomplete. Jeremy would especially like
   ``ADS[{^*``, and would ultimately like all except ``BOX$GTP&<``. We
   convert to FORMATTER output, but that is not a great solution.

Done List
---------

The following bug reports have been fixed:

#. ``(coerce x 'list)`` goes to ``as(<list>, x)``, not
   ``as(x, <list>)``.
#. ``new-options`` had an infinite loop.
#. Problem with superclasses and options in ``defstruct`` fixed.
#. Multiple comments with nothing to attach to cause problems. For
   example: ( ; ; ) An even trickier case is (; comment #+feature x)
   Fixing this required some additional read-table hacking.
#. Macro ``must-be-call`` was incorrectly used before defined.
#. A CLtL1-style ``loop`` form (no atoms, repeat forever) does not get a
   ``block(return)`` generated for it.
#. The following expression was translated into something with three
   ``block`` statements::

     (dolist (x (rest choices) nil)
       (let ((result (stock-random-objects x put-in)))
         (when result (return result))))

#. The ``ecase`` form wasn't handled right -- the values are not treated
   as quoted, and a default error clause is generated which is not
   needed because Dylan ``select`` defaults to an error anyway.
#. The reader distinguishes ``NIL`` and ``()``, which gets converted to ``|()|``,
   but the code doesn't always expect a ``|()|`` where a list can appear.
   Similarly, the code doesn't always check for possible comments. Added
   functions ``null/``, ``first/``, ``rest/``, ``second/``, which are the same as their
   slash-less counterparts, except that they handle ``|()|`` and comments.
   Did a global replace, so these functions are propobably called in
   places where they are not really needed.
#. Added &allow-other-keys to new-options.
#. Fixed a bug in WHEN.
#. Fixed with-slots to not introduce a local variable when the argument
   is atomic.
#. Fixed cvt-defclass to handle comments on slots.
#. Fived cvt-if-not to handle #'remove-if-not as well as (remove-if-not
   ...).
#. Added conversion functions for symbol-function and symbol-value.
#. Added ACROSS, FOR ... ON, UP/DOWNFROM, UP/DOWNTO clause to loops.
#. Fixed translation of (- x) and (/ x); special case for (- ).
#. Fixed doc strings in defstructs.
#. Reading #2A( ... ) gives an error when there is a comment in the ...,
   so I turned off comment reading within.
#. Macroexpanding sometimes gave errors because of ``|()|`` and comments.
   Added a to-normal-lisp function to get rid of these. Somewhat
   problematic.

Credits for LTD
---------------

LTD was written by `Peter Norvig <http://norvig.com/>`__ as a side
project in Aug/Sep 1995 and is copyright by
`Harlequin <http://www.harlequin.com>`__. Much of the DPP
pretty-printing code is adapted from the Harlequin Dylan file
``infixify.dylan`` in the directory ``~dylan/tools``, written by
`Jonathan Bachrach <http://www.ai.mit.edu/people/jrb/jrb.html>`__ and
`Paul Haahr <http://www.webcom.com/haahr/welcome.html>`__, and copyright
by Harlequin Ltd (although none of that code was copied verbatim, since
it is in Dylan, not Lisp). Much of the LTD conversion code is adapted
from Harlequin file :file:`lisp-to-dylan.lisp` in the directory
:file:`~swm/dylan`, written by `Scott
McKay <http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&c2coff=1&safe=off&q=author:swm%40attbi.com+>`__
and copyright by Harlequin. Some of that code was copied verbatim. The
motivation for LTD was that swm's code worked by doing replacements in a
editor buffer, and so was not portable. Also, I felt the Common Lisp
pretty-printing system could do a better job of indenting, in most
cases.

LTD requires Common Lisp with the pretty-printer that is documented in
`Chapter
27 <https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node253.html#SECTION003100000000000000000>`__
of *Common Lisp: the Language 2nd Edition*, by Guy L. Steele Jr, Digital
Press, 1990, and in even greater detail in MIT AI Lab Memo
MIT/AIM-1102a, July 1989, and in the article "`Using the New Common Lisp
Pretty Printer <https://dl.acm.org/doi/10.1145/1039991.1039996>`__", by `Richard C.
Waters <http://www.merl.com/people/waters.html>`__, *Lisp Pointers*, V,
2, 1992.
