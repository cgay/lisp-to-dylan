=====================================
Pitfalls for Lisp to Dylan Conversion
=====================================


Dylan and Common Lisp (which we abbreviate in this document as "Lisp")
are fairly similar languages, but it is not trivial to translate a Lisp
program into Dylan. That is, if you want to take a Lisp program "out of
the box" and convert it to run 100% correctly in Dylan, you would
essentially need to either write a Lisp interpreter in Dylan, or write a
Lisp to Dylan *compiler*, which would produce correct (but not always
easily human-readable) code. Instead of doing either of those, it would
make more sense to link the Lisp code in to Dylan using the foreign
function interface.

On the other hand, if you are interested in converting a Lisp program to
Dylan so that you can then further modify it, and if you are willing to
put in some manual work, then you want not a compiler, but rather a
*translator* or *converter*, which produces clean, human-readable Dylan
code, but which will need some hand editing. The program *LTD* (for
**L**\ isp **T**\ o **D**\ ylan) is just such a translator.

In the following sections we describe the areas where the translation
can run into trouble.

NIL versus ()
-------------

Lisp has a single object, written as either ``nil`` or ``()``, which
serves the role of three distinct Dylan objects: ``#f`` for false,
``#()`` for the empty list, and ``#"nil"`` for the symbol whose name is
"nil". In Dylan, only ``#f`` counts as false for conditional tests. Our
suggestion is that you edit your Lisp programs and make sure that you
always use ``nil`` for false and ``()`` for the empty list; LTD will
then work smoothly. You should also make sure that you consistently use
the function ``null`` to check for the empty list, and ``not`` to check
for false. If you don't want to follow this advice, LTD provides
:ref:`conversion-options` you can set.

There is a similar problem that ``t`` in Lisp is both a symbol and the
canonical true value, while ``#t`` in Dylan is not a symbol. This does
not cause as much confusion, so LTD ignores the problem.

Single versus multiple namespaces
---------------------------------

In Dylan there is a single namespace associating variable names with
objects of all kinds. For example, the variable name ``$pi``, the
function name ``reverse`` and the class name ``<list>`` are all in this
single namespace. It is possible to subdivide this namespace into
modules, but that is a :ref:`separate issue <packages>`. In Lisp, there is
one name space for variables, another for function names, and a third
for data types. The symbol ``list`` has one value as a function, another
as a type, and is free to be given yet another as a variable.

So there are two problems: (1) doing an analysis to tell which way
``list`` (or ``'list``) is intended to be used, and (2) deciding what to
do when ``list`` is used in what to Dylan is several different ways. LTD
address part of problem (1): it makes choices based on the function to
which an expression is an argument. For example, consider the following
conversions of ``list`` done by LTD:

::

   LISP                         DYLAN
   (coerce x 'list)             as(<list>, x)
   (mapcar 'list args)          map(list, args)
   (list 'list)                 list(#"list")

These are possible because LTD knows that the second argument to
``coerce`` is a type, and the first argument to ``mapcar`` is a funcion.

But there are some cases where conversions like this cannot be done
automatically. Consider ``(setq var 'list)``. This should be translated
as ``var := list`` if the variable is used later in
``(funcall var 1 2 3)``; it should be translated as ``var := <list>`` if
the variable is used in ``(coerce x var)``, and it should be translated
as ``var := #"list"`` in other cases. But in general, deciding where a
variable will be used is undecidable. So LTD does not even attempt to do
non-local analysis. By default, ``'list`` translates as the symbol
``#"list"`` and ``list`` translates as the variable ``list``. This means
that LTD will make mistakes in expressions such as
``(let ((var 'list)) (funcall var x y))`` or
``(let ((list nil)) (list list))``.

Special (dynamic) variables
---------------------------

Lisp has special variables with dynamic scope. Dylan does not have this
feature. However, Open Dylan has provided an extension, ``dynamic-bind``,
that implements variables with dynamic scope. LTD uses ``dynamic-bind`` for
all variables whose name starts and ends with an asterisk.

Optional arguments
------------------

Lisp allows both optional and keyword parameters. Dylan allows only
keyword parameters. It is easy enough to change
``(defun f (x &optional y) ...)`` to the Dylan equivalent of
``(defun f (x &key y) ...)``, and it is even possible to change calls
such as ``(f 1 2)`` to ``(f 1 :y 2)``, assuming that two passes through
the code are allowed. However, it is hard to know if anything needs to
be done to an expression such as ``(apply f l)``, because you don't know
if ``f`` is a function that was defined to take optional arguments. LTD
handles optional arguments in built-in Common Lisp functions, but does
not handle them for user-defined functions; you will need to look
carefully at calls to all functions that take optional arguments. Future
versions of LTD may address this issue.

EVAL and symbol-to-value coercion
---------------------------------

Lisp has an ``eval`` function; Dylan does not. Furthermore, Lisp has
other ways of getting some of the power of ``eval``. For example, in
``(funcall (read) x)``, the user can type any symbol for the result of
the ``read``, and ``funcall`` will coerce that symbol into a function to
be called. This capability is not present in Dylan. (Its absence means
that one does not have to keep all the defined functions in a Dylan
object library, which is a very good thing, but it does make translation
difficult.)

This problem shows up in Lisp expressions such as
``(setq f 'nreverse)``. Whether this should translate as
``f := #"nreverse"`` or ``f := reverse!`` depends on how ``f`` is
subsequently used. LTD takes the approach that symbols will not be
converted to functions, unless they appear in a function argument
position that is known to take only functions. If you meant to refer to
the function, you should write ``(setq f #'nreverse)`` in the source
code; this will get translated as ``f := reverse!`` as expected. If you
write ``(mapcar 'nreverse lists)`` or ``(funcall 'nreverse list)`` then
LTD will convert ``'nreverse`` to ``reverse!``, but it will not do any
more complicated data-flow analysis.

Macros
------

Lisp provides complex user-defined macros that can run arbitrary code
before deciding on the expansion. Dylan also provides a powerful macro
facility, but it is not quite as powerful as Lisp's. Therefore, it is
not feasible to translate each ``defmacro`` from Lisp to Dylan. LTD thus
requires you to write a :ref:`ltd-fn <ltd-fn>` definition for
each macro in your program. Future versions of LTD may automate this,
but we are waiting for the procedural macros to be defined.

Lexical conventions for symbols, etc.
-------------------------------------

Variable names and symbols in Dylan are composed from numbers, digits,
and the characters "``!&*<=>|^$%@_``". They may not start with
"``-+~?/``". If a variable starts with a digit, it must contain two
consecutive letters. LTD alters symbols that do not meet these
conventions: it changes illegal characters to ``%``. It does not check
to see if such changes will result in name clashes.

In addition, note that in Lisp, ``|x|`` and ``|X|`` are two different
symbols, while in Dylan, ``x`` and ``X`` are the same names, although
``#"x"`` and ``#"X"`` are different symbol literals. LTD currently does
not deal with this problem.

Numbers
-------

Dylan has a simpler number type hierarchy (and syntax for number
constants) than Lisp. Dylan does not implement complex numbers (although
it reserves a place for them). LTD prints ``#C(1 2)`` as ``1 + 2 * $i``,
with the assumption that some library defines the constant ``$i`` as
``sqrt(-1)``. Similarly, Dylan does not define ratios, so LTD prints
``1/2`` as ``1 / 2`` within code, but as ``0.5`` within a literal. Dylan
does not provide the transcendental functions ``log``, ``sin``, etc. in
the core language, but rather puts them in a library, which has not yet
been specified. LTD assumes that the function names are the same in Lisp
and Dylan. Perhaps most importantly, Dylan is not yet explicit about how
bignums are used, and whether small integer arithmetic overflows into
bignums. LTD does not deal with this problem, but you may have to.

.. _packages:

Packages versus modules
-----------------------

Lisp packages perform much of the same function as Dylan modules, but
packages are more dynamic. LTD does not attempt to deal with functions
that access and alter packages at run time. Future versions may.

One particular confusion is with the keyword package. In Lisp, the
keyword package is just another package, although with the restriction
that all symbols in it are constants that evaluate to themselves. But
all symbols, keyword or not, are first class objects. In Dylan, there is
only one namespace of symbols, and several namespaces (modules) for
names, which get mapped into a single namespace by importing assertions.
The syntax for a symbol is either ``#"symbol"`` or ``symbol:``, both are
equivalent. The latter syntax makes Dylan symbols look like Lisp
keywords (and they are used the same way in parameter lists), but
otherwise there is no symbol/keyword distinction in Dylan. Therefore,
the safest thing is to convert Lisp's ``'(sym :key)`` to
``#(#"user:sym" #"key")``, although LTD also gives you the option of
converting this to ``#(#"sym" #"key")`` if you know there will be no
package/name conflicts.

CLOS method combination
-----------------------

Lisp allows for a lot of latitude in the ways methods are defined and
combined. One can specify the order on which parameters are tested for
dispatch. One can define ``:before``, ``:after`` and ``:around``
methods. One can combine methods in customized ways. Dylan does not
allow any of this, or rather, it forces the programmer to be responsible
for it using ``call-next-method``. LTD does not attempt to deal with
this; it just flags method qualifiers with a warning.

Functions with different names
------------------------------

Some functions have substantially the same definition in Lisp and Dylan,
but just have different names. For example, ``cons`` in Lisp is called
``pair`` in Dylan. We provide a table of translations of this kind in
the file `tables.lisp <https://github.com/dylan-lang/lisp-to-dylan/blob/master/code/tables.lisp>`__. For example, the line

.. code:: common-lisp

   (ltd-fn cons pair)

defines ``pair`` as the translation for the function ``cons``. You may
add new entries to this table by adding lines of the form

.. code:: common-lisp

   (cvt-fn lisp-function dylan-function)

Functions with similar (but different) semantics
------------------------------------------------

Some functions have similar semantics, but vary in minor detail. For
example, the Lisp function ``length`` performs much the same purpose as
the Dylan function ``size``, but there are differences. For one thing,
``size`` can give you the size of a hashtable, while ``length`` will
signal an error when applied to a hashtable. LTD substitutes ``size``
for ``length`` anyways, on the grounds that very few programs rely on
signalling and catching an error like this. If your program does rely on
such behavior, you'll need to deal with it manually. LTD always
translates ``length`` to ``size``, via ``(ltd-fn length size)``.

Another example is the function ``+``, which is limited to two arguments
in Dylan, but takes any number in Lisp. It is easy enough to translate
``(+ a b c)`` to ``a + b + c``, but what about ``(apply #'+ args)``?
Currently, LTD translates this use of ``#'+`` to ``+``, which won't be
right if ``args`` can be other than a list of length two.

The biggest example of similar functions is in the sequence functions.
For example, ``remove`` means much the same thing in Lisp and Dylan,
except that Lisp's ``remove`` takes more keyword arguments. LTD has two
approaches to handling this. First, some calls can be converted to the
corresponding Dylan function. For example,

.. code:: common-lisp

   (remove item seq :test-not #'my= :key #'slot1 :start n)

will be translated by LTD to

.. code:: dylan

   remove!(item, copy-sequence(seq, start: n),
           test: method (x, y)
                   (complement(my=))(x, slot1(y));
                 end)

But this does not work so well in ``(apply #'remove args)``, because we
don't know what keywords will be in ``args``. To handle that, we have
defined a library of functions that duplicate the Common Lisp sequence
functions (except that they don't support the deprecated ``:test-not``
keyword). These functions have names like ``cl-remove``.

Another problem is ``error``, and the other condition-signalling
functions. In Lisp, they take an argument that can be either a format
string, an instance of a condition class, or the name of a condition
class. The corresponding Dylan functions support the first two options,
but not the third. LTD correctly translates ``(signal 'error)`` to
``(signal (make <error>))``, but it translates ``(apply #'signal args)`` to
``(apply signal args)``, which would be incorrect if a condition class
name is provided. A final problem is ``equalp``, which has no direct
equivalent in Dylan. LTD maps it to ``=``, which could cause problems.

Functions that do not appear in Dylan
-------------------------------------

Common Lisp has several hundred functions that do not appear in Dylan at
all. Some of these, like ``1+`` and ``cdar`` are easy enough to replace
with a simple inline Dylan expression. LTD does that with the following
expressions:

.. code:: common-lisp

   (ltd-fn (1+ x)        `(+ ,x 1))
   (ltd-fn (cdar x)      `(tail (head ,x)))

Other functions, such as ``rassoc``, could be provided for in the same
way, or by a simple Lisp compatibility library written in Dylan. They
will be added on an as-needed basis. Others, like ``eval`` or ``read``
would require an extensive library, which remains to be written.

Some functions have a half-hearted implementation in LTD. For example,
LTD handles some of the simpler syntactic clauses in the ``loop`` macro,
but not all of them (there is an option to macroexpand complex
``loop`` callss, and then convert the result). Similarly, LTD handles the
easy format directives in format strings, but not the complex ones. (One
could try to "macroexpand" the format string using the ``formatter``
macro in Common Lisp. LTD does not do this because the results often are
still not usable.)
