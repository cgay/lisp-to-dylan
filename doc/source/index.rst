===============================
Converting Common Lisp to Dylan
===============================


.. note:: This documentation is based on the `original docs
          <https://norvig.com/ltd/doc/ltd.html>`__ written by Peter Norvig, but has been
          converted from HTML to Restructured Text.  Bit rot and typos have been fixed
          and new content will be added here as LTD evolves.

*[Historical note: I recently found this 8-year old code hidden in the
attic. Some readers might find it useful as an example of (1)* `40,000
lines of code in the Dylan language (almost) <https://github.com/dylan-lang/lisp-to-dylan/tree/master/test/ref>`__\ *, or (2)*
:ref:`the powerful pretty-printing <pretty-printing>`
*capabilities in Common Lisp. Not many people appreciate that you can
produce nicely formatted output so easily -- here I produce Dylan code
with nice indentation, to any specified column width, just by converting
Lisp to a parse-tree syntax that is equivalent to Dylan, and then
writing pretty-print directives for the parse-trees. All the rest is
handled automatically.*

As I told swm [#swm]_, haahr [#haahr]_, and jrb [#jrb]_, finding this old code made me feel like
Dorothy waking up from her dream: "And you were in it, and you, and you.
Some of it was beautiful, but most of it ..." well, most of it didn't
turn out the way we hoped back in 1994-95. -Peter Norvig]

----

Overview
********

This document is designed to help you convert code from Common Lisp to
Dylan. The first part of the document lists problems to look out for;
the second part documents a Common Lisp program called *LTD* (for
**L**\ isp **T**\ o **D**\ ylan) that helps to automate the conversion.

.. toctree::

   Pitfalls <problems>
   Conversion Tool <tool>


`Peter Norvig <http://norvig.com/>`__


.. rubric:: Footnotes

.. [#swm] Scott McKay

.. [#haahr] Paul Haahr

.. [#jrb] Jonathan Bachrach
