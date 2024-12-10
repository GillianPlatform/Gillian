Symbolic Testing
================

Writing Symbolic Tests
----------------------

The whole-program symbolic testing aspect of Gillian-C works in a close way to `Klee <https://klee.github.io>`_'s.

Declaring Symbolic Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to declare symbolic variables, we hijack the ``__builtin_annot_intval`` function of CompCert. An symbolic integer is declared in the following way:

.. code-block:: c

   int a = __builtin_annot_intval("symb_int", a)

As Gillian-C is in its very early stage, it is impossible to declare variables that have undefined size in memory.

It is possible to declare a symbolic string of fixed size by declaring all of its components one by one. For example, for a string that contains one character:

.. code-block:: c

   int a = __builtin_annot_intval("symb_int", a);
   ASSUME(-128 <= a); ASSUME(a <= 127);
   char c_a = (char) a;
   char str_a[] = { c_a, '\0' };

Assumptions and Assertions
^^^^^^^^^^^^^^^^^^^^^^^^^^

For any C boolean expression ``e``, it is possible to write:

.. code-block:: c

   ASSUME(e);
   ASSERT(e);

The expression ``e`` is compiled to commands (since C expressions can have side-effects but GIL expressions cannot).
Then, a final GIL expression will contain a "serialised C integer" (since ``C`` booleans are actually integers that have value 0 or 1).
A serialized C integer is a list of the form ``{{ "int", x }}`` where ``x`` is a GIL number.
``ASSUME`` will call the internal GIL ``assume`` in the form ``assume(e = {{ "int", 1 }})`` which means "check that the obtained boolean expression is True", otherwise, cut the branch.
``ASSERT`` does the same as assume but calls the internal GIL ``assert`` instead.

As opposed to Gillian-JS, we use C expressions directly, and not custom expressions. This benefits is that one does not have to learn a new syntax for writing tests. However, this causes the execution to branch a lot for. ``ASSUME`` will then cut any branch that we do not want. In Gillian-JS, given the complex control flow of JavaScript, there is a lot more branching happening, which can become quite difficult to handle. Also, in JavaScript, the very complex semantics of expressions can lead to behaviours that are not desired by the used, and providing a simpler expression syntax is more straightforward.
