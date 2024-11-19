Symbolic Testing
================

Writing Symbolic Tests
----------------------

The whole-program symbolic testing module of Gillian-JS extends JavaScript with a mechanism for declaring symbolic variables and performing first-order reasoning on them.

Declaring Symbolic Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One can declare untyped symbolic variables, symbolic booleans, symbolic strings, and symbolic numbers as follows:

.. code-block:: js

   var x = symb(x); // Untyped symbolic variable

   var b = symb_number(b); // Symbolic boolean
   var n = symb_number(n); // Symbolic number
   var s = symb_string(s); // Symbolic string

The single parameters provided to these functions indicate the name of the created symbol, or *logical variable*, that Cosette will further use in the reaasoning. Normally, we choose these to coincide with the JavaScript variables in which they are stored so that the outputs of the analysis are more readable.

Assumptions and Assertions
^^^^^^^^^^^^^^^^^^^^^^^^^^

Cosette provides a mechanism for reasoning about the symbols, in the form of assumptions and assertions, as follows:

.. code-block:: js

   Assume(B); // Assume that the boolean expression B holds
   Assert(B); // Assert that the boolean expression B holds

The grammar of boolean expressions (``B``) and expressions (``E``) is (approximately) as follows:

.. code-block:: text

   B ::=
     | x                         // (Boolean) variables
     | E = E                     // Equality
     | E < E | E <= E            // Comparison
     | not B | B and B | B or B  // Logical operators
   
   E ::=
     | c                           // Constants
     | x                           // Variables
     | E + E | E - E | ...         // Numeric operators
     | E ++ E | s-len E | s-nth E  // String concat, length, and n-th

Here is the example of a symbolic test using assumptions and assertions:

.. code-block:: js

   // Create two symbolic numbers
   var n1 = symb_number(n1), n2 = symb_number(n2);

   // Assume that they are non-negative and different
   Assume((0 <= n1) and (0 <= n2) and (not (n1 = n2)));

   // Perform some calculations
   var res = f(n1, n2);

   // Assert, for example, that n1 and n2 are not greater than the result
   Assert((n1 <= res) and (n2 <= res));

This example is already in the repository (with ``f`` instantiated to ``n1 + n2``), and you can run it, starting from the ``Gillian`` folder, as follows:

.. code-block:: bash

   dune build
   make js-init-env
   cd JaVerT/environment
   dune exec -- gillian-js wpst Examples/Cosette/simple_example.js -s

Since the assertion in the end does hold, there will be no output from Cosette, meaning that the test has passed. If however, you change ``n1 + n2`` to ``n1 * n2`` and re-run the example, you will be faced with the following error message and counter-model:

.. code-block:: text

   Assert failed with argument ((#n1 <=# (#n1 * #n2)) /\ (#n2 <=# (#n1 * #n2))).
   Failing Model:
     [ (#n2: 1), (#n1: 0) ]

which means that the assertion does not hold if ``n1 = 0`` and ``n2 = 1``. Here, variables prefixed by ``#`` denote logical variables; in this case, the parameters given to the ``symb_number`` function.

Semantics of Operators
^^^^^^^^^^^^^^^^^^^^^^

Importantly, the semantics of all of the operators is deliberately **NOT** as in JavaScript. For example, comparison and numeric operators do not perform any implicit type coercions. If you want to use JavaScript comparison/numeric operators, say ``<=``, you can proceed as follows:

.. code-block:: js

   var res_leq_n1 = n1 <= res;

   Assert(n1_leq_res);

Typing and Objects in Symbolic Tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since we do not (yet) perform lazy initialisation in symbolic execution, errors may occur if you attempt to reason about symbolic objects or untyped symbolic variables. This can be prevented as follows:

.. code-block:: js

   var x = symb(x);
   Assume(not (typeOf x = Obj));

where ``typeOf`` is the built-in GIL typing operator and ``Obj`` is the built-in GIL object type. In this way, it is guaranteed that ``x`` is not an object (but may still equal ``null``).
