Symbolic Testing
================

Writing Symbolic Tests
----------------------

The whole-program symbolic testing module of Gillian-JS extends JavaScript with a mechanism for declaring symbolic variables and performing first-order reasoning on them.

Declaring Symbolic Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One can declare untyped symbolic variables, symbolic booleans, symbolic strings, and symbolic numbers as follows:

.. code-block:: js

   var x = symb(); // Untyped symbolic variable

   var b = symb_number(); // Symbolic boolean
   var n = symb_number(); // Symbolic number
   var s = symb_string(); // Symbolic string

Assumptions and Assertions
^^^^^^^^^^^^^^^^^^^^^^^^^^

Gillian-JS provides assumption and assertion constructs, as follows:

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

Here is an example of a symbolic test using assumptions and assertions:

.. code-block:: js

   // Create two symbolic numbers
   var n1 = symb_number(), n2 = symb_number();

   // Assume that they are non-negative and different
   Assume((0 <= n1) and (0 <= n2) and (not (n1 = n2)));

   // Perform some calculations
   var res = f(n1, n2);

   // Assert, for example, that n1 and n2 are not greater than the result
   Assert((n1 <= res) and (n2 <= res));

The above example is already in the repository, with ``f`` instantiated to ``n1 + n2``. You can run the example, starting from the ``Gillian`` folder, as follows:

.. code-block:: text

   dune exec -- gillian-js wpst Gillian-JS/Examples/Cosette/simple_example.js

Since the assertion in the end does hold, the execution exits successfully. If, instead, you change ``n1 + n2`` to ``n1 * n2`` and re-run the example, you will be faced with the following error message and counterexample:

.. code-block:: text

   Compilation time: 0.018218s
   Total time (Compilation + Symbolic testing): 3.528332s
   Errors occurred!
   Here's a counterexample: [ (#gen__0: 0.), (#gen__1: 1.) ]
   Here's an example of final error state: FAILURE TERMINATION: Procedure main, Command 77
					   Errors: EPure(((#gen__0 <=# (#gen__0 * #gen__1)) /\ (#gen__1 <=# (#gen__0 * #gen__1))))
   // the rest of the state omitted

which means that the assertion does not hold if ``n1 = 0`` and ``n2 = 1``.

Semantics of Operators
^^^^^^^^^^^^^^^^^^^^^^

Importantly, the semantics of all of the operators is deliberately **not** as in JavaScript. For example, comparison and numeric operators do not perform any implicit type coercions. If you want to use JavaScript comparison/numeric operators, say ``<=``, you can proceed as follows:

.. code-block:: js

   var res_leq_n1 = n1 <= res;

   Assert(n1_leq_res);
