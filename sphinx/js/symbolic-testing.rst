Symbolic Testing
================

Writing Symbolic Tests
----------------------

The whole-program symbolic testing module of Gillian-JS (codenamed Cosette) extends JavaScript with a mechanism for declaring symbolic variables and performing first-order reasoning on them.

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

Symbolic Testing of Buckets.js
------------------------------

We symbolically test Buckets.js, a real-world JavaScript data-structure library, with the goal of obtaining 100% line coverage. The results are presented in the table below, with each row containing:

- The name of the folder being tested, which also indicates the data structure in question
- The number of tests required for 100% line coverage
- The total number of GIL commands executed by running these tests
- The total testing time (in seconds)

Testing Results
^^^^^^^^^^^^^^^

=================== ====== ============== ==========
Data Structure      Tests  GIL Commands   Time (s)
=================== ====== ============== ==========
**arrays**               9        330,147      2.678
**bag**                  7      1,343,393      5.064
**bstree**              11      3,751,092     12.507
**dictionary**           7        401,575      1.833
**heap**                 4      1,492,204      3.411
**linkedlist**           9        588,714      4.141
**multidictionary**      6      1,106,650      3.803
**queue**                6        407,106      2.140
**priorityqueue**        5      2,312,226      4.121
**set**                  6      2,178,222      4.458
**stack**                4        306,449      1.625
**Total**           **74** **14,217,778** **45.781**
=================== ====== ============== ==========

The results are 1.3% slower and the number of executed GIL commands is 0.1% greater than reported in the submitted version---we will update accordingly. This is due to minor changes to the JS-2-GIL compiler and the JS symbolic engine.

Reproducing the Results
^^^^^^^^^^^^^^^^^^^^^^^

Starting from the ``Gillian`` folder, execute the following:

.. code-block:: bash

   dune build
   make js-init-env
   cd Gillian-JS/environment

Then, to reproduce the results for a specific folder from the first column of the above table, execute the following:

.. code-block:: bash

   ./testCosetteFolder.sh Examples/Cosette/Buckets/<folder>

In order to obtain the number of executed commands, append the ``count`` parameter to the last command. Therefore, for example, the command to run the tests for the ``queue`` data structure and obtain the number of executed commands is

.. code-block:: bash

   ./testCosetteFolder.sh Examples/Cosette/Buckets/queue count

**Note**: The times obtained when counting executed commands will be slower, due to the fact that the tests will be run in single-thread mode.

Detailed Per-Folder Breakdown: Buckets.js
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========
**arrays**       1      2      3      4      5      6      7      8      9      **Total**
================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========
**Time (s)**      0.259  0.288  0.264  0.264  0.259  0.285  0.258  0.569  0.232     2.678
**GIL Commands** 33,903 34,675 34,896 42,866 30,483 55,210 34,765 39,532 23,817   330,147
================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========

================ ====== ====== ======= ======= ======= ======= ======= =========
**bag**          1      2      3       4       5       6       7       **Total**
================ ====== ====== ======= ======= ======= ======= ======= =========
**Time (s)**      0.501  0.453   0.963   0.641   0.577   0.923   1.006     5.064
**GIL Commands** 99,395 60,935 301,687 208,336 158,635 200,411 313,994 1,343,393
================ ====== ====== ======= ======= ======= ======= ======= =========

================ ======= ========== ====== ======= ======= ======= ======= ======= ======= ======= ======= =========
**bstree**       1       2          3      4       5       6       7       8       9       10      11      **Total**
================ ======= ========== ====== ======= ======= ======= ======= ======= ======= ======= ======= =========
**Time (s)**       0.746      2.540  0.684   0.763   1.015   1.028   1.013   1.131   0.762   0.762   2.063    12.507
**GIL Commands** 123,798  1,254,635 72,637 169,155 192,683 192,683 191,633 390,919 100,266 177,362 885,321 3,751,092
================ ======= ========== ====== ======= ======= ======= ======= ======= ======= ======= ======= =========

================ ====== ====== ====== ====== ====== ====== ====== =========
**dictionary**   1      2      3      4      5      6      7      **Total**
================ ====== ====== ====== ====== ====== ====== ====== =========
**Time (s)**      0.275  0.238  0.217  0.352  0.229  0.217  0.305     1.833
**GIL Commands** 61,161 54,140 44,569 55,033 55,914 41,904 88,854   401,575
================ ====== ====== ====== ====== ====== ====== ====== =========

================ ======= ======= ======= ======= =========
**heap**         1       2       3       4       **Total**
================ ======= ======= ======= ======= =========
**Time (s)**       0.517   1.487   0.629   0.778     3.411
**GIL Commands** 135,140 804,659 169,522 382,883 1,492,204
================ ======= ======= ======= ======= =========

================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========
**linkedlist**   1      2      3      4      5      6      7      8      9      **Total**
================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========
**Time (s)**      0.648  0.577  0.603  0.438  0.293  0.295  0.257  0.718  0.312     4.141
**GIL Commands** 43,209 57,458 97,728 82,345 63,645 66,093 30,794 97,225 50,217   588,714
================ ====== ====== ====== ====== ====== ====== ====== ====== ====== =========

=================== ======= ======= ======= ======= ======= ======= =========
**multidictionary** 1       2       3       4       5       6       **Total**
=================== ======= ======= ======= ======= ======= ======= =========
**Time (s)**          0.504   0.813   0.566   0.579   0.678   0.663     3.803
**GIL Commands**    130,145 312,351 166,638 145,627 158,934 192,955 1,106,650
=================== ======= ======= ======= ======= ======= ======= =========

================ ====== ====== ====== ====== ====== ======= =========
**queue**        1      2      3      4      5      6       **Total**
================ ====== ====== ====== ====== ====== ======= =========
**Time (s)**      0.332  0.345  0.345  0.249  0.403   0.466     2.140
**GIL Commands** 71,514 69,962 45,067 36,767 62,624 121,172   407,106
================ ====== ====== ====== ====== ====== ======= =========

================= ======= ======= ======= ======= ========= =========
**priorityqueue** 1       2       3       4       5         **Total**
================= ======= ======= ======= ======= ========= =========
**Time (s)**        0.757   0.731   0.449   0.993    1.191      4.121
**GIL Commands**  399,730 287,433 121,329 450,539 1,053,195 2,312,226
================= ======= ======= ======= ======= ========= =========

================ ====== ======= ========= ======= ====== ======= =========
**set**          1      2       3         4       5      6       **Total**
================ ====== ======= ========= ======= ====== ======= =========
**Time (s)**      0.386   0.679     1.743   0.622  0.292   0.736     4.458
**GIL Commands** 78,959 242,304 1,265,278 232,776 66,700 292,205 2,178,222
================ ====== ======= ========= ======= ====== ======= =========

================ ====== ====== ====== ======= =========
**stack**        1      2      3      4       **Total**
================ ====== ====== ====== ======= =========
**Time (s)**      0.343  0.331  0.331   0.620     1.625
**GIL Commands** 52,233 44,958 55,097 154,161   306,449
================ ====== ====== ====== ======= =========

Reproducing the Buckets.js Bugs found by :doc:`../publications/cosette` and :doc:`../publications/javert-2`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starting from the ``Gillian`` folder, execute the following:

.. code-block:: bash

   dune build
   make js-init-env
   cd Gillian-JS/environment

:doc:`../publications/cosette` Multi-Dictionary Bug
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to reproduce the multi-dictionary bug reported by :doc:`../publications/cosette`, execute:

.. code-block:: bash

   ./testCosette.sh Examples/Cosette/Buckets/multidictionary/bug/multidictionary_bug.js

You will obtain a failing model

.. code-block:: text

   Assert failed with argument False.
   Failing Model:
     [ (#x1: #x2) ]

The bug is caused by the library wrongly treating the case in which we try to remove a key-value pair for a key with no associated values. The code of the test is as follows:

.. code-block:: js

   var dict = new buckets.MultiDictionary()

   var s = symb_string(s);
   var x1 = symb_number(x1);
   var x2 = symb_number(x2);

   dict.set(s, x1);
   dict.set(s, x2);

   dict.remove(s, x1);
   var res = dict.remove(s, x2);
   Assert(((not (x1 = x2)) and (res = true)) or ((x1 = x2) and (res = false)));

The test puts two symbolic numbers, ``x1`` and ``x2`` for the same symbolic key ``s`` into an empty multidictionary, then removes ``x1``, and then removes ``x2`` and registers the value returned by ``remove``. Then, it asserts that that value was ``true`` if the two keys were different, and ``false`` if the two keys were the same. What the failing model says is that, when the two keys are equal, the library, in fact, throws a native JavaScript error (indicated by the argument ``False`` of the failed assert).

:doc:`../publications/javert-2` Linked-List Bugs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to reproduce the linked-list bugs reported by :doc:`../publications/javert-2`, execute:

.. code-block:: bash

   ./testCosette.sh Examples/Cosette/Buckets/linkedlist/bug/linkedlist_bug_1.js
   ./testCosette.sh Examples/Cosette/Buckets/linkedlist/bug/linkedlist_bug_2.js
   ./testCosette.sh Examples/Cosette/Buckets/linkedlist/bug/linkedlist_bug_3.js

All of the bugs are causes by the library treating non-integer indexing incorrectly; we explain the bug found by the first test in detail, the remaining two are analogous. For the first test, the failing model is as follows:

.. code-block:: text

   Assert failed with argument
     ((((#x3 == 0) /\ (#x2 == #x1)) \/
       ((#x3 == 1) /\ (#x2 == #x2))) \/
       (((! (#x3 == 0)) /\ (! (#x3 == 1))) /\ (#x2 == undefined))).
   Failing Model:
     [ (#x2: 4), (#x3: 0.5), (#x1: 3) ]

The code of the test is as follows:

.. code-block:: js

   var list = new buckets.LinkedList()

   var x1 = symb_number(x1);
   var x2 = symb_number(x2);
   var x3 = symb_number(x3);

   list.add(x1)
   list.add(x2)

   var res = list.elementAtIndex(x3);
   Assert( (((x3 = 0) and (res = x1)) or
            ((x3 = 1) and (res = x2))) or
            (((not (x3 = 0)) and (not (x3 = 1))) and (res = undefined)) );

The test inserts two symbolic numbers, ``x1`` and ``x2``, into an empty linked list, and then indexes the list with a third symbolic number, ``x3``. The expected outcome is that: if ``x3 = 0``, the indexing returns ``x1``; if ``x3 = 1``, the indexing returns ``x2``; and, otherwise, the indexing returns ``undefined``. The failing model, however, says that if ``x3 = 0.5``, the indexing will also return ``x2``.