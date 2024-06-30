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
   ASSUME(-128 <= a );ASSUME( a <= 127);
   char c_a = (char) a;
   char str_a[] = { c_a, '\0' };

Assumptions and Assertions
^^^^^^^^^^^^^^^^^^^^^^^^^^

For any ``C`` boolean expression ``e``, it is possible to write:

.. code-block:: c

   ASSUME(e);
   ASSERT(e);

The expression ``e`` is compiled to commands (since C expressions can have side-effects but GIL expressions cannot).
Then, a final GIL expression will contain a "serialised C integer" (since ``C`` booleans are actually integers that have value 0 or 1).
A serialized C integer is a list of the form ``{{ "int", x }}`` where ``x`` is a GIL number.
``ASSUME`` will call the internal GIL ``assume`` in the form ``assume(e = {{ "int", 1 }})`` which means "check that the obtained boolean expression is True", otherwise, cut the branch.
``ASSERT`` does the same as assume but calls the internal GIL ``assert`` instead.

As opposed to `Gillian-JS <../JavaScript/stest#assumptions-and-assertions>`_, we use C expressions directly, and not custom expressions. This benefits is that one does not have to learn a new syntax for writing tests. However, this causes the execution to branch a lot for. ``ASSUME`` will then cut any branch that we do not want. In Gillian-JS, given the complex control flow of JavaScript, there is a lot more branching happening, which can become quite difficult to handle. Also, in JavaScript, the very complex semantics of expressions can lead to behaviours that are not desired by the used, and providing a simpler expression syntax is more straightforward.

Symbolic Testing of Collections-C
---------------------------------

Fixes
^^^^^

Symbolically testing Collections-C let to the following bug-fixing pull requests. They fix previously unknown bugs and usage of undefined behaviours:

- `Fix buffer overflow <https://github.com/srdja/Collections-C/pull/119>`_ (bug)
- `Remove the usage of cc_comp_ptr <https://github.com/srdja/Collections-C/pull/122>`_ (undefined behaviour)
- `Test coincidentally passing while they should not <https://github.com/srdja/Collections-C/pull/123>`_ (bugs and undefined behaviours)
- `Fix overallocation <https://github.com/srdja/Collections-C/pull/125>`_ (bug)
- `Fix hashing function <https://github.com/srdja/Collections-C/pull/126>`_ (performance-reducing bug)

Reproducing the Results
^^^^^^^^^^^^^^^^^^^^^^^

For license reason, we do not include the Collections-C code in the Gillian repository.
There is an `external repository <https://github.com/GillianPlatform/collections-c-for-gillian>`_ that contains the Collections-C code adapted to testing in Gillian-C and Klee.

In order to clone it, simply run, from the Gillian folder:

.. code-block:: bash

   cd ..
   git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collection-
   cd Gillian

There are two ways of launching the tests:

- Using the ``bulk-wpst`` command of Gillian-C which has a nicer output (using Rely), but cannot run the tests in parallel.
- Using a bash script that will call ``gillian-c wpst`` as many times as there are files to test, but supports parallel mode (this is the one we used for measures). (NOTE: since then, we removed the option for parallel mode, and plan of having a better implementation in the future)

Using bulk-wpst
"""""""""""""""

From the Gillian folder run:

.. code-block:: bash

   dune exec -- gillian-c bulk-wpst ../collections-c/for-gillian

You will see every test suites executing one by one. Two tests will fail, this is intended. They represent two of the bugs we've found and are explained `here <#bug-tests>`_.

Using the bash script
"""""""""""""""""""""

From the Gillian folder, for each folder you want to test, use:

.. code-block:: bash

   Gillian-C/scripts/testFolder.sh ../collections-c/for-gillian/folder

For example, to run the test suite related to singly-linked lists, run:

.. code-block:: bash

   Gillian-C/scripts/testFolder.sh ../collections-c/for-gillian/slist

The ``array_test_remove.c`` buffer overflow bug
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This test corresponds to this pull request: `Fix buffer overflow <https://github.com/srdja/Collections-C/pull/119>`_.
It is particularly interesting: the original test suite did not catch it. We thought that a concrete test with the right values would catch it, but it didn't. The reason is that it overflowed but did not fail. It is therefore a *security issue*. However, our symbolic memory model cannot overflow, and the bug was caught.

The ``list_test_zipIterAdd.c`` flawed test
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This test is also interesting but for different reasons. The code it is testing (the ``list_zip_iter_add`` function) does not contain any bug.
However, the test itself did contain a bug but still passed. Here is why:

The test added two elements (``"h"`` and ``"i"``) in two separate lists at the index 2. It then tested that the elements actually appeared at the second index of their respective lists, in the following way:

.. code-block:: c

   list_index_of(list1, "h", zero_if_ptr_eq, &index);
   CHECK_EQUAL_C_INT(2, index);
   
   list_index_of(list1, "i", zero_if_ptr_eq, &index);
   CHECK_EQUAL_C_INT(2, index);

However, note that both tests are executed on ``list1``! What happened then is that ``list_index_of`` was not finding ``"i"`` in ``list1`` because it wasn't there, and therefore did not modify ``index``. Since the first check was correct, the value of ``index`` was still ``2`` and the test passed anyway.

Our symbolic tests however, use symbolic 1-character strings, and assume **the bare minimum about the input values** to make them pass, in order to explore as many possible paths as possible.

Here, we replaced every one-character strings ``"X"`` with one-character symbolic string ``str_X``. For the test to pass, it should be *enough* for ``str_h`` to be different from every element in ``list1`` and for ``str_i`` to be different from every element in ``list2``. And this is exactly what we assumed. However, we never assume that ``str_i`` has to be different from every element in ``list1`` because it is not necessary for the test to pass.

However, here, the equality between every element of ``list1`` and ``str_i`` is tested. There is no indication as to the result of this test, so the execution branches. Therefore, there is a path created where ``list_index_of(list1, str_i, zero_if_ptr_eq, &index)`` will assign ``0`` to index, and the test will fail.

This shows how symbolic testing helps writing *more robust* tests.
