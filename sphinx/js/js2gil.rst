JS-2-GIL and Test262
====================

Coverage of JS-2-GIL
--------------------

JS-2-GIL is a compiler from ECMAScript ES5 Strict to GIL. JS-2-GIL has broad coverage. It supports the entire core (chapters 8-14), with the exception of the ``RegExp`` literal, as well as all of the JavaScript built-in libraries except the following:

* ``Date``, ``RegExp``, ``JSON``
* ``Number.prototype.toFixed``
* ``String.fromCharCode``
* ``charCodeAt``, ``localeCompare``, ``match``, ``replace``, ``search``, ``split``, ``trim``, ``toLocaleLowerCase``, ``toLocaleUpperCase``, ``toLowerCase``, ``toUpperCase`` (in ``String.prototype``)
* ``decodeURI``, ``decodeURIComponent``, ``encodeURI``, ``encodeURIComponent``, ``parseInt``, ``parseFloat`` (in the global object)

Additionally, indirect ``eval`` is not supported, as it is meant to be executed as non-strict code. Furthermore, all ``Function`` constructor code runs as strict-mode code.

Correctness of JS-2-GIL
-----------------------

The JS-2-GIL compiler can be split into two compilers: JS-2-JSIL, which compiles JavaScript to JSIL, the intermediate representation that we have used in :doc:`../publications/javert` / :doc:`../publications/cosette` / :doc:`../publications/javert-2`; and JSIL-2-GIL, the compiler from JSIL to GIL, the intermediate representation of Gillian.

Previously, we have tested JS-2-JSIL against Test262, the JavaScript official test suite (specifically, `this commit from 2016/05/30 <https://github.com/tc39/test262/commit/91d06f>`_). As Test262 commit targets ES6, we had to identify the subset of tests that are appropriate for JS-2-JSIL, as explained in detail in :doc:`../publications/javert`. We obtained 8797 applicable tests, of which JS-2-JSIL passes 100%.

We have initially tested JS-2-GIL successfully on the same 8797 tests and reported this in the submitted version of the paper. However, these tests were not systematically categorised and we were not able to automate the testing process to our satisfaction using the bulk testing mechanism of Gillian. For this reason, we have chosen to work with the latest version of Test262, forked `here <https://github.com/GillianPlatform/javert-test262>`_, where each test comes with a precise description of its intended outcome. For example, a test that is supposed to fail at parsing time with a JavaScript native syntax error will contain the following in its header:

.. code-block:: text

   negative:
     phase: parse
     type: SyntaxError

We re-filter Test262 to find the applicable tests and run them using JS-2-GIL and the concrete semantics of Gillian-JS. The summary results are presented in the table below and will be included in the final version of the paper. A more detailed, per-folder breakdown is available further below.

============================ ======== ========= =====
/                            Language Built-ins Total
============================ ======== ========= =====
**Total tests**                  3202      7860 11062
Non-strict tests                  583       136   719
**Strict tests**                 2619      7724 10343
Non-strict features                41        50    91
For unimplemented features         17      1182  1199
Using unimplemented features       23        12    35
ES6+                                3         2     5
**Applicable**                   2535      6748  9013
Passing                          2530      6745  9005
Failing                             5         3     8
============================ ======== ========= =====

Explanation: Table Columns
^^^^^^^^^^^^^^^^^^^^^^^^^^

- **Language**: tests for the core language (chapters 8-14 of the `ECMAScript ES5 standard <https://www.ecma-international.org/ecma-262/5.1/>`_).
- **Built-ins**: tests for the built-in libraries (chapter 15 of the `ECMAScript ES5 standard <https://www.ecma-international.org/ecma-262/5.1/>`_).
- **Total**: all tests.

Explanation: Table Rows
^^^^^^^^^^^^^^^^^^^^^^^

- **Total tests**: all tests.
- **Non-strict tests** (filtered out): tests that should be run only in non-strict mode. They contain the ``flags: [noStrict]`` directive and are filtered out automatically by our bulk testing mechanism.
- **Strict tests**: tests that should be run in strict mode.
- **Non-strict features** (filtered out): tests that combine strict and non-strict mode, using either indirect eval or the non-strict ``Function`` constructor (91 tests, list available in the ``non_strict_tests`` variable of ``test262_filtering.ml`` `[link] <https://github.com/GillianPlatform/Gillian/blob/PLDI20/Gillian-JS/lib/Test262/Test262_filtering.ml>`_).
- **For unimplemented features** (filtered out): tests that test unimplemented features, such as the ``JSON`` library (1205 tests, list available in the ``tests_for_unimplemented_features`` variable of ``test262_filtering.ml`` `[link] <https://github.com/GillianPlatform/Gillian/blob/PLDI20/Gillian-JS/lib/Test262/Test262_filtering.ml>`_). Note that the structural tests for some of these features still pass, as we have the appropriate stubs in the initial heap.
- **Using unimplemented features** (filtered out): tests that use unimplemented features to test behaviours of implemented features, with the most prominent examples being the ``Date`` constructor and ``RegExp`` literals (29 tests, list available in the ``tests_using_unimplemented_features`` variable of ``test262_filtering.ml`` `[link] <https://github.com/GillianPlatform/Gillian/blob/PLDI20/Gillian-JS/lib/Test262/Test262_filtering.ml>`_).
- **ES6+** (filtered out): tests that use behaviours beyond ES5 (5 tests, list available in the ``es6_tests`` variable of ``test262_filtering.ml`` `[link] <https://github.com/GillianPlatform/Gillian/blob/PLDI20/Gillian-JS/lib/Test262/Test262_filtering.ml>`_). For example, two language tests test for the ES6 completion of the ``if`` statement (returns ``undefined`` instead of the ES5 ``empty``), one language test uses a function declaration in statement position (disallowed in ES5), and two built-in tests require a specific ordering of object keys (implementation-dependent in ES5).
- **Applicable**: the number of tests applicable to JS-2-GIL.
- **Passing**: the number of tests passing.
- **Failing**: the number of tests failing.

Explanation: Failing Tests
^^^^^^^^^^^^^^^^^^^^^^^^^^

The following eight tests

- ``test262/test/language/line-terminators/7.3-6.js``
- ``test262/test/language/line-terminators/7.3-5.js``
- ``test262/test/language/line-terminators/7.3-15.js``
- ``test262/test/language/line-terminators/invalid-string-cr.js``
- ``test262/test/language/source-text/6.1.js``
- ``test262/test/built-ins/Number/S9.3.1_A3_T1.js``
- ``test262/test/built-ins/Number/S9.3.1_A3_T2.js``
- ``test262/test/built-ins/Number/S9.3.1_A2.js``

fail due to a discrepancy between how Unicode characters are treated in JavaScript (either UCS-2 or UTF-16) and OCaml (sequences of bytes). One solution would be to move to strings provided by the `Camomile <http://camomile.sourceforge.net/>`_ library instead of the native OCaml strings.

Reproducing the Results
^^^^^^^^^^^^^^^^^^^^^^^

1. Clone our `forked Test262 repository <https://github.com/GillianPlatform/javert-test262>`_ to a folder on your machine. Inside that folder, you can find the Test262 tests in the ``test`` subfolder. In particular, ``test/language`` contains the core language tests, whereas ``test/built-ins`` contains the tests for the built-in libraries.
2. To run all of the tests, execute the following command inside your Gillian folder:

.. code-block:: bash

   esy
   esy x javert bulk-exec [relative path to your Test262 folder]/test

For example, we normally clone Test262 in the same folder as the Gillian project and change its folder name from ``javert-test262`` to ``test262``. We then run all of the tests by executing the following commands from within the ``Gillian`` folder:

.. code-block:: bash

   cd ..
   git clone https://github.com/GillianPlatform/javert-test262.git test262
   cd Gillian
   esy
   esy x javert bulk-exec ../test262/test

The testing should take approximately thirty minutes. The bulk tester will actively report progress, folder-by-folder, and signal any test failures encountered. In the end, a list of all failed tests (the eight given above) will be printed.

1. If you would like to test a specific subfolder of the test suite, simply add it to the test path. For example, to run only the tests for ``Array.prototype.reduce``, execute

.. code-block:: bash

   esy x javert bulk-exec ../test262/test/built-ins/Array/prototype/reduce/

4. If you would like to examine the filtered tests, you can find them in ``test262_filtering.ml`` `[link] <https://github.com/GillianPlatform/Gillian/blob/PLDI20/Gillian-JS/lib/Test262/Test262_filtering.ml>`_.

Detailed Per-Folder Breakdown: Language
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

================================ ================ === ======== ================== ========= =========== ============= ===================== =========== ===================== =========== ======== ================ ======== =========== ============== =========== ========== ===== =========== =====
/                                arguments-object asi comments directive-prologue eval-code expressions function-code future-reserved-words global-code identifier-resolution identifiers keywords line-terminators literals punctuators reserved-words source-text statements types white-space Total
================================ ================ === ======== ================== ========= =========== ============= ===================== =========== ===================== =========== ======== ================ ======== =========== ============== =========== ========== ===== =========== =====
**All tests**                                  46 101       18                 62        58        1469           212                  55             3                    11          49       25               41      145          11             13           1        733   109          40  3202
**Non-strict tests**                           12   0        0                 57         4         153           107                   7             2                     5           0        0                0        0           0              0           0        227     9           0   583
**Strict tests**                               34 101       18                  5        54        1316           105                  48             1                     6          49       25               41      145          11             13           1        506   100          40  2619
**Non-strict features**                         0   0        0                  0        25           1             5                   0             0                     0           0        0                0        0           0              0           0         10     0           0    41
**For unimplemented features**                  0   0        0                  0         0           0             0                   0             0                     0           0        0                0       17           0              0           0          0     0           0    17
**Using unimplemented features**                0   0        2                  0         0           3             4                   0             0                     0           0        0                0       12           0              0           0          2     0           0    23
**ES6+**                                        0   0        0                  0         0           0             0                   0             0                     0           0        0                0        0           0              0           0          3     0           0     3
**Applicable**                                 34 101       16                  5        29        1312            96                  48             1                     6          49       25               41      116          11             13           1        491   100          40  2535
**Passing**                                    34 101       16                  5        29        1312            96                  48             1                     6          49       25               37      116          11             13           0        491   100          40  2530
**Failing**                                     0   0        0                  0         0           0             0                   0             0                     0           0        0                4        0           0              0           1          0     0           0     5
================================ ================ === ======== ================== ========= =========== ============= ===================== =========== ===================== =========== ======== ================ ======== =========== ============== =========== ========== ===== =========== =====
     
Detailed Per-Folder Breakdown: Built-ins
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

================================ ===== ======= ==== ========= ================== ========= ================== ===== ==== ======== ====== ======== ======== ===== ==== ==== === ====== ====== ========== ======== ====== ====== ========= =====
/                                Array Boolean Date decodeURI decodeURIComponent encodeURI encodeURIComponent Error eval Function global Infinity isFinite isNan JSON Math NaN Number Object parseFloat parseInt RegExp String undefined Total
================================ ===== ======= ==== ========= ================== ========= ================== ===== ==== ======== ====== ======== ======== ===== ==== ==== === ====== ====== ========== ======== ====== ====== ========= =====
**All tests**                     2171      42  430        52                 52        28                 28    33    7      398     31        7        2     2   90   81   7    152   2892         40       57    501    749         8  7860
**Non-strict tests**                27       0    0         0                  0         0                  0     0    0       88      4        2        0     0    0    0   2      0      7          0        0      0      3         3   136
**Strict tests**                  2144      42  430        52                 52        28                 28    33    7      310     27        5        2     2   90   81   5    152   2885         40       57    501    746         5  7724
**Non-strict features**              0       0    0         0                  0         0                  0     0    0       50      0        0        0     0    0    0   0      0      0          0        0      0      0         0    50
**For unimplemented**                0       0   17        45                 45        21                 21     0    0        0      0        0        0     0   81    0   0      5      5         33       50    455    404         0  1182
**Using unimplemented features**     3       0    0         0                  0         0                  0     0    0        3      0        0        0     0    0    0   0      0      0          0        0      0      6         0    12
**ES6+**                             0       0    0         0                  0         0                  0     0    0        0      0        0        0     0    0    0   0      0      2          0        0      0      0         0     2
**Applicable**                    2141      42  413         7                  7         7                  7    33    7      257     27        5        2     2    9   81   5    147   2878          7        7     46    336         5  6748
**Passing**                       2141      42  413         7                  7         7                  7    33    7      257     27        5        2     2    9   81   5    144   2878          7        7     46    336         5  6745
**Failing**                          0       0    0         0                  0         0                  0     0    0        0      0        0        0     0    0    0   0      3      0          0        0      0      0         0     3
================================ ===== ======= ==== ========= ================== ========= ================== ===== ==== ======== ====== ======== ======== ===== ==== ==== === ====== ====== ========== ======== ====== ====== ========= =====