Folder Structure
================

.. note::
   Folders marked with (❌PLDI20) are out of scope of the PLDI 2020 Gillian Paper.

* **Gillian-JS**

  * ``bin``: The Gillian-JS binary

  * ``environment``: Execution environment, not part of the repository, created using ``esy init:env``. It contains useful scripts for testing Gillian-JS, and examples are copied in it so that they can be safely modified.

  * ``examples``: Various examples

    * ``Fantine``: Bi-abduction examples (❌PLDI20)

    * ``Cosette``: Symbolic testing examples

      * ``buckets``: Tests for the Buckets.js library

      * ``case_studies``: Data structures used for initial evaluation (not reported)

    * ``JaVerT``: Verification examples (❌PLDI20)

  * ``lib``: The core of Gillian-JS

    * ``compiler``: The JS-2-GIL compiler

    * ``JSIL``: Syntax of JSIL and related constructs

    * ``JSLogic``: Verification-related constructs (assertions, predicates, specifications, etc.) (❌PLDI20)

    * ``parsing``: JSIL parsing (programs, annotations, etc.)

    * ``semantics``: Implementation of concrete and symbolic memory models

      * ``CObject.ml``: Concrete objects

      * ``CHeap.ml``: Concrete heaps

      * ``JSILCMemory.ml``: Concrete memory

      * ``SFVL.ml``: Symbolic field-value lists

      * ``SHeap.ml``: Symbolic heaps

      * ``JSILSMemory.ml``: Symbolic memory

    * ``test262``: Bulk testing for the Test262 test suite

    * ``utils``: Various utilities (configuration, I/O, etc.)

  * ``runtime``: JS-2-GIL compiler runtime (JSIL implementations of JavaScript internal and built-in functions)

  * ``scripts``: Various scripts for setting up the environment and running analyses
