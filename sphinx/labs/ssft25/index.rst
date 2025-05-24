SSFT 2025
=========

.. toctree::
   :hidden:

   Installation instructions <install>

Welcome to the Gillian lab at SSFT 2025!

This lab take the Separation Logic and Compositional Symbolic Execution theory you've seen in the lectures, and recontextualise it with a semi-automatic verification tool built with Gillian.

Installation
------------

See :doc:`here <install>` for installation instructions.

We recommend you follow these ahead of time, as installing the Gillian container can take a while.

General notes
-------------
* If you get a :code:`SIGPIPE` or :code:`"Broken pipe"` error, try making a small change to your code and trying again.
* If Gillian seems stuck or unresponsive, try opening the Command Palette with (:code:`F1` or :code:`Ctrl+Shift+P` by default) and running the *"Reload window"* command 

Session 1
---------
This session focuses on introductory exercises to familiarise yourself with Gillian and its Compositional Symbolic Execution -based verification.

These exercises consist of singly-linked-list algorithms that aim to demonstrate:

* Folding and unfolding predicates
* Compositinally calling functions
* Applying proof tactics and lemmas
* Loop invariants
* Strengthening specifications

These exercises are in the :code:`lab1` directory of the lab repository.

If you have time left over, try your hand at exercises A-D from :code:`lab2`.


Session 2
---------
The second session will apply what you've learned in session 1 to some more complicated exercises.

This includes:

* Verifying algorithms on different data structures, including sets, doubly-linked lists, and binary search trees
* Writing a complex predicate for a well-bracketed property
* Verifying algorithms ported from the real-world `Collections-C <https://github.com/srdja/Collections-C>`_ library.
* A harder verification for a loop invariant

These exercises are in the :code:`lab2` directory of the lab repository.
