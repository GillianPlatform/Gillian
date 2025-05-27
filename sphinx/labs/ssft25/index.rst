SSFT 2025
=========

.. toctree::
   :hidden:

   Installation instructions <install>

Welcome to the Gillian lab at SSFT 2025!

This lab take the Separation Logic and Compositional Symbolic Execution theory you've seen in the lectures, and recontextualise it with a semi-automatic verification tool built with Gillian.

.. admonition:: **Installation instructions**

   See :doc:`here <install>` for installation instructions.

   We recommend you follow these ahead of time, as installing the Gillian container can take a while.

Lecture 1: Compositional Software Verification
----------------------------------------------

This lecture covers:

* Separation logic
* Specification and verification of sequential programs for mutating data structures
* Tools inspired by separation logic, based on compositional symbolic execution

:download:`Main slides </_static/docs/cse-ssft25-lecture1.pdf>`

:download:`WISL slides </_static/docs/cse-ssft25-wisl.pdf>`

Lab Session 1: An introduction to Gillian
-----------------------------------------

These exercises are in the ``lab1`` directory of the lab repository. They include list algorithms to demonstrate:

* Folding and unfolding predicates
* Applying proof tactics and lemmas
* Calling functions compositionally
* Loop invariants

If you have time, check out exercises A-D from the ``lab2`` directory for some examples with different data structures.

Lecture 2: Compositional Symbolic Execution
-------------------------------------------

This lecture covers:

* Theoretical foundations - Compositional Symbolic Execution (CSE) and bi-abduction
   * CSE for a simple C-like state
   * State-parametric CSE
   * Functional compositionality
* State combinators for CSE 

*Slides coming soon...*

Lab session 2: Further experience with Gillian
----------------------------------------------

These exercises are in the ``lab1`` directory of the lab repository. They include:

* Some fun, harder examples of data-structure algorithms
* List algorithms ported from the real-world `Collections-C library <https://github.com/srdja/Collections-C>`_
* Some harder iterative list algorithms

Feel free to tackle these in any order, and refer back to the Lab 1 exercises if you need a refresher.

Misc. lab notes
---------------
* The language server highlights compilation errors (and other unexpect problems) in red, and verification falures in blue.
* The debugger supports breakpoints! This can be handy when restarting the debugger after making changes to your code; assign breakpoints to the relevant lines and click the *Continue* button |continue_button|.
* If you get a :code:`SIGPIPE` or :code:`"Broken pipe"` error, try making a small change to your code and trying again.
   * *Note from Nat: I have \*absolutely no idea\* why this happens. Even with a repro, it disappears if I try to track it down 🙃*
* If Gillian seems stuck or unresponsive, or if the WISL language server fails to start, try opening the Command Palette with (:code:`F1` or :code:`Ctrl+Shift+P` by default) and running the *"Reload window"* command.

.. |continue_button| image:: /_static/img/ssft/continue_button.png
   :height: 1.2em
