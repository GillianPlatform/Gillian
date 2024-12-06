
Initial Lab
===========

Welcome to the first Gillian lab! In this lab, you'll be using Gillian and its :doc:`debugger </debugger>` to add missing proof tactics to while programs and verify them with :doc:`/wisl/index`.

You are provided with 2 files: ``sll.wisl`` and ``dll.wisl``, which contain predicates, lemmas and functions. The goal is to use WISL and its debugger to insert the right proof tactics for every function to successfully verify. 
Note that in practice, Gillian can infer a lot of these annotations on its own; for this lab, we disable this by using Gillian's "manual" mode.
For every while loop, the invariant is already provided.


Getting started
---------------

First off, make sure you have the necessary prerequisites:

* `Docker <https://www.docker.com/>`_

* `VSCode <https://code.visualstudio.com/>`_

  * ...with the `Dev Containers extension <https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers>`_ installed

Then, clone the lab files and open the folder in VSCode:

.. code-block:: text
  
  $ git clone https://github.com/GillianPlatform/gillian-lab.git --branch WEBSITE --depth 1
  $ code gillian-lab

And initialise the dev container:

* If necessary, select *"Yes, I trust the authors"*.

* Launch the dev container by clicking *"Reopen in Container"* on the popup.

  * If you cannot find the popup, open the Command Palette by pressing ``F1``, then run *"Dev Containers: Open Folder in Container..."* and select the ``gillian-lab`` folder.

Then, to start the debugger:

* Open ``sll.wisl`` (or ``dll.wisl``) in the editor

* Scroll down to the function you want to debug

* Click the text above the function saying *"Verify ..."*

Note that you can quickly return to a point in the execution after modifying the program by setting a breakpoint on the relevant line, restarting the debugger, and clicking the Continue button.

Proof tactics
-------------

| **Folding & unfolding predicates**
| You can fold a predicate with:
|  ``[[ fold pred_name(param1, ... paramN) ]]``
| And similarly unfold a predicate with:
|  ``[[ unfold pred_name(param1, ... paramN) ]]``

| **Applying lemmas**
| A number of lemmas are provided for you; you can apply them like so:
|  ``[[ apply lemma_name(param1, ... paramN) ]]``

| **Logical assertions**
| You can assert a logical condition with:
|  ``[[ assert someCondition ]]``
| Some proofs will require you to use ``assert`` to bind variables. For example, let's imagine that you need to apply a lemma, and one of the parameters is the value contained in a cell at the address in variable ``t``, i.e. your state contains ``t -> ?``, and you want to apply ``some_lemma(t, ?)``. The problem is, you do not have any program or logical variable available that contains the right value to use as the second parameter. One solution would be to use a program variable:
|  ``v := [t];``
|  ``[[ apply some_lemma(t, v) ]];``
| However, modifying the program for the sake of the proof is against the spirit of things! That's when ``assert {bind: ..}`` comes in:
|  ``[[ assert {bind: #v} (t -> #v) ]];``
|  ``[[ apply some_lemma(t, #v) ]];``

| **Conditionally applying tactics**
| You can use if/else in a logical context, like so:
|  ``[[ if (condition) { .. } { .. } ]]``

| **Loop invariants**
| Loop invariants are provided for you where necessary. They are declared like so:
|  ``[[ invariant {bind: #x, #y} P(#x, #y) ]]``

Common issues
-------------

Syntax
^^^^^^

The while language syntax can be a bit tricky:

* Put everything in parentheses! Operator precedence may be unpredictable.
* There is a semi-colon *between* each command inside a block, but *not at the end* (e.g. the last statement in an if-else block).
* Logical commands are surrounded by ``[[ .. ]]``.

Automatic unfolding in preconditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Even in manual mode, Gillian will automatically unfold any predicate in the precondition of a function if it is not recursive.

In particular, the ``dlist`` predicate gets unfolded into its ``dlseg`` definition automatically.

Folding a list with one element
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

| You may be having trouble trying to fold ``SLL`` with a single value, i.e. ``SLL(x, [v])``. This can go wrong because Gillian can't find the base case, ``SLL(null, [])``. Since the base case doesn't require any resources from your state, you're free to fold it from nothing, like so:
|  ``[[ fold SLL(null, []) ]];``
