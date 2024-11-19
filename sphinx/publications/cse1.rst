Compositional Symbolic Execution, Part I
========================================

.. rubric:: Compositional Symbolic Execution for Correctness and Incorrectness Reasoning

Authors
-------
* Andreas Lööw
* Daniele Nantes-Sobrinho
* Sacha-Élie Ayoun
* Caroline Cronjäger
* Petar Maksimović
* Philippa Gardner

Abstract
--------
The introduction of separation logic has led to the development of symbolic execution techniques and tools that are (functionally) compositional with function specifications that can be used in broader calling contexts. Many of the compositional symbolic execution tools developed in academia and industry have been grounded on a formal foundation, but either the function specifications are not validated with respect to the underlying separation logic of the theory, or there is a large gulf between the theory and the implementation of the tool. We introduce a formal compositional symbolic execution engine which creates and uses function specifications from an underlying separation logic and provides a sound theoretical foundation for, and indeed was partially inspired by, the Gillian symbolic execution platform. This is achieved by providing an axiomatic interface which describes the properties of the consume and produce operations used in the engine to update compositionally the symbolic state, for example when calling function specifications. This consume-produce technique is used by VeriFast, Viper, and Gillian, but has not been previously characterised independently of the tool. As part of our result, we give consume and produce operations inspired by the Gillian implementation that satisfy the properties described by our axiomatic interface. A surprising property is that our engine semantics provides a common foundation for both correctness and incorrectness reasoning, with the difference in the underlying engine only amounting to the choice to use satisfiability or validity. We use this property to extend the Gillian platform, which previously only supported correctness reasoning, with incorrectness reasoning and automatic true bug-finding using incorrectness bi-abduction. We evaluate our new Gillian platform by using the Gillian instantiation to C. This instantiation is the first tool grounded on a common formal compositional symbolic execution engine to support both correctness and incorrectness reasoning.

Venue
-----
European Conference on Object-Oriented Programming (ECOOP)

Year
----
2024

DOI
---
* doi:`10.4230/LIPIcs.ECOOP.2024.25 <https://doi.org/10.4230/LIPIcs.ECOOP.2024.25>`_
