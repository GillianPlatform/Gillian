Compositional Symbolic Execution, Part II
=========================================

.. rubric:: Compositional Symbolic Execution for the Next 700 Memory Models

Authors
-------
* Andreas Lööw
* Seung Hoon Park
* Daniele Nantes-Sobrinho
* Sacha-Élie Ayoun
* Opale Sjöstedt
* Petar Maksimović
* Philippa Gardner

Abstract
--------
Multiple successful compositional symbolic execution (CSE) tools and platforms exploit separation logic (SL) for compositional verification and/or incorrectness separation logic (ISL) for compositional bug-finding, including VeriFast, Viper, Gillian, CN, and Infer-Pulse. Previous work on the Gillian platform, the only CSE platform that is parametric on the memory model, meaning that it can be instantiated to different memory models, suggests that the ability to use custom memory models allows for more flexibility in supporting analysis of a wide range of programming languages, for implementing custom automation, and for improving performance. However, the literature lacks a satisfactory formal foundation for memory-model-parametric CSE platforms.

In this paper, inspired by Gillian, we provide a new formal foundation for memory-model-parametric CSE platforms. Our foundation advances the state of the art in four ways. First, we mechanise our foundation (in the interactive theorem prover Rocq). Second, we validate our foundation by instantiating it to a broad range of memory models, including models for C and CHERI. Third, whereas previous memory-model-parametric work has only covered SL analyses, we cover both SL and ISL analyses. Fourth, our foundation is based on standard definitions of SL and ISL (including definitions of function specification validity, to ensure sound interoperation with other tools and platforms also based on standard definitions).

Venue
-----
Conference on Object-Oriented Programming Systems, Languages, and Applications (OOPSLA)

Year
----
2025

DOI
---
* doi:`10.1145/3763151 <https://doi.org/10.1145/3763151>`_
