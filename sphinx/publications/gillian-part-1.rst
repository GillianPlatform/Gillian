Gillian, Part I
===============

.. rubric:: Parametric Symbolic Execution for Real-World Programming Languages

Authors
-------
* José Fragoso Santos
* Petar Maksimović
* Sacha-Élie Ayoun
* Philippa Gardner

Abstract
--------
We introduce Gillian, a language-independent framework for the development of symbolic analysis tools. Gillian supports whole-program symbolic testing, semi-automatic verification, and automatic compositional testing using bi-abduction. It comes with meta-theoretical results that are parametric on the memory model of the target language and a modular implementation that closely follows the meta-theory, all designed to minimise the instantiation effort of the user. In this paper, we focus on the parametric symbolic execution engine at the core of Gillian and its associated meta-theory. We instantiate Gillian to obtain symbolic testing tools for JavaScript and C, and use these tools to find bugs in real-world code, with times that either outperform or are competitive with the existing language-specific tools.

Venue
-----
Conference on Programming Language Design and Implementation (PLDI)

Year
----
2020

DOI
-----------
* doi:`10.1145/3385412.3386014 <https://doi.org/10.1145/3385412.3386014>`_

Notes
-----
* :doc:`gillian-part-1-diff`
