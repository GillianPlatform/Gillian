JaVerT 2.0
==========

.. rubric:: Compositional Symbolic Execution for JavaScript

Authors
-------
José Fragoso Santos
Petar Maksimović
Gabriela Sampaio
Philippa Gardner

Abstract
--------
We propose a novel, unified approach to the development of compositional symbolic execution tools, bridging the gap between classical symbolic execution and compositional program reasoning based on separation logic. Using this approach, we build JaVerT 2.0, a symbolic analysis tool for JavaScript that follows the language semantics without simplifications. JaVerT 2.0 supports whole-program symbolic testing, verification, and, for the first time, automatic compositional testing based on bi-abduction. The meta-theory underpinning JaVerT 2.0 is developed modularly, streamlining the proofs and informing the implementation. Our explicit treatment of symbolic execution errors allows us to give meaningful feedback to the developer during wholeprogram symbolic testing and guides the inference of resource of the bi-abductive execution. We evaluate the performance of JaVerT 2.0 on a number of JavaScript data-structure libraries, demonstrating: the scalability of our whole-program symbolic testing; an improvement over the state-of-the-art in JavaScript verification; and the feasibility of automatic compositional testing for JavaScript.

Venue
-----
Symposium on Principles of Programming Languages (POPL)

Year
----
2019

DOI
---
* doi:`10.1145/3290379 <https://doi.org/10.1145/3290379>`_
