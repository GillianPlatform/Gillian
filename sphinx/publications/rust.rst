Rust
====

.. rubric:: A Hybrid Approach to Semi-automated Rust Verification

Authors
-------
* Sacha-Élie Ayoun
* Xavier Denis
* Petar Maksimović
* Philippa Gardner

Abstract
--------
We propose a hybrid approach to end-to-end Rust verification where the proof effort is split into powerful automated verification of safe Rust and targeted semi-automated verification of unsafe Rust. To this end, we present Gillian-Rust, a proof-of-concept semi-automated verification tool built on top of the Gillian platform that can reason about type safety and functional correctness of unsafe code. Gillian-Rust automates a rich separation logic for real-world Rust, embedding the lifetime logic of RustBelt and the parametric prophecies of RustHornBelt, and is able to verify real-world Rust standard library code with only minor annotations and with verification times orders of magnitude faster than those of comparable tools. We link Gillian-Rust with Creusot, a state-of-the-art verifier for safe Rust, by providing a systematic encoding of unsafe code specifications that Creusot can use but cannot verify, demonstrating the feasibility of our hybrid approach.

Venue
-----
Conference on Programming Language Design and Implementation (PLDI)

Year
----
2025

DOI
---
* doi:`10.4230/LIPIcs.ECOOP.2024.25 <https://doi.org/10.4230/LIPIcs.ECOOP.2024.25>`_
