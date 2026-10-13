
---

gillian-c contains CompCert, which is licensed under the INRIA Non-Commercial License Agreement. Use of the gillian-c binary is limited to educational, research, personal, or evaluation purposes. Each gillian-c tarball includes the license as `LICENSE-CompCert`.

The binaries need [Z3](https://github.com/Z3Prover/z3) on the `PATH`; gillian-c2 also needs [CBMC](https://github.com/diffblue/cbmc).

macOS: the binaries are not notarised. A binary downloaded with a browser gets the quarantine attribute; remove it with `xattr -d com.apple.quarantine <bin>`, or download with `curl`.
