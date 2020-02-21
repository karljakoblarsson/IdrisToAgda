---
title: Opposition Notes
subtitle: "Thesis: Formally Verifing WebAssembly with KWasm"
author: Jakob Larsson
documentclass: scrartcl
---

Opposition
==========

  <!-- - The planning and structure of the report. -->
  <!-- - Definition of the problem at hand. -->
  <!-- - Method and realization. -->
  <!-- - Delimitation. -->
  <!-- - Theory. -->
  <!-- - Analysis. -->
  <!-- - Results and the handling of the result. -->
  <!-- - The shaping of the report and formal points like references. -->
  <!-- - The examiner for the student who does his or her oral presentation should -->
  <!--   also evaluate the opposition and approve or reject it. -->


```
Author: Jakob Larsson (MPALG)

Thesis: Formally Verifing WebAssembly with KWasm
Author of Thesis: Rikard Hjort
```

### Summary
This is a thesis about formal verification of software. Specifically, it
attempts to further the start of art in verifing WebAssembly. With a specific
focus at smart contracts as they have very high correctness requirements. The
goal is to verify a common but simple smart contract, but in order to do that,
the tools and mechanization needs to be improved. They use the K project and
improves its WebAssembly implementation

I think it is really interesting with a case study about a practical verification
attempt. Many things are missing, but the author tries to fix that with
precision and care. The ambition is that it will be easy to verify smart
contracts in the future. It is interesting to read about what needs to be
improved and fixed in the tools before we are there.

### Points of Critique

#### A
The scope is big in the beginning. They want to formally verify a complete
contract. But as the thesis progresses the scope gets smaller and smaller. Much
time is spent on bugs in the used tools. This is understandable and common, but
it needs to be acknowledged and discussed. In the end there is a lot of
discussion about what doesn't work, not about what actually does, which is
a lot of things.

#### B
There are no proper results. The completed work is never evaluated
in any way. This is of course hard in exploratory work, but I would like to see
an attempt at evaluation. State what is possible after the project which was
not possible before. And what is the value of that?

#### C
This work is about verifing a specific smart contract, by any means possible it
seems in the end.  But the authors use the tool they know and do not consider
others. It would be interesting with a small survey about other possible tools.



### List of Questions

- You choose K because you were familiar with it and because it as a Wasm
    embedding, as stated in the report. But have you done any surveying about
    other tools which may be suitable?
- How applicable is this work to other implementations of Wasm other then the
    Ethereum embeddning?
- Since the EVM uses byte memory and most other computing platforms have larger
    word sizes, do you think your work can be generalized somehow?
- You choosed to improve K instead of Z3 as it imporves the K project for any
    user. But is it possible that you would have gotten a lot further
    otherwise?
- Is it possible to use other SMT solvers with K instead of Z3?
- How far has KEVM come? You say that Runtime Verification has contracts proven
    for generic properties. What and how?
- Is WRC20 the "easiest" to verify contract used in practice, or are there
    other simpler contracts which would be possible to verify?
- Is it possible to complete the reversal proof with less number of added
    axioms?
- How bad are the issues about webassembly test suite. You only say that they
    use unspecified modules. But you don't say anything else about thoose
    modules.  Are they really needed? Would it be possible to refactor the
    tests instead? It may seem like an easy cop-out.
- You seem to be booged down in nitty-gritty details about bytes and memory
    storage. Would it be possible to go further by adding larger lemmas and
    axiom?  Like that the byte-order reversing function is correct which is
    possible to verify by hand.
- There are many new axioms added to K. Every new axiom is a liability, and this
    projects adds as many as 40 just for one single proof. Is there possible to
    do with less?

### Other Comments and Feedback

More specific comments about the report is given in the git-repo.

