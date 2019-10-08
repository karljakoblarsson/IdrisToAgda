A Prototype Proof Translator fro HOL to Coq
===========================================
[Ewen Denny]

They develop a intermediate bytecode representation of proofs supported in both
HOL and Coq. They do not develop a type checker for this format.
They develop a translator from HOL to the intermediate and from the
intermediate to Coq.

Focus on proofs, not on terms.
They represent proofs in a backwards style.

Their translation is done in four passes. First a pass in HOL, translation of
interference steps in HOL, most information is discarded. Secondly, The list of
HOL interference steps is converted to a DAG. Thridly, START HERE TODO. Last,
conversion from bytecode to Coq

Section 2 is maybe interesting in how to write a language comparison.

"In this article we will examine the specific case of translating HOL proofs to
a Coq readable format."

### Further refs to checkout
MathWeb [AHJ+00]
Open Mechanized Reasoning Systems [GPT94]
Felty and Howe [FH97] - A translation of HOL into Nuprl, they focus on terms
not on proofs.


Importing HOL into Isabelle/HOL
===============================
Steven Obua and Sebastian Skalberg

They use a fact of the idea of HOL to reccord each proof step done to
the HOL-kernel. The proofs steps are then replayed in the target system.
They need to make sure that thier proof represenatation is space efficient and
suited to storing.

Most of the time the target and destination system are close enough that the
translation is straight forward. They handle a few smaller special cases where
the systems differ.

### Further refs to checkout
[5,6] from HOL to Coq. Only 5 have a implementation
5: Ewan Denney - A prototype proof translator from HOL to Coq
6: F Wiedijk - Encoding HOL Light in Coq, Unpublished notes


Hybrid Interactive Theorem Proving using Nuprl and HOL
======================================================
Amy P. Felty and Douglas J. Howe

They formalize a substansial mathematical problem using parts of Nuprl and
libraries from HOL together.

According to the authors this is the first case where two different systems are
used together. However there are many previous cases where systems are linked,
a problem of a specific form is imported into another system.  See: [13] [15]

Both Nuprl and HOL uses tactics derived from LCF[6]. Both systems use really
similar reasoning. Some parts where HOL uses special purpose tactixs needed to
be reimplemented in Nuprl.

### Further refs to check out
[13]
[15]

Translating HOL to Dedukti
==========================
A. Assaf, G. Burel

Dedukti is a logic framwork where it seems to be possible to embedd a large
part of used logic systems. Proof systems are increasingly important today.
Therefore interoperabillity will become more important, both for increased
confidence and to build larger proofs. Lambda-Pi-calculus, known prev. as LF
(Logic framfwork) is a dependently typed lambda-calculus.

The introduction of this paper is really basic and starts by describing
Curry-Howard. That seems odd, everyone who would be interested in the should
already know about it and at least one proof system.

Similar to another paper, one problem of HOL in this context is that the proof
steps are not recorded. Therefore proof step recording is added to make it easy
to share proofs.

According to the authors,
6: F Wiedijk - Encoding HOL Light in Coq, Unpublished notes,
Importing HOL into Isabelle/HOL and
Naumov - The HOL/NuPRL translator
all suffer scalability problems. While their implementation do not.

They have developed a tool, Holide, which translates HOL proofs in the
OpenTheory format into Dedukti. The successfully translate the OpenTheory
standard libary.

The last sentence above is huge. Successfully a complete standard library
(given that the std.lib. of OpenTheory is substansial) is much further than
most other attempts here.

Section 4 describes the translator
Section 5 show correctness

### Further refs to check out
[2]
[26]
[28]
[7] A translatorn of Coq to Dedukti - G. Burel CoqInE - Translating...


Dedukti
=======
Dedukti-team

Dedukti is again a framework based on lambda-pi-modulo calculs. In which it is
possible to embedd sveral other logic systems. The authors sees the possibility
to use Dedukti as a framework for all of logic and prrof systems. The have
implemented translation from several systems to Dedukti, in some degree at
least.

Interestingly for us they present translations from both simply type lambda
calculus and a subset of ML. That is very related to our project. However, it
is still on a very theoretical level, and not a general purpose transpiler.

In section 8.3 they talk about universe. That is probably interesting for me.
But not much is said.
