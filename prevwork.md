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
5: Ewan Denney - A prototypr proof translator from HOL to Coq
6: F Wiedijk - Encoding HOL Light in Coq, Unpublished notes
