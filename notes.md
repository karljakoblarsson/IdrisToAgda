2019-04-10
==========

Now I'm able to use my Agda backend to put out some IR from the Idris
1 compiler! It's not an instance of 'show' so it's not pretty but it works.

TODO Onto Blodwen I guess.


2019-04-09
==========

Patrik's notes when he was trying to manually translate IdrisLibs into Agda:
https://github.com/patrikja/SeqDecProb_Agda

Agda is totalt, but Idris is not. Which will probably be a problem some day.
No, sorry thats wrong. Both Agda and Idris is total by default but includes
'bot' if you want it.

Idris 1 uses A-normal form in its complilation. May be good to know someday.
[Idris compilation pipeline](https://github.com/idris-lang/Idris-dev/wiki/Idris-Compilation-Pipeline)

loadModule in the file s/Id/Parser.hs is the main function which reads and
parses an Idris file.
In the same file the function 'fnOpt' is parseing file options.

The function idrisMain is the main entrypoint to the Idris 1 compiler.
The Idris 1 compiler takes the option '--codegen' which determines which
codegen to use. I'm trying to add Agda as an option here with some success.

Done Today
----
- Find backend code in Idris 1
  Which intermediate language should I use? Should I just write a codegen
  similar to the c and JS variants and hope that works. The codegen can decide
  which level of intermediate it gets (which decl). But is it high enough
  level?
- Complie Idris 1 with changes. I have tried to copy code into a new backend,
  it complies but crashes on runtime.


2019-04-04
==========

Blodwen feels less finished than I had hoped. But maybe it's still the most
interesting thing to write a backend for. But what is the difference between
running interactive and AOT compile? I think I know where the AoT code is at
least.

TODO
----
- Write simple Idris programs
  See simpleIdris.idr
- Write simple Agda programs again
  See simpleAgda.agda
- Write manual translation between STLC Idris -> Agda
  See simpleAgda.agda
- Find backend code in Blodwen
    Or since Idris and Agda are so similar should I just use the AST directly
    after parsing?
- Find backend code in Idris 1
- Compile Idris 1 with simple change
  I currently run a version compiled from source
- Compile Blodwen with simple change
  I currently run a version compiled from source

[This page is probably the starting point to lots of info about Idris internals](https://github.com/idris-lang/Idris-dev/wiki/Idris-back-end-IRs)

Some simple code in both Idris (1) and Agda is in the file 'simpleAgda.agda'.
Manually translated, which is simple for such trivial examples. Just defining
Nat and addition as well as vectors with concatenation. However this file uses
pattern matching which should be hard to translate properly in general.


2019-04-03
==========

Today the proposal was accepted. Finally!

I will start on translating Idris to Agda. There are a lot of problems on the
way, but thats why it's research.

There are three big parts of the project in this stage:

Software setup
--------------
- Setup a working build environment for both Idris and Agda.
- Pick specific versions to target for both languages.
- Idris 1 and Blodwen/Idris 2 are different, and Blodwen is still alpha
  software.
- Decide how to implement the transpiler, is it part of the Idris
  implementation as a new back-end, or is it a stand-alone program.

Decide progression of language features to implement/Order-of-attack
--------------------------------------------------------------------
It's impossible to start by implementing the full language from scratch, i need
to start with a really small subset and expand it with small steps. Dependent
types especially har many subtleties and quirks which can make things very
hard.

Something like:
    STLC -> Vec n A -> simple pattern matching? -> Product-types -> Sum-types

Reccords and classes are probably to hard for this project.

Find a running example
----------------------
If possible I would like to find progressibly more advanced files in the
IdrisLibs code. Which uses more and more features of Idris to guide my
implementation.


Misc. TODO
----------
- Contact Nicolas Botta about some insight into IdrisLibs.


I'm able to build Idris 1 from source, so that's one step done.
