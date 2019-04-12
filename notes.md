2019-04-12
==========
I should be writing on the planning report all day today.

"Decide progression of language features to implement/Order-of-attack"-part?
----------------------------------------------------------------------------
I should start with simple data typ declarations like:
```Idris
data Bool = True | False
data Nat = Zero | Succ Nat
```

And simple functions which work on the simple types.
```
add : Nat -> Nat -> Nat
add Z s = s
add (Suc a) b = add a (Suc b)

not : Bool -> Bool -> Bool
not True = False
not False = True

```

However, from here on it's harder to know the way. I would like to continue
with simple dependent types, since that's the main point of the project. To be
able to translate the usual Vector example. But than I need some simple pattern
matching, which is supposed to be hard. I already need simple pattern matching
in these simple examples.

After that Product-types, and sum-types is maybe the next step. But I imagine
that here be dragons. Around here I will find the problems that will be the
interesting content in the final thesis. Some which maybe impossible to solve,
or at least take a few years of research.

Then there probably won't be time, but records/classes/interfaces is next. The
final frontier maybe. I should not belive I will get that far. But I probably
need to have a plan, just in case.

### Simply Typed Lambda-calc, from my types notes
```
T, A, B, … ::= Bool | T → T

t ::= true | false | x | t t | λ (x : A) t  -- With names
                   | n | t t | λ A t  -- Namefree

bV ::= true | false
fV ::= λ(x : A) t
```

### Syntax constructs I need to handle (Unconclusive)
Which is maybe not big logical problems or so, but they are still work that
needs to be done. Ordered similar to the Idris Tutorial
- Type signatures
- Declarations
- "Assignment"
- Strings
- Numbers int/double
- Function definitions
- Pattern matching
- Data declarations
- Function application
- where-clauses
- Holes
    Idris has two kinds of Holes, but Agda only one?
- Types as first class values
- Dependent types
- Infix-application
- Implict arguments (In Agda they need to be explicit? So I need that info from the compiler)
- `using`-notation
- `mutual`-blocks
- IO ?
- `do`-notation
- Lazy-eval. Idris is eager by default, and I belive Agda is as well? But Idris
    support lazy, i'm not sure if Agda does.
- Codata types
- Records (Which in Agda is basically classes as well)
- List comprehensions
- `case`-expressions
- How to handle Totality
- Modules
    - Import declarations
    - Export declarations
- Namespaces
- Idris Interfaces...

Practical software problems
---------------------------
In the file `Agda.Syntax.Concrete` the full AST for Agda as produced by the
parser is. Maybe I can reuse this code, then I get a lot of code for free.
Especially if I target Idris 1 since it's written in haskell just like Agda.
But maybe also for Blodwen since simple Haskell is really similar to Idris. But
this might be an argument for targeting Idris 1.


2019-04-11
==========
It will be harder to write a Blodwen backend. The backend code is more tightly
integrated with the tyepchecking. But there are clearly defined IR's like TT
and TTImp which is positive for me.

I'm starting to think a bit how to handle Universe, in Agda they are implicit
but in Idris implicit. As I understand it now. That's one of the main
diffrences between the languages for simple functions and definitions.

Now I have written a stub Agda backen in Blodwen. It doesn't do anything yet,
but it compiles and is runnable. So it will be possible to use Blodwen as well.
But it feels like it would be harder. The translation will be the same, it
only the implementation which differ. It is cool to use the latest version,
however the project is not about that so if I spend to much time fighting with
blodwen, the project can suffer. So maybe thats the argument to target Idris1.
And probably the latest stable version of Agda, 2.5.4.

I should look more into IdrisLibs. And I should have contacted Nicola several
days ago.

The practical parts are mostly done for the planning part. How can I progress
on the
"Decide progression of language features to implement/Order-of-attack"-part?

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
