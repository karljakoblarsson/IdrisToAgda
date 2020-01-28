Opposition
==========

The oral presentation is followed by opposition from the opponent(s) who are in
the process of doing their own master’s theses. The maximum number of students
to be opponent for one thesis is two persons. That is, not two groups unless
both groups contain only one person. Trouble finding an opponent? Try the FAQ
page.

The opponents have 10-15 minutes to present their opinion on how the
presentation was performed, to ask questions on the material and to discuss it
with the presenter. The questions and discussion should not focus on details
but be more general so the rest of the audience who have not read the written
report can follow the discussion and maybe give their own input. The examiner,
who is leading the session, shall approve (or reject) the presentation as well
as the opposition. The opposition has to be both oral and written. The
opposition report is to be handed over the group after the presentation. If you
want to, you can use this template for oppositionPreview the document (examples
taken from the domain of Software Engineering).

Useful points to consider when performing the opposition are:

  - The planning and structure of the report.
  - Definition of the problem at hand.
  - Method and realization.
  - Delimitation.
  - Theory.
  - Analysis.
  - Results and the handling of the result.
  - The shaping of the report and formal points like references.
  - The examiner for the student who does his or her oral presentation should
    also evaluate the opposition and approve or reject it.

You enroll as an opponent by e.g. finding a suitable presentation for
opposition on the page where you book your own presentation. Contact the
student(s) in question and make an agreement. You may also find a suitable
presentation in other ways, e.g. when attending writing seminar II. It is not
uncommon that two groups arrange so that they oppose on each other, but
non-symmetrical arrangements are also ok. Make sure that you get the report in
time, not less than a week before the presentation, so you can do a proper job.

A thesis must have at least one and at most two opponents, i.e. individuals. It
is up to the students to find a thesis to oppose to and to find an opponent
that opposes to his or her thesis. If you have trouble finding an opponent,
you may try the discussion groups on your program pages.

Finally, you will present your thesis work. You need to find an opponent and
hand over the draft thesis report to the opponent at least a week before the
presentation takes place. Note that your examiner has to approve your can book
your presentation. You should also arrange with your supervisor, so that she/he
can attend your presentation.

See the page for presentation dates and times (Links to an external site.) to
book your time slot for presenting your work.


Template
========

### Opposition Report
Jakob Larsson (MPALG)

Thesis: Formally Verifing WebAssembly with KWasm
Authors of Thesis: Rikard Hjort

### Summary
``One small paragraph summary of the thesis. What is the topic? What the type
of research done? What is the main result?``
This is a thesis about formal verification of software. Specifically, it
attempts to further the start of art in verifing WebAssembly. With a specific
focus at smart contracts as they have very high correctness needed. The goal is
to verify a common but simple smart contract, but in order to do that, the
tools and models needs to be improved.

``One small paragraph on what you liked about the thesis. What was well done in
your point of view?``
I think it is really interesting with a case study of a practical verification
attempt. Many other formal verification litterature is way to abstract and
about theory. This project tries to verify an actual program and depicts the
steps needed to get there. As is common, it is alot harder in practice then in
theory. The tools have certain drawbacks which needs to be worked-around.

### Points of Critique1
``Summarize 2-3 critique points/very critical question with a short explanation
(max. 1 paragraph each) why you consider the answer/response to that
point/question important for the validity of the presented research.``

#### A
Since much of the work is about verifing the programs, by any means possible.
It seems hard to not consider tools other than K. You probably should have
a small survey about other possible tools.

#### B
You seem to be booged down in nitty-gritty details about bytes and memory
storage. Would it be possible to go further by adding larger lemmas and axiom?
Like that the byte-order reversing function is correct which is possible to
verify by hand.

#### C
There are many new axioms added to K. Every new axiom is a liability, and this
projects adds as many as 34 just for theese problems. Is there possible to do
with less?

#### D
How bad are the issues about webassemblies test suite. You only say that they
use unspecified modules. But you don't say anything else about thoose modules.
Are they really needed? Would it be possible to refactor the tests instead? It
may seem like an easy cop-out.


### List of Questions1
``A plain list of questions prepared for opposition. During the opposition,
each opponent should target to ask at least 5 questions, including at least one
question that addresses a serious point of critique. You can of course deviate
from the list during the opposition, e.g. in case the presentation answered
some of your questions or there are other questions that came up while
listening to the presentation.``

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


### Other Comments and Feedback
``Here you can optionally add more detailed comments and feedback.``

More specific comments about the report is given in the git-repo.

