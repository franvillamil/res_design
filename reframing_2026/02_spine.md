# The spine: what the course is organized around

## Problem with the current spine

The current order — intro → elements of quantitative data → causality → causal inference
methods → advanced topics — is a **topic list inherited from the causal inference curriculum**.
It works, but it has two costs. Students can't tell where they are in an argument, because
there isn't one; and "advanced topics" is a name for a leftovers session, which is why deck 5
grew to 110 frames.

## Proposed spine

Two layers. A **triad** introduced in session 1 and revisited every week, and a **sequence of
five design questions** that maps one-to-one onto the five lectures.

### Layer 1 — three research goals (the triad)

| Goal | The question | A CSS example to anchor it |
| :--- | :--- | :--- |
| **Describe** | What is the case? How much, where, for whom? | Chetty et al. social capital atlas |
| **Explain** | Does X cause Y, and through what? | Guess et al. Facebook feed experiment |
| **Predict** | Given what I observe, what happens next / elsewhere? | Blumenstock et al. wealth from phone metadata |

Why this earns its place as the spine:

- It is the **cleanest way to make "what is your question?" concrete**. Most bad student
  designs are a description question with a causal answer bolted on, or a causal question
  answered with a predictive design. Naming the three goals gives you vocabulary to diagnose
  that in ten seconds during the workshop.
- Each goal has **different success criteria** — coverage and validity for description,
  identification for explanation, out-of-sample performance for prediction — so it motivates
  everything that follows.
- It is **the actual fault line in CSS**, and the students will hit it in every other course in
  the program. Getting it in week 1 of a 1-year MA pays off downstream.
- It restores the commented-out prediction material with a reason to exist.

Return to the triad at the end of every session with one slide: *what does this week change
about how you'd describe / explain / predict?*

### Layer 2 — the five design questions

| # | Session | The question the session answers |
| :--- | :--- | :--- |
| 1 | Introduction | **Why design at all?** What does it mean to answer a question with evidence? |
| 2 | Questions and data | **What is the question, and where would the data come from?** |
| 3 | Measurement | **Do the numbers mean what you think they mean?** |
| 4 | Causality | **What comparison would license a causal claim?** |
| 5 | Designs and credibility | **What variation can you actually exploit, and how far does the answer travel?** |
| 6 | Workshop | Students run their own designs through questions 1–5 |

The wording matters: each is phrased as something a student must be able to answer about
**their own project**, not as a topic to be covered. Put the five questions on a slide in
session 1, and open each subsequent session with that slide, current question highlighted. It
costs one slide a week and it is what makes a five-week course feel like an argument.

## What changes in the allocation

```
                        current                    proposed
questions / theory      ~0.5 session               ~0.5 session
data provenance (CSS)   —                          ~0.5 session      <- new
measurement + descr.    ~0.5 session               ~1 session        <- doubled
causality               ~1 session                 ~1 session
design templates        ~1 session                 ~0.5 session      <- compressed
methods again, detailed ~0.7 session               —                 <- cut
generalization          ~0.3 session               ~0.3 session
credibility + ethics    —                          ~0.4 session      <- new
```

The two additions are paid for almost entirely by the deletion in session 5, which is why the
reframing does not require the course to lose anything it currently teaches well.

## The scope line, stated once so it can be enforced

The syllabus promises no statistical techniques and no code. The operational version of that
promise, for every slide in the course:

> Teach **what variation a design exploits and what has to be true for it to work**. Never
> teach how the corresponding quantity is estimated.

So: DiD is "compare the change in the treated group to the change in a group that should have
moved in parallel, and the design lives or dies on whether that parallel movement is
credible." Not a two-way fixed effects specification. This line is what session 5's detailed
methods block crosses, and having it written down makes the cut obvious rather than arbitrary.
