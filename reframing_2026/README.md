# Research Design — Reframing plan for Fall 2026

Drafted 2026-09-03. **Course starts Tue Sep 15, 2026** (12 days out).

## The proposal in one paragraph

The course currently spends **2.5 of its 5 lectures on causal-inference machinery** (potential
outcomes → DAGs → biases → FE/DiD/RDD/IV → the same methods again in more detail), and roughly
**half a lecture on measurement and description**. For a cohort at the *start* of a CSS master's
who will meet estimation again with the math in AQM, that allocation is backwards: the things
that actually break MA theses are bad questions, found data of unknown provenance, and
unvalidated measures — not a shaky grasp of the DiD estimator. The reframing **compresses
causality to ~1.5 sessions, gives measurement + description a full session, and opens a new
block on where computational data comes from** (found vs. designed data, algorithmic
confounding, non-representativeness by construction, access). CSS stops being a source of
example slides and becomes the course's design problem. Scope, session count, and slot times
are unchanged.

## Files

| File | What's in it |
| :--- | :--- |
| [`01_diagnosis.md`](01_diagnosis.md) | Audit of the five existing decks — what works, what's duplicated, what's out of scope |
| [`02_spine.md`](02_spine.md) | The organizing logic: describe/explain/predict + the five design questions |
| [`03_sessions.md`](03_sessions.md) | Session-by-session keep / cut / add, with time budgets |
| [`04_examples.md`](04_examples.md) | Worked examples and paper candidates, with what each one teaches |
| [`05_readings.md`](05_readings.md) | Assigned readings per session + instructor background |
| [`06_assessment.md`](06_assessment.md) | Milestone ladder, assessment weights, workshop format |
| [`07_todo.md`](07_todo.md) | Triaged task list: before Sep 15 / during term / next year |

## Headline changes

1. **Session 2 is rebuilt** around data provenance (the CSS core), and measurement moves out of it.
2. **Session 3 becomes a full session on measurement and description** — currently squeezed into
   the back half of session 2.
3. **Prediction is promoted to a first-class research goal** alongside description and
   explanation. It is currently commented out in `3_causality/causality.tex` (six dead frames).
4. **Session 5's duplicate methods block is cut** (~65 frames re-teaching FE/DiD/RDD/IV with
   regression algebra — both redundant and outside the stated no-statistics scope). The freed
   time goes to generalization, credibility/reproducibility, and research ethics.
5. **A milestone ladder** replaces the current jump from week 4 to the workshop: five short,
   cheap-to-grade deliverables that assemble into the final essay.
6. **Decks get capped at ~55 frames** with the overflow moved to explicit appendix sections.
   Current decks run 86–110 frames for a 3h slot that also has to fit a paper discussion.

## Known instructor intents already folded in

- The note at the top of `syllabus_research_design.tex` — *raise the final essay to 50–60%,
  drop participation* — is implemented in [`06_assessment.md`](06_assessment.md) (essay 55%,
  participation dropped).
- `syllabus_research_design.tex` still says **Fall 2025** and Sep 16; `index.md` already has the
  2026 dates. The syllabus needs updating either way — see [`07_todo.md`](07_todo.md).

## If there isn't time for all of this

[`07_todo.md`](07_todo.md) has a **minimal-change variant** that keeps the existing session
order and gets most of the CSS benefit from three inserted blocks and one deletion. That version
is realistically doable in the 12 days available.
