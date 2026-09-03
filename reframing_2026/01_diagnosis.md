# Diagnosis: audit of the current five decks

Frame counts and PDF pages as of 2026-09-03.

| Deck | Frames | PDF pp. | Verdict |
| :--- | ---: | ---: | :--- |
| `1_introduction/` | 39 | 85 | Good bones, slightly over-long on process |
| `2_basics_quantitative_data/` | 86 | 141 | Doing the work of two sessions |
| `3_causality/` | 91 | 132 | Strong but over-length; prediction block dead |
| `4_causal_inference_methods/` | 58 | 85 | Best-calibrated deck in the course |
| `5_advanced_topics/` | 110 | 148 | Two decks stapled together; ~60% duplicative |

Each folder holds one `.tex` deck of the same name (`introduction.tex`,
`basics_quant_data.tex`, `causality.tex`, `causal_inference.tex`, `advanced.tex`).

## What is working and should be protected

- **The Challenger opening** (session 1). A genuinely excellent cold open: an explanatory
  question, an obvious-in-hindsight selection problem, no statistics required, images already
  in `img/challenger*.jpg`. Keep verbatim.
- **The RQ ladder** (session 2): `Topic > RQ > Theory` refined into
  `Previous evidence > argument > RQ > Hypotheses`, then the honest version with the
  "anecdotal argument" step. This is the most useful twenty minutes in the course for students
  who have never written a design. Keep and expand.
- **"Good RQs" criteria with counterexamples** (answerable / relevant / feasible / narrow,
  with the three-way contrast of good vs. too broad vs. many-questions-in-one). Keep.
- **Concepts as rule-based vs. ideal types**, and the insistence that
  *operationalize ≠ measure*. Rare to see taught explicitly; keep.
- **The democratic-backsliding measurement contrast** (objective indicators vs. V-Dem
  subjective coding). Perfect no-math measurement-validity lesson.
- **The "usual suspects" bias sequence** (confounding → reverse causality → selection →
  collider → post-treatment), with the collider animation and the closing
  "what should *not* be controlled for" recap. Strongest teaching block in the course.
- **The three-design-templates overview** in session 4. This is already the right idea; the
  reframing promotes it to the organizing frame for the whole session.
- **Snow's cholera** and the **street-names DiD** (instructor's own work). Both keep.

## Problems, in order of how much they cost

### 1. Session 5 is two decks stapled together, and most of it is out of scope

`advanced.tex` runs 110 frames. The first ~45 are a coherent session (generalization,
meta-analysis, external validity, placebo/robustness logic, synthetic control). The remaining
~65 — the `Causal methods again, in detail` section — re-teach controlling, matching,
propensity scores, fixed effects, DiD, RDD, and IV, this time *with* the regression
machinery: `Controlling in regression`, `Fixed effects and regression` (×3),
`RDD and regression`, `How does IV work?` (×7), `Two approaches to matching: propensity score`
(×2).

Two problems at once. It duplicates session 4, and it breaks the course's own stated scope —
the syllabus promises "we will not cover statistical techniques or advanced data analysis."
**Cut the whole section**; park it as an optional appendix deck for students continuing to AQM.

### 2. Nothing in the course is about where computational data comes from

The MA is in Computational Social Science. CSS currently enters only as *examples* — Barberá's
Twitter ideal points, Blumenstock's phone metadata, Müller-Crepon's roads, the Guess et al.
platform experiment. All good examples, but no session treats the design problems that are
**specific** to computational data:

- found/observational digital traces vs. data you designed to collect
- the platform as part of the data-generating process (algorithmic confounding)
- non-representativeness by construction, and why it is a property of the design not the sample
- drift: a measure that silently stops meaning what it meant
- access and reproducibility in the post-API era
- a classifier's output used as a variable — validation, and error that is not random
- ethics and informational risk at scale

Every one of these is teachable with zero statistics and zero code. This is the largest gap
and the main thing the reframing fixes.

### 3. Prediction is commented out

`causality.tex` contains six consecutive commented frames: `About prediction (in the social
sciences)` and `About prediction` ×5, plus `Prediction and explanation`. For a CSS cohort this
is the wrong thing to have cut. Prediction is a distinct research goal with its own design
logic and its own failure modes, and it is what half the field the students are entering
actually does. Restore it, and promote description/explanation/prediction to the course's
organizing triad.

### 4. Measurement and description are crammed into the back of session 2

`basics_quant_data.tex` covers, in one 3h slot that also has a paper discussion: research
questions, theory generation, concepts, operationalization, three distinct measurement issues,
proxies, latent variables, validation, missing data, sampling bias, unit of analysis, levels of
explanation, univariate and bivariate description, and a worked example. That is 86 frames and
two sessions of material. In practice something gets dropped every year, and it is almost
certainly description — which is then never taught, despite being the goal of a large share of
computational work.

### 5. Deck lengths are not compatible with the slot

A 3h session realistically holds ~100–110 minutes of lecture once you account for arrival,
recap, a break, and 45 minutes of paper discussion. At 1.5–2 min/frame that is **55–70 frames
maximum**, and less when frames are overlay-heavy. Three of five decks exceed that; two exceed
it badly. The fix is not faster delivery, it is an explicit appendix convention.

### 6. No scaffolding between week 4 and the workshop

The final essay is 40% of the grade and is the first research design most of these students
will ever have written. The only checkpoints are a `Re-cap and final essay` slide in session 4
and the workshop itself in session 6. Students arrive at the workshop having started thinking
about their design that week. A milestone ladder fixes this at near-zero grading cost — see
`06_assessment.md`.

### 7. Infrastructure: no presenter notes anywhere

`grep -c '\note{'` returns **0 for all five decks**, and there is no
`beamer_notes_preamble.tex` in `slides/`. The project conventions in `CLAUDE.md` describe a
two-wrapper notes pattern used elsewhere in these course materials; this course never adopted
it. Worth adding as decks get rewritten, not as a standalone task.

### 8. Minor

- The `Charlie Kirk / rise of political violence` running example in session 2 is topical and
  effective but will date fast. Keep the slot, mark it as a **swappable current-event example**
  so it is obvious what to replace each year.
- Two `Off topic: Controlling` frames in `causality.tex` drift into regression a session early.
  Compress to one conceptual slide.
- `Key ingredientes` (session 1) — typo.
- `Research process in detail (1)–(6)` is six slides for something that lands in two or three.
