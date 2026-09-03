# Session-by-session plan

Slot is 18h–21h (session 6 is 15h–21h). Working budget per lecture session:

```
18:00–18:15   arrival, recap slide, milestone hand-in
18:15–19:05   lecture block A            (~50 min, ~25-30 frames)
19:05–19:15   break
19:15–20:05   lecture block B            (~50 min, ~25-30 frames)
20:05–20:55   paper discussion           (~50 min)
20:55–21:00   next week + milestone brief
```

**Target: ~55 frames in the main body.** Anything else goes after a
`\section{Appendix}` and is skipped live.

---

## Session 1 — Why design at all? (Tue Sep 15)

Source deck: `1_introduction/introduction.tex` (39 frames — closest to right already).

### Keep
- Challenger cold open, in full. Best thing in the course.
- Types of research (theoretical/empirical, qual/quant).
- Empirical research as exploiting **variation**; research design as inference.
- `A few questions on variation` — the Socratic block on why we care about variation, the
  "few observations" critique, why statistics at all, where the variation is in attribution
  science. Keep all of it.
- Unit of analysis, including `What's the unit of analyses in the data behind this graph?`
- `Not only about data or empirical evidence` — the point that theses fail on the non-data
  part. This is the course's thesis statement; move it earlier.
- Logistics, calendar, textbooks, assessment.

### Cut / compress
- `Research process in detail (1)–(6)` → **2 frames**. Six is three too many for week 1;
  students have no hooks to hang the detail on yet. The detail returns naturally in weeks 2–5.
- `Empirical Research` and `Empirical research` (two near-identical titles) → merge.
- Fix `Key ingredientes` typo.

### Add
- **The triad**: describe / explain / predict, one frame defining it, one frame per goal with
  its CSS anchor example (Chetty atlas / Guess experiment / Blumenstock phone metadata).
  ~4 frames. This is the single most important addition in the course.
- **The five design questions** slide — the course map. 1 frame, reused every week.
- **Where does data come from?** first pass: *designed* (survey, experiment, coding scheme)
  vs. *found* (traces, admin records, scraped text). One frame, plants the flag for session 2.
- **"Audit a claim" exercise** (~15 min, 3–4 frames): three claims on screen, one of them
  AI/CSS-flavoured, one descriptive, one causal. Students say in pairs: what's the goal, what
  would the data have to look like, what's the first thing you'd doubt. Gets them talking in
  hour one, which sets the participation norm for a small cohort.

### Milestone briefed at the end
M1: one sentence naming a topic. Due at the start of session 2.

### Reading
Hannah Fry, *What Data Can't Do* (keep). Add Salganik, *Bit by Bit* ch. 1 — free online,
short, and it frames the whole MA, not just this course.

---

## Session 2 — Questions, and where data comes from (Tue Sep 22) — **rebuilt**

Source: front half of `2_basics_quantitative_data/basics_quant_data.tex` +
substantial new material. This is the session that changes most.

### Block A — from topic to answerable question (~25 frames, mostly existing)
- The RQ ladder, both versions, including the honest "anecdotal argument" step. Keep as-is.
- `Good RQs, in brief` and `Good RQs, more in detail` with the three-way counterexample
  contrast. Keep as-is.
- `Example on generating RQs` (the school peers vs. teaching quality argument). Keep — it is
  a good live exercise.
- `Stories, RQs, and theories` / `Generating theories` — the abstraction ladder, and the
  "if you can't tell a story out of the theory you're not there yet" advice. Keep.
- Mark the **Charlie Kirk running example** as a swappable slot; replace with whatever is
  current in Sep 2026, keeping the same RQ1/RQ2/RQ3 decomposition structure.
- Move here from session 3's current deck: **levels of explanation (macro/meso/micro)** —
  it belongs with theory-building, not with measurement.

### Block B — the data-generating process, socially and technically (~28 frames, **new**)

This is the CSS core of the course. Sequence:

1. **Designed vs. found data.** Two frames. Designed data answers a question you had; found
   data answers a question someone else had, or no question at all. Everything else in the
   block follows from that.
2. **Salganik's ten characteristics of big data sources.** Big / always-on / non-reactive
   (the attractions); incomplete / inaccessible / non-representative / drifting /
   algorithmically confounded / dirty / sensitive (the design problems). ~6 frames. This is a
   *research design* framework with no statistics in it, and it gives students a checklist
   they will use for the rest of the MA.
3. **Non-representative by construction** — the Xbox survey (Wang et al. 2015). An absurdly
   unrepresentative panel producing good election forecasts. The lesson is that
   representativeness is a property of the **design**, not of the sample. ~3 frames.
4. **Drift** — Google Flu Trends (Lazer et al. 2014). A measure that silently stops meaning
   what it meant, partly because the platform changed underneath it. ~4 frames. Doubles as
   the session's assigned paper.
5. **Algorithmic confounding** — the platform is in your DAG. Ranking and recommendation
   shape the behaviour you observe, so an observed association can be a property of the
   system rather than of the people. ~3 frames. Forward-references DAGs in session 4.
6. **Access and the post-API age** (Freelon 2018) — what happens to a design when the data
   source can revoke access, and what that means for reproducibility. ~2 frames. Practical:
   several of them will propose scraping something for the final essay.
7. **Ethics preview** — one frame, informational risk and consent at scale, pointing to the
   full treatment in session 5. Flag it early because essay proposals get written in week 4.

### Milestone
M1 collected. M2 briefed: topic → question, plus units and the variation you'd exploit.

### Reading
Lazer, Kennedy, King & Vespignani (2014), *The Parable of Google Flu*, Science. Three pages,
no math, and it is simultaneously about measurement validity, drift, and big-data hubris.
(Müller-Crepon moves to session 3.)

---

## Session 3 — Do the numbers mean what you think? (Tue Sep 29) — **promoted to a full session**

Source: back half of `basics_quant_data.tex` (which already has ~40 frames of good material
here), plus new CSS measurement content.

### Block A — concepts and measurement (~28 frames, mostly existing)
- Concepts as building blocks; rule-based vs. ideal types; the household exercise; the
  political-violence exercise. Keep.
- `Operationalization`, with *operationalize ≠ measure*. Keep.
- `Importance` — the point that a huge share of good quantitative work *is* conceptual and
  operational improvement, and that this matters **especially** for CSS. Keep and sharpen.
- Civil war / outbreak worked example. Keep.
- The three measurement issues, proxies, latent variables, validation. Keep.
- Democratic backsliding: objective indicators vs. V-Dem. Keep — best measurement frame in
  the course.
- Missing data and sampling bias as measurement problems. Keep.

### Block A additions (**new**, ~8 frames)
- **A classifier's output as a variable.** When your independent variable is the output of a
  model — a topic label, a sentiment score, an estimated ideology — the measurement question
  doesn't go away, it gets harder, and the error is *not* random. Use Barberá's Twitter ideal
  points (images already in `img/barbera_tw*.jpg`, `img/barbera_etal*.jpg`) as the well-done
  case: what was validated against what. Then the general rule from Text-as-Data: validate,
  always, against something external.
- **A proxy that encodes the thing you didn't mean** — Obermeyer et al. (2019): an algorithm
  allocating care used *healthcare cost* as a proxy for *health need*. Purely a construct
  validity failure, no statistics needed to see it, and it lands hard with this cohort. ~3
  frames.

### Block B — description as a first-class goal (~18 frames, existing + expanded)
The current deck has this material but it sits at the end of an overloaded session and gets
cut. Given a proper slot:
- Describing variables, describing relationships, univariate and bivariate. Keep.
- `Is description useful?` — expand from a rhetorical aside into the actual argument.
  Description has its own design requirements: coverage, comparability, a defensible
  denominator, and a clear population.
- **Add**: Chetty et al. (2022) social capital — a purely descriptive project at enormous
  scale, with real design decisions and real consequences. The example that proves
  description isn't the consolation prize.
- **Add**: culturomics (Michel et al. 2011) and its critique (Pechenick et al. 2015) — a
  descriptive claim about culture that turns out to be partly a claim about what got
  digitised. Ties back to session 2's drift and to corpus composition. ~3 frames.
- Wartime civilian deaths worked example. Keep as the closing synthesis.

### Milestone
M2 collected. M3 briefed: your key concept, how you'd measure it, one threat to that measure.

### Reading
Müller-Crepon, Hunziker & Cederman, *Roads to Rule, Roads to Rebel* (moved here from session
2). It is fundamentally an operationalization paper — a relational concept of state capacity
built out of road networks — so it belongs in the measurement session, where the discussion
can be "what concept, what operationalization, what would you have done instead."

---

## Session 4 — What comparison licenses a causal claim? (Tue Oct 6)

Source: `3_causality/causality.tex` (91 frames → trim to ~55).

### Keep
- Explaining relationships; potential outcomes; the fundamental problem.
- The PO worked example — but **compress from 8 frames to ~5**.
- ATE vs. ATT, including why ATT is often the useful one in practice.
- Experiments and their threats: randomization failure, SUTVA, attrition, compliance,
  external validity. Keep all five.
- DAGs: what one is, mediation vs. moderation, the income-inequality example, DAGs and
  mechanisms.
- Front doors and back doors, the full ten-frame build. Keep — it earns the length.
- **Usual suspects**: confounding, reverse causality, bidirectional causation, selection
  bias, collider bias (with the animation), post-treatment bias, and the closing
  `what should not be controlled for` recap. Keep entirely. Best block in the course.

### Cut / compress
- `Off topic: Controlling` ×4 → **1 conceptual frame**. Controlling as blocking a back door,
  not as adding a regressor. The regression version belongs to AQM.
- Restore the **prediction** frames here or in session 1 (currently 6 commented frames). If
  the triad is properly introduced in session 1, one or two frames here suffice: prediction
  needs no causal identification, which is exactly why it can't answer a causal question.
  Hofman, Sharma & Watts (2017) is the one-page reference.

### Add (**new**, ~6 frames)
- **Platform experiments as the modern experimental ideal** — and their limits. Guess et al.
  (this week's paper) as the well-resourced case: real randomization, huge N, and *still*
  bounded external validity because it ran on one platform in one campaign. Pair with Bail
  et al. (2018), where exposure to opposing views on Twitter *increased* polarization — a
  clean, memorable experiment whose finding contradicts the intuitive mechanism.
- **Interference is the normal case in CSS, not an edge case.** SUTVA is introduced above as
  an assumption; make the CSS point explicitly, because it is where a lot of student designs
  on network or platform data quietly fail. If treating one user affects their contacts,
  there is no clean control group, and the design has to be built around that from the start
  rather than patched afterwards. No math required to make this land.

### Milestone
M3 collected. M4 briefed: the comparison you'd make, and what would bias it.

### Reading
Guess et al. (2023), *How do social media feed algorithms affect attitudes and behavior in an
election campaign?* Science. Keep — and it now sits in the right session.

---

## Session 5 — What variation can you exploit, and how far does it travel? (Tue Oct 13) — **rebuilt**

Source: `4_causal_inference_methods/causal_inference.tex` (58 frames, well-calibrated) +
the *first* ~45 frames of `5_advanced_topics/advanced.tex`. The remaining ~65 frames of
`advanced.tex` are cut.

### Block A — the design templates, at design level only (~28 frames)
- **Exogenous variation** as the unifying idea. Keep the existing framing — it is already
  the right one.
- `Overview of the 3 design templates`. Promote this to the organizing frame of the block.
- For each of **matching/controlling, fixed effects, DiD, RDD, IV**, exactly two things:
  *what variation does it exploit*, and *what has to be true for it to work*. Roughly 3–4
  frames each, no estimation. Existing frames already do this well in this deck; the job is
  resisting the pull toward the detailed versions.
- Keep Snow's cholera (`img/did_snow.pdf`), the street-names DiD (`img/did_TJ*`), and the IV
  and RDD worked examples.
- Keep `Alternative approaches to IV: build your own` — good for essay proposals.
- Keep `Controlling and exploiting exogeneity` — the point that designs combine.

### Block B — generalization and credibility (~25 frames)
- **External validity** (Egami & Hartman 2023) and **construct validity**. Keep.
- **Meta-analysis as a design idea** — the natural-resources example. Compress from ~10
  frames to ~5: the point is that a literature is itself an object of study, and that funnel
  plots reveal what didn't get published.
- **Temporal validity** (Munger 2023). Keep — and it is especially pointed for CSS, where the
  platform under study may not exist in five years.
- **Placebo and robustness tests as design logic**, not as statistics: what would have to be
  false for your result to be an artifact, and can you go look. Keep.
- Synthetic control — keep as **appendix**, cover only if time.

### Block B additions (**new**, ~10 frames)
- **Researcher degrees of freedom and the forking paths problem.** Then Breznau et al. (2022):
  162 researchers, same data, same hypothesis, results spanning both signs. Nothing in it
  requires understanding a model, and it is the strongest possible argument for why you
  commit to a design *before* seeing results. ~4 frames.
- **Pre-registration and pre-analysis plans** — and the explicit statement that the final
  essay *is* a pre-analysis plan. This reframes the assignment from "an essay about methods"
  to "the actual document a researcher writes," which noticeably improves what students hand
  in. ~2 frames.
- **Research ethics for computational work** — informational risk, consent at scale,
  identifiability of "anonymous" data. Emotional Contagion (Kramer et al. 2014), the
  Tastes/Ties re-identification, Netflix de-anonymization, and GDPR as the framework these
  students will actually be bound by. ~4 frames. There may be nowhere else in the MA where
  this is covered, and students proposing to scrape or link data need it *before* they write
  their essays, not after.
- **Closing: prediction revisited.** The Fragile Families Challenge (Salganik et al. 2020) —
  hundreds of teams, unusually rich longitudinal data, best models still predicting life
  outcomes poorly. Return to the session-1 triad, close the course on calibrated humility
  rather than on a list of methods. ~3 frames.

### Cut from this session
The entire `Causal methods again, in detail` section of `advanced.tex` — ~65 frames covering
controlling in regression, matching mechanics, propensity scores, FE (×13), DiD (×13), RDD
(×10), IV (×8). Duplicative of Block A and outside the course's stated scope. Move to
`slides/appendix_methods_detail/` and offer as optional self-study for students continuing
to AQM.

### Milestone
M4 collected. M5 briefed: 1-page design sketch, swapped with an assigned peer before the
workshop.

### Reading
None assigned — protect the time for questions before the essay. Optional: Salganik,
*Bit by Bit* ch. 6 (ethics).

---

## Session 6 — Workshop (Mon Oct 26, 15h–21h)

Structure is currently "12–15 slots, details in class." Proposed changes in
`06_assessment.md`; the substantive ones:

- Group presentations must include a slide titled **"the threat I can't solve."** Removes the
  incentive to oversell, and produces much better discussion.
- Presenters must state **which of the three goals** their project has (describe / explain /
  predict) and **which design template** they're using, in the first minute. Closes the loop
  on the spine.
- Peer reviewers are **assigned in advance** and have read the M5 sketch. Reviewing cold in
  the room produces vague feedback; reviewing a document produces specific feedback.
- Timeboxing for ~12–15 slots across 6h: 12 min present, 8 min discussion, hard stop.
