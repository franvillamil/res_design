# Next steps

**Updated 2026-09-10. Session 1 is Tue Sep 15 (5 days).**
Sessions: Sep 15, 22, 29, Oct 6, 13. Workshop Mon Oct 26. Essay due Tue Oct 27.

Session 1 is **ready**: deck, notes version, teaching guide, and the 2026 syllabus are all built
and compile clean. What follows is everything still outstanding.

---

## Settled — no action needed

- **Essay deadline Oct 27** confirmed against the exam calendar.
- **Group formation**: no separate deadline. Whoever submits **Memo 5 together (Mon Oct 19)** is a
  group, and that grouping is binding for the presentation and the essay. In the syllabus and on
  the workshop slide.
- **The two worked examples** (hotel bookings, Spotify) are labelled just "examples" on the
  slides. Mention on the spot that they are real theses or course projects, without naming anyone.
- **Charlie Kirk stays** as the session-2 running example for now. Revisit later; the slot is
  marked `% SWAPPABLE:` so the structure survives a swap.
- **Memos get no written feedback.** Questions in class are welcome, and raising things that struck
  you while reading them is fine, but nothing formal and nothing announced.
- Aula Global assignments set up; syllabus PDF pushed.

---

## The content budget (corrected)

The paper discussion is **~25 minutes, not half the session**. An earlier version of this plan
assumed 45-50, which under-filled session 2 by about 20 minutes before it was fixed. The real
budget for a lecture session:

```
 180  slot
 -12  arrival / settling
 -10  break
 -25  paper discussion
 -13  close, memo brief, questions
 ---
 120  minutes of lecture content
```

At roughly 2-2.5 min per frame with a couple of pair exercises, that is **about 55-60 frames
including section roadmaps**. Session 1 (no paper) has more time and runs 69. Session 2 now runs
59. **Build sessions 3-5 against 120 minutes, not 100.**

Each deck should carry a little more than fits, with a short "if you are running behind" list in
its teaching notes, rather than a little less.

## Before each session — the decks

Only session 1 exists so far. Each new deck goes in `reframing_2026/slides/`, built from
[`08_outline_2026.md`](08_outline_2026.md) and adjusted for how the previous session actually went.

### By Sep 22 — session 2, "Questions and data" — **BUILT**

`slides/2_questions_and_data/` — 50 frames, compiles clean. Block A (topic → question, theory,
mechanisms, levels of explanation moved here) + the new provenance block (readymade/custommade,
Salganik's ten characteristics, Xbox, Google Flu, algorithmic confounding, post-API, dirty and
sensitive) + a structured paper discussion and the Memo 2 brief.

Expanded 2026-09-11 from 50 to 59 frames after correcting the paper-discussion budget: restored
the "Research and RQs" recap and the Mahou exercise in Block A, added "Incomplete", and added a
new practical block (where data actually lives, what a codebook gives you, what a real data file
looks like, and a six-question checklist before committing to a source).

- [ ] **Two figures still missing**, both marked `% TODO:` in `questions_body.tex`. The frames work
      without them, so this is an improvement rather than a blocker:
  - the **CDC vs GFT** over-prediction time series (Google Flu)
  - the **Xbox** demographic-skew bar chart and forecast-vs-outcome plot
- [x] **Done** - teaching notes for session 2 (`teaching_notes_session2.pdf`, 10pp).

### By Sep 29 — session 3, "Measurement and description"

- [ ] Assemble from the existing back half of `basics_quant_data.tex` (mostly exists, just needs
      room)
- [ ] Add classifier-output-as-a-variable, using the Barberá images already in `img/`
- [ ] Add Obermeyer proxy failure; expand the description block with Chetty and culturomics
- [ ] Source figures: Obermeyer cost-vs-illness, an Opportunity Insights map, n-gram + corpus
      composition

### By Oct 6 — session 4, "Causality" (cheapest week, mostly deletion)

- [ ] Trim `causality.tex` 91 → ~55 frames; PO example 8 → 5; `Off topic: Controlling` 4 → 1
- [ ] Restore the commented-out **prediction** frames (1–2, since the triad carries the load now)
- [ ] Add platform experiments and their limits (Guess + Bail), and interference-as-normal-in-CSS
- [ ] Source the Bail treatment-effect-by-party figure

### By Oct 13 — session 5, "Designs and credibility"

- [ ] **Cut** `Causal methods again, in detail` (~65 frames) to `slides/appendix_methods_detail/`.
      Move, don't delete
- [ ] Merge `causal_inference.tex` block A with the first ~45 frames of `advanced.tex`
- [ ] Compress meta-analysis 10 → 5; move synthetic control to the appendix
- [ ] Build the credibility block (forking paths, Breznau, pre-registration) and the ethics block
- [ ] Build the closing prediction-revisited frames (Fragile Families) and the return to the triad
- [ ] Present **Villamil & Balcells** as your own worked DiD, including what you'd do differently
- [ ] Source figures: Breznau distribution-of-estimates, Fragile Families scatter

### By Oct 23 — workshop materials

- [ ] Pair the groups once Memo 5 is in (Oct 19), and circulate each sketch to its partner
- [ ] Write the peer-review sheet (written comments, handed in, graded on the group)
- [ ] Slot schedule: ~12 slots, 12 min + 8 min, hard stop
- [ ] Required-slides list, including **"the threat I can't solve"**

---

## Carried over

- [x] **Done** - 40-student pass on the session-1 teaching notes. Think-pair-share replaces "let
      silences sit" at the variation block, the three unit-of-analysis graphs are now a show of
      hands, and a general note on method was added to the front of the guide.
- [ ] **Teaching notes for sessions 3–5.** Sessions 1 and 2 have them.
- [ ] **Em-dash pass on the plan docs.** Done for the syllabus, both decks, and both teaching
      guides (which now share a heading style). Still outstanding: ~143 in `08_outline_2026.md`
      and scattered through the other plan files. These are internal working documents, so this is
      cosmetic.
- [ ] **Decide when the new decks replace `res_design/slides/`.** The old decks are untouched and
      still what `index.md` would link to.
- [ ] `index.md`: add slide PDF links as each deck is finished.
- [x] **Done** — Aula Global course ID in the website sidebar, now `id=208247`. Sidebar year fixed to
      2026, schedule table reduced to 13px.

## Housekeeping

- [ ] Adopt the two-wrapper notes pattern in decks 2–5 as they are built (session 1 already has it;
      `slides/beamer_notes_preamble.tex` exists and is reusable).
- [ ] Build artifacts (`.aux`, `.nav`, `.snm`, `.toc`, `.fdb_latexmk`, `.synctex.gz`) are cluttering
      both slide trees, and there is a stray `files/survivorship_bias copy.png`. **Ask before
      deleting**, and check `.gitignore` covers them going forward.
