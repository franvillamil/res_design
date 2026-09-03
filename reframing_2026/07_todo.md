# Triaged task list

**Today: 2026-09-03. Session 1 is Tue Sep 15 — 12 days.** Sessions 2–5 fall on Sep 22,
Sep 29, Oct 6, Oct 13; workshop Mon Oct 26.

The full reframing in `03_sessions.md` is roughly 2–3 days of concentrated work. That is
probably not available before Sep 15, but it doesn't need to be: only session 1 has to be
ready on Sep 15, and the sessions that change most (2, 3, 5) are 19, 26 and 40 days out. The
sequencing below exploits that.

---

## Before Sep 15 — must do (≈half a day)

- [ ] **Update the syllabus.** `syllabi/current/syllabus_research_design.tex` still says
      *Fall 2025* and Sep 16; `index.md` already has the 2026 dates. Fix the term, the dates,
      and the session table.
- [ ] **Set the new assessment weights** in the syllabus (essay 55%, participation dropped) —
      per the instructor note already at the top of that file. This has to be right on day one
      because students are told the weights in session 1.
- [ ] **Add the milestone ladder to the syllabus** (`06_assessment.md`) — also has to be
      announced in session 1, and it's five table rows.
- [ ] **Swap the session 2 reading** to Lazer et al. (Google Flu) and move Müller-Crepon to
      session 3, in both the syllabus and `index.md`.
- [ ] **Session 1 deck edits** — the only deck needed on Sep 15:
  - add the **triad** frames (describe / explain / predict + one anchor example each) — ~4
  - add the **five design questions** course-map frame — 1
  - add **designed vs. found data** — 1
  - add the **audit a claim** exercise — 3–4
  - compress `Research process in detail (1)–(6)` to 2
  - merge the two `Empirical Research` frames; fix `Key ingredientes`
  - update the calendar and assessment frames to the new weights and dates
- [ ] **Update the session-outline text** in the syllabus for sessions 2–5 to match the new
      titles and content. Students will hold you to this document, so it's worth getting the
      new framing in there now even if the decks come later.

## Before Sep 22 (session 2) — the big build (≈1 day)

Session 2 is the most-changed session. Nine days after session 1.

- [ ] Split `2_basics_quantitative_data/` into two lecture folders:
  - `2_questions_and_data/` — RQ/theory front half + the new provenance block
  - `3_measurement/` — concepts/measurement/description back half
  - Renumber `3_causality` → `4_causality`, `4_causal_inference_methods` +
      `5_advanced_topics` → `5_designs_and_credibility`.
- [ ] Move **levels of explanation (macro/meso/micro)** from the measurement material into the
      theory block.
- [ ] Build the **data provenance block** (~28 frames) per `03_sessions.md`:
      designed vs. found → Salganik's ten characteristics → Xbox → Google Flu → algorithmic
      confounding → post-API access → ethics preview.
- [ ] Source figures for **Google Flu** and **Xbox** (see `04_examples.md`).
- [ ] Replace or refresh the **Charlie Kirk** running example; add a `% SWAPPABLE:` comment
      marking the slot for future years.

## Before Sep 29 (session 3) — measurement session (≈half a day)

- [ ] Assemble `3_measurement/` from the existing back-half frames — most of this already
      exists and just needs to stop being rushed.
- [ ] Add the **classifier-output-as-a-variable** frames, using the Barberá images already in
      `img/`.
- [ ] Add **Obermeyer et al.** proxy-failure frames; source the figure.
- [ ] Expand the **description** block: add Chetty social capital and culturomics + its
      critique; source figures.
- [ ] Keep the wartime civilian deaths example as the closing synthesis.

## Before Oct 6 (session 4) — causality trim (≈2 hours)

Mostly deletion, so this is the cheapest session to prepare.

- [ ] Trim `causality.tex` from 91 → ~55 frames: compress the PO example 8 → 5, `Off topic:
      Controlling` 4 → 1, move overflow to `\section{Appendix}`.
- [ ] Restore the commented-out **prediction** frames (one or two, since the triad now carries
      the load from session 1).
- [ ] Add **platform experiments and their limits** (Guess + Bail) and the
      **interference-is-normal-in-CSS** frames; source the Bail figure.

## Before Oct 13 (session 5) — the cut and the new block (≈1 day)

- [ ] **Cut the `Causal methods again, in detail` section** from `advanced.tex` (~65 frames).
      Move to `slides/appendix_methods_detail/` and offer as optional self-study. *Move, don't
      delete* — it's good material in the wrong course.
- [ ] Merge `causal_inference.tex` Block A (design templates, ~28 frames, already
      well-calibrated) with `advanced.tex`'s first ~45 frames.
- [ ] Compress the meta-analysis block 10 → 5 frames.
- [ ] Build the **credibility block**: forking paths → Breznau many-analysts →
      pre-registration and "your essay is a pre-analysis plan".
- [ ] Build the **ethics block**: informational risk, consent at scale, Emotional Contagion,
      re-identification, GDPR.
- [ ] Build the **closing prediction-revisited** frames (Fragile Families) and the
      return-to-the-triad slide.
- [ ] Source figures for Breznau and Fragile Families.
- [ ] Move synthetic control into the appendix section.

## Before Oct 26 (workshop)

- [ ] Assign peer reviewers and circulate M5 sketches ~3 days ahead.
- [ ] Write the peer-review sheet (two written comments, handed in — this is the 10%).
- [ ] Fix the slot schedule: 12 min / 8 min / hard stop.
- [ ] Prepare the required-slides list, including **"the threat I can't solve."**

## Housekeeping, any time

- [ ] `index.md` — update session titles to the new framing; un-comment the slide and reading
      links as decks are built.
- [ ] Add `slides/beamer_notes_preamble.tex` and adopt the two-wrapper notes pattern from
      `CLAUDE.md`. **All five decks currently have zero `\note{}`.** Best done incrementally as
      each deck is rewritten rather than as a separate project.
- [ ] Adopt the `% ----` frame-separator convention (per `CLAUDE.md`) in rewritten decks.
- [ ] Add a one-line annotation to the syllabus textbook list saying which books are for *this*
      course and which are for the methods courses that follow. Add Salganik.
- [ ] Consider deleting the LaTeX build artifacts under `slides/` (`.aux`, `.bcf`, `.fls`,
      `.fdb_latexmk`, `.snm`, `.nav`, `.toc`, `.synctex.gz`) and the stray
      `files/survivorship_bias copy.png` — **ask first**, and check that `.gitignore`
      covers them going forward.

---

## Minimal-change variant

If the full restructuring isn't happening this year, **three insertions and one deletion** get
most of the CSS benefit while leaving the session order and all five decks where they are:

1. **Session 1: add the triad** (describe / explain / predict, ~5 frames). Cheapest change with
   the largest effect on how students think, and it makes every later session easier to place.
2. **Session 2: insert a 40-minute data provenance block** (~15 frames) — designed vs. found
   data, Salganik's ten characteristics, Google Flu, algorithmic confounding. Pay for it by
   cutting the measurement material that currently gets rushed anyway, and by swapping the
   reading to Lazer et al. Accept that description stays thin this year.
3. **Session 5: delete the `Causal methods again, in detail` section** (~65 frames) and spend
   the freed 60–70 minutes on credibility (Breznau, pre-registration) and ethics. This is the
   single highest-value change in the whole plan and it is a *deletion*, so it costs nothing
   but the decision.
4. **Add the milestone ladder** (`06_assessment.md`). Pure syllabus text, no slides, and it is
   the thing most likely to improve the essays.

That is realistically one day of work, fits in the 12 days available, and leaves the full
restructuring — splitting session 2, promoting measurement to its own session — as next
year's job with this document already written.
