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

## Before each session — the decks

Only session 1 exists so far. Each new deck goes in `reframing_2026/slides/`, built from
[`08_outline_2026.md`](08_outline_2026.md) and adjusted for how the previous session actually went.

### By Sep 22 — session 2, "Questions and data" (the big build)

- [ ] Split `2_basics_quantitative_data/` into the RQ/theory front half plus a new provenance block
- [ ] Build the data-provenance block (~28 frames): designed vs found, Salganik's ten
      characteristics, Xbox, Google Flu, algorithmic confounding, post-API access, ethics preview
- [ ] Move **levels of explanation** (macro/meso/micro) here from the measurement material
- [ ] Source two figures: **CDC vs GFT** over-prediction, and the **Xbox** demographic skew
- [ ] Mark the Charlie Kirk slot `% SWAPPABLE:`

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

- [ ] **Apply the 40-student pass to the session-1 teaching notes.** They still say "let silences
      sit" (line ~396) and treat the Socratic blocks as if this were a seminar of twelve. With 40
      people, silence gets filled by the same three voices. Needs think-pair-share throughout:
      pose, 60 seconds silent, 2 minutes in pairs, take 3–4 contributions — and call on *pairs*,
      not individuals. Also: make the three unit-of-analysis graphs a show of hands.
- [ ] **Teaching notes for sessions 2–5.** Only session 1 has one.
- [ ] **Em-dash pass on the plan docs and teaching notes.** Done for the syllabus and slides; the
      rest still has them (167 in the session-1 teaching notes, 143 in the outline).
- [ ] **Decide when the new decks replace `res_design/slides/`.** The old decks are untouched and
      still what `index.md` would link to.
- [ ] `index.md`: add slide PDF links as each deck is finished.

## Housekeeping

- [ ] Adopt the two-wrapper notes pattern in decks 2–5 as they are built (session 1 already has it;
      `slides/beamer_notes_preamble.tex` exists and is reusable).
- [ ] Build artifacts (`.aux`, `.nav`, `.snm`, `.toc`, `.fdb_latexmk`, `.synctex.gz`) are cluttering
      both slide trees, and there is a stray `files/survivorship_bias copy.png`. **Ask before
      deleting**, and check `.gitignore` covers them going forward.
