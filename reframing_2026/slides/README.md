# Reframed slide decks (2026)

New decks built from [`../08_outline_2026.md`](../08_outline_2026.md). Kept separate from
`res_design/slides/` so the existing decks stay untouched until each new one is signed off.

## Structure

```
slides/
  beamer_preamble.tex        -> symlink to ../../slides/beamer_preamble.tex
  beamer_notes_preamble.tex     new: pgfpages 4-on-1 config for the notes version
  img                        -> symlink to ../../slides/img
  1_introduction/
    intro_body.tex                 all frames + \note{} — this is the file you edit
    introduction.tex               wrapper: the presentation
    introduction_notes.tex         wrapper: handout mode, slides + notes side by side
    teaching_notes_session1.md     extended teaching guide (slide-by-slide, timings, triage)
    teaching_notes_session1.pdf    ^ compiled
```

Three levels of notes, from shortest to longest: the `\note{}` in the body (a few lines, for
glancing at while presenting) → `introduction_notes.pdf` (those notes beside each slide) →
`teaching_notes_session1.pdf` (the full guide: what to say, what to ask, expected answers,
timings, and what to cut when running late).

Rebuild the guide with:

```sh
sed 's/≠/$\\neq$/g' teaching_notes_session1.md > /tmp/tn.md
pandoc /tmp/tn.md -o teaching_notes_session1.pdf --pdf-engine=xelatex --toc --toc-depth=2 \
  -V geometry:a4paper -V geometry:margin=2.2cm -V colorlinks=true -V fontsize=11pt
```

`intro_body.tex` is shared by both wrappers, so slides and notes can never drift apart.

## Building

```sh
cd 1_introduction
pdflatex introduction.tex          # run twice — \againframe needs the second pass
pdflatex introduction_notes.tex    # notes version, built independently
```

## Conventions

- 4:3, every frame wrapped in `% ----` separators, `\note{}` after every frame
  (empty is fine — it keeps the slide/note pairing in sync in the 4-on-1 layout).
- Images use the tikz `remember picture, overlay` pattern, not `\imageframe{}`,
  because `\imageframe` cannot carry a `\note{}`.
- Session 1 is 67 frames including section roadmaps. Session 1 has no paper
  discussion, so it has ~140 min of lecture time; later sessions should target ~55.
