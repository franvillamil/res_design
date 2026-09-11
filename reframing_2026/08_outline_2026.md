# Research Design 2026 — logistics + detailed lecture outline

Drafted 2026-09-09, revised 2026-09-10. **Session 1 is Tue Sep 15 — 5 days out.**
Sessions: Sep 15, Sep 22, Sep 29, Oct 6, Oct 13 (18h–21h) + workshop Mon Oct 26 (15h–21h).

This document is for you to check before any slides get built. Part 1 is logistics — grading,
assignments, and the paper discussion. Part 2 is the session-by-session outline. Part 3 is the
build order and what I need from you each week.

Two of your own notes drive most of Part 1:

> *"No written essay, PRESENT in class. No tiene sentido con AI (maybe make them write a backup
> document of max 2000 words or 5 pages) [...] assign discussant to each, 10min + 5min"*
> — `Teaching/AQM2 2027.md` (a different course, but the problem and the fix both transfer)

> *"En cada práctica, dejar 15-20min al final para responder a 1-2 preguntas que se hagan en el
> momento [...] se trata de incentivar que vengan a clase, que atiendan en la práctica, y premiar
> la asistencia continuada. **cambiar esto por lo de entregar los memos**"*
> — `Teaching/ideas_practicas.md`

---

# PART 1 — LOGISTICS

## 1.1 Assessment

Two of your notes pointed in opposite directions here. The syllabus header says **raise the essay
to 50–60%, drop participation**; the AQM2 note says **written essays don't make sense any more
with AI**. Research Design is where that bites hardest, because the final essay is a research
design proposal — exactly the genre an LLM writes most fluently and most emptily.

The resolution is not to abandon written work but to **stop putting weight on writing done
unsupervised, and put it on what happens in the room instead**. Concretely: the paper commentary
is dropped, the workshop presentation becomes the largest single component, and everything except
the essay is graded on a pair or a group — which is also the only thing that scales to 40
students.

### The scheme

| Component | % | Graded as | Change |
| :--- | ---: | :--- | :--- |
| Design memos (5) | 15 | pair (M5: group) | **new** — replaces participation |
| Workshop presentation | 35 | group | +15 |
| Workshop peer feedback, on a paired group | 10 | group | same weight, new format |
| Final essay | 40 | **group only** | same weight, groups now required |
| Paper commentaries | — | — | **dropped**, now voluntary |

Current (2025) for comparison: participation 15 / paper reviews 15 / presentation 20 /
feedback 10 / essay 40.

**The essay is group-only this year.** The presentation, the peer feedback and the essay are one
piece of work seen three times; letting the essay split off from the group that presented it would
not make sense, and at 40 students individual essays are 40 documents rather than 12.

**Paper commentaries are gone as a graded item.** A short written commentary on a paper is
close to the ideal task for an LLM — bounded, generic, and invisible to you whether it was
thought about or generated. Grading it bought a paper trail and not much else. Reading stays
expected, commentary stays welcome, neither is marked. §1.3 explains what replaces the
accountability.

**No discussant role.** It was the other half of that mechanism and it does not survive at 40
students anyway — three papers cannot give forty people a turn.

**The workshop presentation is the centre of gravity at 35%.** It is the one thing that cannot
be outsourced: a group standing in front of the room, taking questions about a design they have
to have understood. Together with peer feedback, 45% of the grade is earned on the workshop day.

**Everything except the essay is now graded on a pair or a group.** That is a deliberate trade:
you give up some individual resolution and you get work you can actually evaluate at this class
size, plus feedback that comes from a document rather than from whoever speaks loudest.

*One risk worth knowing you are taking:* 45% of the grade lands on a single day. Group work
absorbs an individual illness, but decide in advance what happens if a whole group misses it.

### The memos (M1–M5)

Five short design exercises, half a page, set at the end of a session and **submitted on Aula
Global by Monday night** — formally Tuesday 00:00, i.e. before the following session. M1–M4 are
written **in pairs**; M5 is written by the **workshop group**. They are the only thing handed in
all term before the essay.

| # | Set | Due | Task |
| :--- | :--- | :--- | :--- |
| **M1** | Sep 15 | Mon Sep 21 | Audit a claim: its goal, the data it would need, your first doubt |
| **M2** | Sep 22 | Mon Sep 28 | A found data source: who made it, who's missing, what it can and can't answer |
| **M3** | Sep 29 | Mon Oct 5 | One concept, operationalised two ways. Which, and what the other one catches |
| **M4** | Oct 6 | Mon Oct 12 | A causal claim: the comparison that would license it, and what would bias it |
| **M5** | Oct 13 | **Mon Oct 19** | *Their group's project*, one page — goes to the paired group |

**They are exercises, not essay fragments.** M1–M4 are about someone else's problem, which is far
easier than practising on your own — there is nothing at stake in admitting the design does not
work — and it means nobody has to commit to a topic in week 1 and defend it for five weeks.
**M5 is the deliberate exception and the bridge**: the group's own project, and what the paired
group reviews. Note it breaks the pattern twice — written by the workshop group rather than a
pair, and due **Mon Oct 19**, a week earlier than the rule would give, because the partner group
needs time to read it before the workshop. Without M5 the workshop is the first time anyone has
looked at their design.

**Each memo runs off that week's lecture**, not off the reading. The prompts are generic enough
to be answered from anything — a news claim, a dataset they can reach — which is the point: the
skill is transferable, and they get to choose material they actually care about.

**Grading cost at 40 students:** 20 pairs at half a page, marked done / done-well — about 25–35
minutes a week.

**The AI question, honestly.** Written at home, these are not AI-proof. What limits the damage is
that they are low-stakes (4% each), that the prompts ask for a judgement rather than a summary,
and that the real assessment is the workshop, where a group has to answer questions about a design
in front of the room. If pairs start handing in generated text, the cheap fix is ten minutes at the
start of the next session having two pairs talk through their memo.

## 1.2 What students already know

Worth stating explicitly, because it sets the scope line and it is easy to get wrong.

This is an MA in Computational Social Science with a **full methods sequence of its own** —
statistics, survey methods, data science, R programming, causal inference, data mining. Some of
those run *in the same semester* as Research Design; the rest come later. Before this course,
students have taken a **3 ECTS basic statistics course**.

So assume: they know descriptive statistics, and roughly up to OLS. Not deeply — don't expect
much — but the vocabulary is not new. Two consequences:

- **You don't have to avoid the words.** "Regression", "control variable", "coefficient" can be
  used as known terms. The scope line forbids *teaching estimation*, not naming it. A slide can
  say "controlling is blocking a back door, not adding a regressor" and land, because they know
  what a regressor is.
- **The scope line gets a much better justification.** It is not "you're not ready for this" —
  which is both untrue and patronising — it is "there is a whole course on this in your own
  programme, and some of you are in it right now." That reframes every cut in this plan from a
  simplification into a division of labour, and it is the honest version.

This is also the strongest argument for the session 5 cut: ~65 frames re-teaching FE/DiD/RDD/IV
with regression machinery duplicates a dedicated causal inference course these students already
have access to.

## 1.3 The paper discussion

You said it never really worked. Looking at the decks, the reason is structural rather than
about the papers.

**The diagnosis.** In every deck, the "Paper discussion" section is a single frame containing one
image — the paper's first page. `basics_quant_data.tex` ends with `\imageframe{../img/mullercrepon}`
and that is the entire apparatus for a 45-minute block. Students were asked for "a few discussion
points, no summary" with no model of what that looks like, no roles, and no prompts on screen.
With forty students in an evening slot, that reliably produces four people who read it, one who
talks, and a slow slide into you summarising the paper yourself.

The graded commentary was the accountability mechanism, and it is now gone (§1.1). So the block
needs a different one.

**1. Accept that there is no enforcement, and design the session for that.** No commentary, no
mark, no memo tied to the reading — a deliberate choice, since a written commentary on a paper is
close to the perfect LLM task and marking it would tell you nothing about who read it. What
replaces enforcement is making the session genuinely unpleasant to sit through unread: say so
plainly once, in week 1, without threatening, and then run the discussion in a way that assumes
they read it. Choosing short papers is doing most of the work here — Google Flu is three pages.

**2. Put the prompts on the slides.** Three or four specific questions per paper, on screen,
prepared in advance — not "what did you think?" These are drafted per paper in Part 2.

**3. Structure the 25 minutes** — and it is 25, not half the session — around pair and
small-group work, because open-floor discussion does not function at this class size:

```
 3 min  You: why this paper is here, and which design question it illustrates
 6 min  Prompts 1-2 on screen. Pairs, 3 min, then 3-4 contributions
 6 min  Prompts 3-4, same shape
 7 min  Groups of four: "redesign it" — one change you'd make and what it buys
 3 min  You: close on the design lesson
```

**This number drives everything else.** At 25 minutes rather than 45, a three-hour session needs
roughly **120 minutes of lecture content**, which is 20 minutes more than an earlier version of
this plan assumed. Sessions 3-5 must be built against 120, not 100.

The "redesign it" step matters more than it looks. Asking students to *evaluate* a published
Science paper is intimidating and produces deference; asking what they would change is a question
they can actually answer, and it is the same skill the workshop and the essay test.

**Why no discussant.** It was in an earlier draft of this plan, borrowed from your AQM2 note. It
does not survive at 40 students: three papers cannot give forty people a turn, so it would create
a graded role for a dozen people and a spectator role for everyone else. Presenting the design
yourself is faster, more reliable, and lets you point the discussion where you want it.

### Which papers

Current set: Müller-Crepon (s2), Guess et al. (s3), Villamil & Balcells (s4). All three are good
and I'd keep all three — but **move them**, so each lands in the session whose design question it
answers, and swap one in for the new session 2.

| Session | Paper | Why here |
| :--- | :--- | :--- |
| 2 — Questions & data | **Lazer, Kennedy, King & Vespignani (2014), "The Parable of Google Flu"** *(new)* | 3 pages, zero equations, and it *is* the session: drift, measurement validity, the platform as part of the data-generating process. The best CSS cautionary tale available and the easiest paper in the course to have opinions about. |
| 3 — Measurement | **Müller-Crepon et al., "Roads to Rule, Roads to Rebel"** *(moved from s2)* | It's fundamentally an operationalization paper — a relational concept of state capacity built out of road networks. In the measurement session the discussion becomes "what concept, what operationalization, what would you have done instead," which is a far better use of it than a general discussion of quantitative research. |
| 4 — Causality | **Guess et al. (2023), feed algorithms** *(moved from s3)* | Students can skip the statistics entirely and still discuss the design, which is the point. Now sits in the session that gives them the vocabulary for it. |

**Villamil & Balcells** moves out of the graded-commentary slot and becomes a **worked example you
present in session 5** — you walk them through your own design, including what you'd do
differently. That is more valuable as a demonstration than as a paper they critique in front of
you, and it frees session 4 for Guess.

Two optional additions worth knowing about, both from your Obsidian note:

- **"What Did We Learn About Political Communication from the Meta2020 Partnership?"** — the
  retrospective on the Guess et al. programme. Two pages as a companion in session 4, and it
  raises industry–academy data access, which nothing else in the course covers.
- **The Gelman post** on crude political agendas in published papers — good as a 5-minute opener
  in session 5's credibility block, not as an assigned reading.

## 1.4 Other logistics decisions

- **Syllabus needs updating regardless** — it still says *Fall 2025* and Sep 16. `index.md`
  already has the correct 2026 dates.
- **Deck length cap: ~60 frames.** A 3h slot holds ~120 min of lecture once you account for
  arrival, a break, ~25 min of paper discussion and the closing brief. Current decks run
  39/86/91/58/110. Everything over the cap goes after `\section{Appendix}` and is skipped live.
- **Presenter notes.** No deck in this course has a single `\note{}`. As decks get rebuilt I'll
  use the two-wrapper pattern from `CLAUDE.md` so you get a slides+notes PDF for each.
- **Add Salganik's *Bit by Bit*** to the textbook list — free online, no statistics, and the only
  book on the list actually about computational research design rather than estimation. Worth a
  one-line annotation saying which books are for *this* course and which belong to the methods
  courses in the programme.

---

# PART 2 — THE FIVE LECTURES

## The spine

Two layers, both introduced in session 1 and revisited weekly.

**The triad — three research goals.** Describe / explain / predict. Each has different success
criteria (coverage and validity / identification / out-of-sample performance), which is what
motivates everything else in the course. Most bad student designs are a descriptive question with
a causal answer bolted on, or a causal question answered with a predictive design; naming the
three goals lets you diagnose that in ten seconds during the workshop. It also restores the
prediction material currently sitting commented out in `causality.tex`.

**The five design questions** — one per session, each phrased as something a student must be able
to answer *about their own project*:

| # | Session | The question |
| :--- | :--- | :--- |
| 1 | Introduction | **Why design at all?** |
| 2 | Questions and data | **What's the question, and where would the data come from?** |
| 3 | Measurement | **Do the numbers mean what you think they mean?** |
| 4 | Causality | **What comparisons can we use to establish causal claims?** |
| 5 | Designs and credibility | **What variation can you exploit, and how far does the answer travel?** |

One slide, shown in session 1 and re-shown at the start of each session with the current question
highlighted. Costs one slide a week and is what makes five lectures feel like one argument.

**The scope line**, stated once so it can be enforced on every slide:

> Teach **what variation a design exploits and what has to be true for it to work.** Never teach
> how the corresponding quantity is estimated.

So DiD is "compare the change in the treated group to the change in a group that should have
moved in parallel, and the design lives or dies on whether that parallel movement is credible" —
not a two-way fixed effects specification. This is the line session 5 currently crosses.

### Session shape (all lecture sessions)

```
18:00–18:12  arrival; recap; the five-questions slide
18:12–19:10  block A (~25-28 frames)
19:10–19:20  break
19:20–20:20  block B (~25-30 frames)
20:20–20:45  paper discussion (~25 min; pairs + small groups)
20:45–21:00  set the memo, next week, questions
```

---

## Session 1 — Why design at all? (Tue Sep 15)

**Source:** `1_introduction/introduction.tex`, 39 frames — the closest to right already.
**Target:** ~52 frames. **This is the only deck needed on Sep 15.**

### Block A — what empirical research is (~26 frames, nearly all existing)

1. **Challenger cold open, in full.** An explanatory question, an obvious-in-hindsight selection
   problem, no statistics required, images already in `img/challenger*.jpg`. Keep verbatim — it's
   the best thing in the course. Pair it with the survivorship-bias planes image as the
   one-picture version of the same lesson.
2. **Types of research** — theoretical/empirical, qual/quant. Keep.
3. **Empirical research as exploiting variation.** Merge the two near-identical
   `Empirical Research` / `Empirical research` frames.
4. **Empirical evidence and claims** + `Opinions about claims?` + `Other types of inference`.
   Keep — this is where the Socratic norm gets set.
5. **`A few questions on variation`** — why we care about variation, the "few observations"
   critique, why statistics at all, where the variation is in attribution science. Keep all of it.
6. **`Description is not easy either`.** Keep — and it now points forward to session 3.
7. **`Not only about data or empirical evidence`** — theses fail on the non-data part. This is the
   course's thesis statement; **move it earlier**, right after the Challenger.
8. **Unit of analysis**, including `What's the unit of analyses in the data behind this graph?`.
   Keep all three frames.

### Block A additions (**new**, ~6 frames)

9. **The triad: describe / explain / predict.** One frame defining it, then one frame per goal
   with a CSS anchor example:
   - *Describe* — Chetty et al. social capital atlas
   - *Explain* — Guess et al. feed experiment (forward-reference to their week-4 paper)
   - *Predict* — Blumenstock et al. wealth from phone metadata (`img/blumenstock*.png` already
     exist)

   **The single most important addition in the course.** Everything later hangs off it.
10. **The five design questions** — the course map. One frame, reused every week.
11. **Where does data come from?** First pass: *designed* (survey, experiment, coding scheme) vs.
    *found* (traces, admin records, scraped text). One frame, plants the flag for session 2.

### Block B — the research process, and two real theses (~14 frames)

12. **`Empirical Research process`** and **`Key ingredientes`** (fix typo). Keep.
13. **`Research process in detail (1)–(6)` → compress to 2 frames.** Six is three too many for
    week 1 — they have no hooks to hang the detail on yet, and it returns naturally in weeks 2–5.
    Keep `Simplyfing` as the payoff.
14. **Two real TFMs from this programme** (**new**, ~4 frames) — from your Obsidian note, and I
    think this is the best available week-1 material after the Challenger:
    - *Hotel bookings.* Data arrives from the hotel: bookings, and whether people actually came.
      The unit of analysis is the booking. The **motivation** is "how do we increase business for
      the hotel?" — and the research question is left blank on the slide. Students feel the gap
      before it gets a name, which sets up "motivation is not a question" a few frames later. The
      payoff: the hotel wanted more business, the data supports a no-show question, and those are
      not the same thing.
    - *Spotify music collaborations.* API data turned into a network structure; variables are
      time and genre. **What questions can we ask here, and how would you approach it?**

    These do work no published example can: they are theses from students like them, they start
    from *data in hand* rather than from a question — which is how most of this cohort will
    actually start — and they set up the whole course as the answer to "so now what?"
15. **"Audit a claim" exercise** (**new**, ~15 min, 3–4 frames). Three claims on screen: one
    descriptive, one causal, one AI/CSS-flavoured. In pairs: what's the goal, what would the data
    have to look like, what's the first thing you'd doubt. Gets them talking in hour one, which
    sets the working norm for a class of 40, where open-floor discussion does not function.

### Housekeeping (~8 frames)

Logistics, the new assessment weights, calendar, textbooks (+ Salganik), the memo ladder
explained, papers to read, next week's reading. **Say explicitly that the final essay is a
pre-analysis plan** — naming the genre measurably improves what students hand in.

- **No reading for this session** — students aren't reachable before the course starts. The Fry
  piece is dropped; Salganik ch. 1 can be mentioned in passing as optional.
- **No paper discussion this week** — use the time for the audit exercise and logistics.
- **M1 set** (due Mon Sep 21, in pairs): the audit exercise, extended — take a claim of your own choosing, name the goal, say what data would be needed, and name the first thing you would doubt.

---

## Session 2 — Questions, and where the data comes from (Tue Sep 22) — **rebuilt**

**Source:** front half of `basics_quant_data.tex` + substantial new material.
**Target:** ~55 frames. **This is the session that changes most.**

### Block A — from topic to answerable question (~26 frames, mostly existing)

1. **The RQ ladder**, both versions: `Topic > RQ > Theory`, refined to
   `Previous evidence > argument > RQ > Hypotheses`, then the honest version with the "anecdotal
   argument" step. Keep as-is — the most useful twenty minutes in the course for students who've
   never written a design.
2. **`Good RQs, in brief`** and **`Good RQs, more in detail`** — answerable / relevant / feasible
   / narrow, with the three-way contrast of good vs. too broad vs. many-questions-in-one. Keep.
3. **`Example on generating RQs`** — the school peers vs. teaching quality argument. Keep as a
   live exercise, don't lecture it.
4. **`Stories, RQs, and theories`** / **`Generating theories`** — the abstraction ladder, and "if
   you can't tell a story out of the theory you're not there yet." Keep.
5. **Levels of explanation (macro/meso/micro)** — **moved here** from the measurement material.
   It belongs with theory-building.
6. **The running current-event example.** The Charlie Kirk / political violence decomposition
   works but dates fast. Keep the *structure* — topic → intuition → main RQ → RQ1/RQ2/RQ3 →
   theory — and swap the topic for whatever is salient in Sep 2026. The pedagogical point is that
   it splits into a descriptive, a distributional and a causal sub-question, not the topic. I'll
   mark the slot `% SWAPPABLE:` in the .tex so it's obvious what to replace each year.
   **→ I need you to pick the 2026 topic, or tell me to keep Kirk.**

### Block B — the data-generating process, socially and technically (~28 frames, **new**)

This is the CSS core of the course and the largest gap in the current version. Right now
computational social science enters only as *examples*; nothing treats the design problems
specific to computational data. Every item below is teachable with zero statistics and zero code.

1. **Designed vs. found data** (2 frames). Designed data answers a question you had. Found data
   answers a question *someone else* had, or no question at all. Everything else follows.
   Callback to the two TFMs from week 1 — both are found data.
2. **Salganik's ten characteristics of big data sources** (~6 frames). Big / always-on /
   non-reactive as the attractions; incomplete / inaccessible / non-representative / drifting /
   algorithmically confounded / dirty / sensitive as the design problems. A research-design
   framework with no statistics in it, and a checklist they'll use for the rest of the MA.
3. **Non-representative by construction — the Xbox survey** (Wang et al. 2015, ~3 frames). An
   absurdly unrepresentative panel — young, male, gamers — producing good 2012 election forecasts.
   The lesson: representativeness is a property of the **design**, not of the sample. Cuts against
   both "my Twitter data isn't random so it's useless" *and* "I have 10 million observations so it
   doesn't matter."
4. **Drift — Google Flu Trends** (Lazer et al. 2014, ~4 frames). A measure that silently stops
   meaning what it meant, partly because the platform changed underneath it. Doubles as this
   week's paper.
5. **Algorithmic confounding** (~3 frames). The platform is in your DAG. Ranking and recommendation
   shape the behaviour you observe, so an observed association can be a property of the *system*
   rather than of the people. Forward-references DAGs in session 4.
6. **Access and the post-API age** (Freelon 2018, ~2 frames). What happens to a design when the
   source can revoke access, and what that means for reproducibility. Practical: several of them
   will propose scraping something for the essay.
7. **Ethics preview** (1 frame). Informational risk and consent at scale, pointing to the full
   treatment in session 5. Flagged early because essay proposals get written in week 4.

### Paper: Lazer et al., "The Parable of Google Flu" *(new)*

*Discussion prompts:* What was GFT actually measuring? Whose behaviour changed — the public's, or
Google's? Would more data have fixed it? What would you have had to do in 2009 to see this coming?
*Redesign step:* you have Google's search logs and CDC data in 2009. Build something that doesn't
break the same way.

**M2 set** (due Mon Sep 28, in pairs): take a source of found data you could realistically get hold
of. Who produced it and why? Who or what is missing from it? Name one question it could answer
well and one it could not.

---

## Session 3 — Do the numbers mean what you think? (Tue Sep 29) — **promoted to a full session**

**Source:** back half of `basics_quant_data.tex` (~40 frames of good material already) + new CSS
measurement content. **Target:** ~55 frames.

Currently this is crammed into the back of an 86-frame session that also covers questions, theory
and data. In practice something gets dropped every year, and it's almost certainly description —
which is then never taught, despite being the goal of a large share of computational work.

### Block A — concepts and measurement (~30 frames)

1. **Concepts as building blocks**; **rule-based vs. ideal types**; the household exercise; the
   political-violence exercise. Keep — rare to see taught explicitly. Run the concept stress-test
   live (10 min): define "political violence" as a rule-based concept, then as an ideal type.
2. **Operationalization**, with **operationalize ≠ measure**. Keep.
3. **`Importance`** — that a huge share of good quantitative work *is* conceptual and operational
   improvement, and that this matters especially for CSS. Keep and sharpen.
4. **Civil war / outbreak worked example.** Keep.
5. **The three measurement issues, proxies, latent variables, validation.** Keep.
6. **Democratic backsliding: objective indicators vs. V-Dem** — same concept, two
   operationalizations, different answers. Keep; best measurement frame in the course.
7. **Missing data and sampling bias as measurement problems.** Keep.

### Block A additions (**new**, ~8 frames)

8. **A classifier's output as a variable.** When your independent variable is a model's output — a
   topic label, a sentiment score, an estimated ideology — the measurement question doesn't go
   away, it gets harder, and the error is *not* random. Barberá's Twitter ideal points as the
   well-done case (`img/barbera_tw*.jpg` already exist): what was validated against what. Then the
   general rule: validate, always, against something external.
9. **A proxy that encodes what you didn't mean — Obermeyer et al. (2019)** (~3 frames). An
   algorithm allocating extra care used *healthcare spending* as a proxy for *health need*; since
   less was historically spent on Black patients at equal sickness, the proxy encoded the
   disparity. Pure construct validity, visible without any statistics, and it connects measurement
   to consequences in a way this cohort remembers.

### Block B — description as a first-class goal (~17 frames)

10. **Describing variables, describing relationships**, univariate and bivariate. Keep.
11. **`Is description useful?`** — expand from a rhetorical aside into the actual argument.
    Description has its own design requirements: coverage, comparability, a defensible
    denominator, a clearly defined population.
12. **Chetty et al. (2022) social capital** (**new**). 21 billion Facebook friendships turned into
    a descriptive atlas. Proves description isn't the consolation prize: real design decisions
    (what's the unit, what's the denominator, what population), real consequences.
13. **Culturomics and its critique** (**new**, ~3 frames). Michel et al. (2011) and Pechenick et
    al. (2015): a descriptive claim about culture that turns out partly to be a claim about what
    got digitised. Ties back to session 2's drift, and teaches corpus composition as a measurement
    problem.
14. **Wartime civilian deaths worked example.** Keep as the closing synthesis — concept →
    operationalization → measure → description, end to end.

### Paper: Müller-Crepon et al., "Roads to Rule, Roads to Rebel" *(moved here)*

*Discussion prompts:* State the concept in one sentence without using the word "roads." What does
the road network fail to capture? Tell me a story where their measure goes up and actual state
capacity goes down. *Redesign step:* same concept, different data source — what would you build?

**M3 set** (due Mon Oct 5, in pairs): take one concept and operationalise it two different ways. Which would you choose, and what does the other one capture that yours doesn't?

---

## Session 4 — What comparisons can we use to establish causal claims? (Tue Oct 6)

**Source:** `3_causality/causality.tex`, 91 frames → **trim to ~55**. Mostly deletion, so this is
the cheapest session to prepare.

### Block A — the logic of causal claims (~28 frames)

1. **Explaining relationships; potential outcomes; the fundamental problem of causal inference.**
   Keep. **Compress the worked example from 8 frames to ~5.**
2. **ATE vs. ATT**, including why ATT is often the useful one in practice. Keep.
3. **Experiments and their threats** — randomization failure, SUTVA, attrition, compliance,
   external validity. Keep all five.
4. **Prediction, briefly** (~2 frames, restored from the commented-out block). Since the triad
   carries the load from session 1, one or two frames suffice here: prediction needs no causal
   identification, which is exactly why it can't answer a causal question. Hofman, Sharma & Watts
   (2017) as the one-page reference.

### Block B — diagrams and the ways it goes wrong (~27 frames)

5. **DAGs**: what one is, mediation vs. moderation, the income-inequality example, DAGs and
   mechanisms. Keep.
6. **Front doors and back doors** — the full ten-frame build. Keep; it earns the length.
7. **`Off topic: Controlling` ×4 → 1 conceptual frame.** Controlling as blocking a back door, not
   as adding a regressor. The regression version belongs to the methods courses — several students
   are taking causal inference in parallel and will get it there.
8. **The usual suspects** — confounding, reverse causality, bidirectional causation, selection
   bias, collider bias (with the animation, `img/collider*`), post-treatment bias, and the closing
   `what should not be controlled for` recap. **Keep entirely — the strongest teaching block in
   the course.**
9. **"Find the back door" exercise** (15 min): a DAG on screen, students name what to control and
   what not to. The collider lesson only sticks if they try it.

### Block B additions (**new**, ~6 frames)

10. **Platform experiments as the modern experimental ideal — and their limits.** Guess et al. as
    the well-resourced case: real randomization, huge N, and *still* bounded external validity
    because it ran on one platform in one campaign. Pair with **Bail et al. (2018)**, where
    exposure to opposing views on Twitter *increased* polarization — a clean, memorable experiment
    whose finding contradicts the intuitive mechanism.
11. **Interference is the normal case in CSS, not an edge case.** SUTVA is introduced above as an
    assumption; make the CSS point explicitly, because it's where a lot of student designs on
    network or platform data quietly fail. If treating one user affects their contacts, there is
    no clean control group, and the design has to be built around that from the start rather than
    patched afterwards. No math needed.

### Paper: Guess et al. (2023), feed algorithms *(moved here)*

*Discussion prompts:* What exactly was randomized, and what wasn't? Who consented, and how does
that bound the estimate? If the effect is near zero, what have we learned — about feeds, or about
three months? Where does SUTVA fail here? *Redesign step:* you have Meta's cooperation for three
months. Same budget, better design?
*Optional companion:* the Meta2020 partnership retrospective — two pages, and it opens up
industry–academy data access, which nothing else in the course covers.

**M4 set** (due Mon Oct 12, in pairs): take a causal claim. What comparison would license it, and what would bias that comparison?

---

## Session 5 — What variation can you exploit, and how far does it travel? (Tue Oct 13) — **rebuilt**

**Source:** `4_causal_inference_methods/causal_inference.tex` (58 frames, the best-calibrated deck
in the course) + the *first* ~45 frames of `5_advanced_topics/advanced.tex`.
**Target:** ~55 frames.

> **The cut.** `advanced.tex` runs 110 frames. Its `Causal methods again, in detail` section —
> ~65 frames re-teaching controlling, matching, propensity scores, FE (×13), DiD (×13), RDD (×10)
> and IV (×8) *with* the regression machinery — is both duplicative of the design-templates block
> and outside the syllabus's own promise that "we will not cover statistical techniques." It also
> duplicates a course the programme already runs: there is a dedicated causal inference course, and
> some students are taking it this same semester. It gets **moved** to
> `slides/appendix_methods_detail/` and offered as optional reference. Move, don't delete — it's
> good material in the wrong course. This single deletion pays for almost every addition in the
> reframing.

### Block A — the design templates, at design level only (~28 frames)

1. **Exogenous variation as the unifying idea.** The existing framing is already right.
2. **`Overview of the 3 design templates`** — promote to the organizing frame of the block.
3. For each of **matching/controlling, fixed effects, DiD, RDD, IV**, exactly two things: *what
   variation does it exploit*, and *what has to be true for it to work*. ~3–4 frames each, no
   estimation. The existing frames already do this well; the job is resisting the pull toward the
   detailed versions.
4. Keep **Snow's cholera** (`img/did_snow.pdf`) and the **IV and RDD worked examples**.
5. **Villamil & Balcells street names as your own worked DiD** (`img/did_TJ*`, `img/map_graves*`)
   — moved out of the graded-paper slot and presented by you, end to end, **including what you'd
   do differently now.** Students seeing an instructor critique their own published design is
   worth more than another paper they critique in front of you.
6. Keep **`Alternative approaches to IV: build your own`** — good for essay proposals — and
   **`Controlling and exploiting exogeneity`** (designs combine).
7. **"Name the variation" exercise** (15 min): four published abstracts, students identify which
   design template. The best possible rehearsal for the workshop.

### Block B — generalization, credibility, ethics (~27 frames)

8. **External validity** (Egami & Hartman 2023) and **construct validity**. Keep.
9. **Meta-analysis as a design idea** — the natural-resources example, **compressed from ~10
   frames to ~5**. The point is that a literature is itself an object of study, and that funnel
   plots reveal what didn't get published.
10. **Temporal validity** (Munger 2023). Keep — especially pointed for CSS, where the platform
    under study may not exist in five years.
11. **Placebo and robustness tests as design logic**, not as statistics: what would have to be
    false for your result to be an artifact, and can you go look. Keep.
12. **Synthetic control → appendix.** Cover only if time.

### Block B additions (**new**, ~13 frames)

13. **Researcher degrees of freedom and forking paths**, then **Breznau et al. (2022)**: 162
    researchers, same data, same hypothesis, estimates spanning both signs. Nothing in it requires
    understanding a model — just the spread of the dots — and it's the strongest available argument
    for committing to a design before seeing results. (~4 frames. Gelman's post on published papers
    with crude political agendas works as the 5-minute opener here.)
14. **Pre-registration and pre-analysis plans** — and the explicit statement that **the final essay
    *is* a pre-analysis plan**. Reframes the assignment from "an essay about methods" to "the actual
    document a researcher writes." (~2 frames)
15. **Research ethics for computational work** (~4 frames). Informational risk, consent at scale,
    identifiability of "anonymous" data: Emotional Contagion (Kramer et al. 2014), the Tastes/Ties
    re-identification, Netflix de-anonymization, and GDPR as the framework these students will
    actually be bound by. There may be nowhere else in the MA where this is covered, and students
    proposing to scrape or link data need it *before* they write their essays.
16. **Closing: prediction revisited.** The **Fragile Families Challenge** (Salganik et al. 2020) —
    160 teams, 12,942 predictor variables, unusually rich longitudinal data, and the best models
    still predict life outcomes poorly. Return to the session-1 triad and close the course on
    calibrated humility rather than on a list of methods. (~3 frames)

**No assigned paper** — protect the time for essay questions. Optional: *Bit by Bit* ch. 6 (ethics).

**M5 set** (due Mon Oct 19, written by the workshop group): one-page sketch of the **group's** project — goal, question, units and variation, the measure, the comparison, the threat you can't solve. Goes to your paired group.

---

## Session 6 — Workshop (Mon Oct 26, 15h–21h)

Currently "12–15 slots, details in class." Proposed:

- **12 min present / 8 min discuss / hard stop.** 40 students in groups of 3–4 is ~12 slots;
  12 × 20 min = 4h, leaving room for breaks and a closing session.
- **This is 35% of the grade** — the largest single component, and the essay group is the same
  group. Say so when you set up the groups, because it changes how seriously the workshop is taken.
- **Required slides:** (1) the goal — describe / explain / predict; (2) the question; (3) units and
  the variation; (4) the concept and its measure; (5) the comparison; (6) **"the threat I can't
  solve."**
- The last one matters most. It removes the incentive to oversell, and a design with no
  acknowledged limitation is either trivial or not understood. It also produces much better
  discussion.
- **Presenters state the goal and the design template in the first minute.** Closes the loop on
  the spine.
- **Groups are paired with each other**, and each reviews its partner. Both have read the
  partner's M5 sketch in advance and arrive with written comments; both hand them in, and that
  document is the 10% — **graded on the group, not the individual.** 40 students in ~12 groups
  gives 6 pairings.
- Why paired groups rather than open feedback: reviewing cold in the room produces vague comments
  and rewards whoever is most comfortable speaking. Reviewing a document your group was assigned
  produces specific comments and produces them from everyone. It is also the only version of this
  that is gradeable at 40 students.
- Present a group's feedback **immediately after its partner presents**, so the pairing is visible
  and reciprocal.
- **Close with 20 minutes on what the room got wrong collectively.** After 15 presentations there
  will be two or three recurring errors, and naming them is worth more than any individual note.

---

# PART 3 — BUILD ORDER

The full reframing is 2–3 days of concentrated work, but it doesn't have to happen at once. Only
session 1 must be ready on Sep 15, and the sessions that change most (2, 3, 5) are 13, 20 and 34
days out. Each week I build the next deck, adjusted for what actually happened in the previous
class.

| When | What I build | Notes |
| :--- | :--- | :--- |
| **This week (by Sep 15)** | Syllabus update + **session 1 deck** | Syllabus first — weights, dates, memo ladder and new reading list all get announced on day one |
| By Sep 22 | **Session 2 deck** — the big rebuild | Split `2_basics_quantitative_data/` into two folders; build the provenance block; source Google Flu + Xbox figures |
| By Sep 29 | **Session 3 deck** | Mostly assembling existing frames that finally have room, + Obermeyer, Chetty, culturomics |
| By Oct 6 | **Session 4 deck** | Mostly deletion — cheapest week |
| By Oct 13 | **Session 5 deck** | The cut + the credibility and ethics blocks |
| By Oct 23 | Workshop materials | Peer-review sheet, slot schedule, required-slides list |

Each deck comes with a slides PDF and a slides+notes PDF (two-wrapper pattern), and gets compiled
and checked before I hand it over.

## What I need from you before I start

1. **Confirm the weights** (§1.1): memos 20 / presentation 35 / peer feedback 10 / essay 35.
   Everything else in the syllabus depends on this.
2. **Are you happy moving Villamil & Balcells** out of the paper slot into a session-5 worked
   example you present yourself? (§1.3)
3. **The swappable session-2 example** — a 2026 topic to replace Charlie Kirk, or keep it.
4. **Anything from how last year actually went** that isn't in the files — which blocks ran long,
   what fell flat, what the cohort struggled with.

## What I'll ask you each week

After each session, before building the next deck: what ran long, what landed, what the memos
revealed they hadn't understood. The decks then get adjusted rather than built blind — that's the
point of doing this week by week rather than all at once.
