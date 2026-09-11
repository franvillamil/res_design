# Session 1 — Why design at all?
## Extended teaching notes

Companion to `introduction.pdf` (68 slides) and `introduction_notes.pdf` (slides + short
presenter notes). This document is the long version: what to say, what to ask, what answers to
expect, and what to cut when you run out of time.

**Slide numbers match the footer in `introduction.pdf`.** Numbers that don't appear as headings
below are full-bleed images; they are covered inside the block they belong to. Slides labelled
*Roadmap* are auto-generated section dividers — say the section name and move on, they cost
five seconds each.

---

## 1. The shape of the evening

Tuesday 18:00–21:00. No paper discussion this week, so the whole slot is lecture, one exercise,
and administration.

| | Block | Slides | Planned |
| :--- | :--- | :--- | ---: |
| 18:00 | Arrival, settling, waiting for stragglers | — | 10 min |
| 18:10 | **A.** Opening + Challenger + the triad | 1–19 | 45 min |
| 18:55 | Break | — | 10 min |
| 19:05 | **B.** Evidence and claims; variation; units | 20–42 | 50 min |
| 19:55 | **C.** From data to a question; the two theses | 43–54 | 25 min |
| 20:20 | **D.** Audit exercise | 55–57 | 20 min |
| 20:35 | **E.** Logistics, reading, M1 | 58–67 | 12 min |
| 20:47 | Close | 68–69 | 3 min |
| 20:50 | Questions, and buffer | — | 10 min |

**Be honest with yourself about this.** The deck contains more material than 155 minutes holds
— probably 185 minutes if you run every discussion properly. That is deliberate: the discussion
slides are elastic and you cannot know in advance which ones this cohort will bite on. But it
means you *will* cut, and if you do not decide in advance what to cut, the thing that gets cut
is whatever is last — which here is the logistics block, where this year's unfamiliar assessment
structure gets explained.

**Cut in this order, and decide before you start:**

1. **Marshmallow pair (26–27)** — 5 min. The replication point is made again in week 5 with
   Breznau.
2. **Attribution science trio (29–31)** — 8 min. Halley alone carries "variation need not come
   from many units." Keep 28 as the setup question and go straight to Halley.
3. **`psycho1` / `Opinions about claims?` (24–25)** — 5 min. Only if desperate: this is where
   the room warms up.
4. **Shorten the exercise to 10 + 10** (55–57) — 5 min.
5. **`Simplifying` (54)** — 2 min. It restates the diagram on 51.

That is 25 minutes of slack without touching anything load-bearing.

**One hard rule.** Be at slide 59 (logistics) by 20:35 whatever has happened. The memos are set
rather than written in class now, so the end of the evening has some give — but the logistics
block cannot be rushed, because it is where the whole assessment structure gets explained and
this year's structure is unfamiliar.

### Before you walk in

- The `collider_animation.gif` is *not* in this deck — that is week 4. Nothing here needs
  anything beyond the PDF.
- Make sure the **syllabus matches slide 61** (memos 20 / presentation 35 / feedback 10 /
  essay 35). The structure is new and they will check.
- Set up the **five memo assignments and the essay submission in Aula Global** before session 1,
  so you can point at them when you explain the assessment.
- Confirm you have **permission to use the two TFM examples**, and that they are anonymised
  enough. You are telling the room they are real; see slide 45.

---

## 2. Block A — Opening, Challenger, and the triad (slides 1–19)

### 2 — Introduction

**Purpose:** set the norm that this room talks, in the first ninety seconds.

Ask the two questions and wait. Do not fill the silence — a cohort learns in week 1 whether
silence is dangerous or normal, and whichever they learn holds for five weeks.

**What they will say:** "methods", "statistics", "how to do research", "how to write the thesis".

**What to do with that:** take it seriously, then redirect. The thesis answer is the useful one —
say that this course is precisely the part of the thesis that goes wrong, and that it is not the
statistics.

**Do not** yet say "there is no statistics in this course." That lands on the next slide with
its justification attached; said here it sounds like an apology.

### 3 — What this course is, and is not

**Purpose:** answer the reasonable objection before it becomes five weeks of quiet doubt.

These students are in a *computational* social science master's with a full methods sequence of
their own — statistics, survey methods, data science, R, causal inference, data mining. Some of
those run this same semester. They have already done a 3 ECTS statistics course. So a course
with no equations needs an explanation, and "you're not ready" is both false and insulting.

**The framing that works — division of labour, not simplification:**

> Your programme already teaches estimation, and teaches it properly, in dedicated courses —
> some of which you are sitting in right now. What it does not teach anywhere else is the part
> that decides whether running the estimator was worth doing at all. That is this course.

**Say explicitly that you will still use the vocabulary.** You will say "regression", "control
variable", "coefficient" — they know these words and there is no reason to tiptoe. The rule is
that this course never asks *how* a quantity is estimated, only *what comparison* is being made
and *what has to be true* for it to mean anything.

**Anticipated question — "so will this help with my thesis?"** Yes, and more than the methods
courses will, because the thesis fails on the question and the data far more often than on the
model. You are allowed to be blunt about this.

### 5–9 — The Challenger sequence

**This is the best twenty minutes in the course. Do not rush it, and do not reveal early.**

**Slide 5 (A real example).** Read it as a live problem, not as a story. They are the racing
team. Seven engine failures in twenty-four races, a cold forecast, a mechanic with a hunch, and
someone asking to see the data on the failures.

Ask: *you have the seven failures in front of you. What do you look at?*

Almost everyone reasons **within the seven failures** — was it cold at each one, is there a
pattern among them. That is the whole point, and it is worth letting two or three people say it
out loud before you move on. Nobody asks for the successes.

**Slide 6 (`challenger1`).** The seven failures plotted against temperature. No pattern.

Say clearly what this is: a reconstruction (Tufte's) of the evidence as it was weighed, not a
photograph of the actual Thiokol chart. Students who go and read about Challenger afterwards
will find the difference, and you want to have said it first.

Ask: *on this evidence, do you launch?* Most will say yes, or say the data is uninformative.
Both are the right reading of this chart. That is the trap closing.

**Slide 7 (`challenger2`).** All twenty-four launches. The pattern is unmistakable.

Let the reaction happen. Then say the sentence the whole course hangs on:

> **The answer was in the observations they didn't look at.**

Nothing was wrong with the data, the statistics, or the people. The selection of which cases
entered the comparison was the entire ballgame.

**Slide 8 (`challenger_wiki`).** Reveal: Challenger, 28 January 1986, seven dead.

Say that this is not a story about people being stupid. Every step looked reasonable from the
inside, and that is exactly why it is frightening. The engineers who were worried could not make
the case with the chart they had.

**Slide 9 (Challenger example).** Name the two things design did, because both recur every week:

- **which observations** enter the comparison;
- **what variation** you need in order to see anything at all.

With only failures, temperature does not vary usefully. You need the non-failures to have a
comparison at all.

This is also the first appearance of **selection on the dependent variable** — selecting cases
on the outcome you are trying to explain. Name it. It is the most common single error in the
theses they will write, and giving it a name now means you can point at it in the workshop with
one word.

### 10 — `plane_wwii` (survivorship bias)

**The same lesson in one picture, and the version they will remember.**

Set it up before showing what it means: WWII bombers returning from missions, bullet holes
mapped. Ask *where would you add armour?*

Most say: where the dots are. Let them.

Then Wald's answer: **armour where there are no dots**. Planes hit in those places did not come
back, so they are not in the data. Identical structure to Challenger — the informative
observations are the missing ones.

One caveat worth saying: the red-dot diagram is a modern illustration, not a wartime document.
Do not let anyone cite it as a primary source.

**Then plant the question that runs through the whole course:** *who is missing from this
dataset?* You will ask it again in week 2 about every found data source, and it is one of the
three or four questions that make the difference between a good and a bad thesis.

### 11 — Not only about data or empirical evidence

**The thesis statement of the course.** It has been moved early on purpose.

The things that break a master's thesis are almost never the estimator. They are:

- a question that cannot be answered;
- data of unknown provenance;
- a measure that does not capture the concept.

Estimation is covered properly elsewhere in the programme. This is the part that decides whether
any of it was worth doing.

The second bullet on the slide is worth dwelling on: everyone believes they already know how to
ask a research question, which is exactly why so few can.

### 12 — Types of research

Two minutes. Locate the course: empirical, mostly quantitative.

Worth adding that almost everything here applies to careful qualitative work too — the logic of
comparison does not change with sample size. It costs one sentence and it stops the qualitative
students from switching off.

The "descriptive vs explanatory" line at the bottom is deliberately a tease. **Do not expand it
here.** It hands straight into the next section.

### 13 — Roadmap → **Three ways to answer a question**

### 14 — Three things you can do with data

**This is the spine of the course.** Everything after it hangs here, and it comes back at the
end of every session and in the workshop.

- **Describe** — what is the case? How much, where, for whom?
- **Explain** — does X cause Y, and through what?
- **Predict** — given what I observe, what happens next, or elsewhere?

**Three things to say explicitly, because students get all three wrong:**

1. These are different **goals**, not difficulty levels. Description is not failed explanation.
   A great many careers are built entirely on description.
2. Each has **its own success criteria**, which is the reason the rest of the course exists.
3. They get confused constantly, and that confusion is where most bad designs come from.

Add that this is the real fault line in computational social science, and they will meet it in
every other course in the programme. Prediction-shaped work and explanation-shaped work look
similar from outside and are evaluated completely differently.

### 15 — Describe (`facebook_separation`)

"Six degrees of separation", measured. **1.59 billion Facebook users; the average is 3.57.**

*(Careful: the earlier Backstrom et al. 2012 study used 721 million users and got 3.74. Two
different studies — do not merge them.)*

Purely descriptive: no causal claim, no prediction, no model. And still full of design
decisions worth naming:

- Who counts as a **user**? Active how recently?
- What counts as a **friendship**? Reciprocal? Ever, or currently?
- Which **population** is the average over — and is it a claim about humanity, or about
  Facebook users in 2016?

That third question is the one to leave hanging. It is the seed of week 2.

Mention Chetty et al. (2022, *Nature*) as the bigger version — 21 billion friendships turned
into a descriptive atlas of cross-class connectedness in the US, with real policy consequences.
One sentence; it gets a proper slot in week 3.

### 16 — Explain (`guess2023`)

Guess et al. (2023), *Science*. Facebook and Instagram feeds randomly switched from algorithmic
ranking to reverse-chronological for three months during the 2020 US campaign.

**Plant it now — this is their week-4 paper**, and saying so makes the reading feel assigned for
a reason rather than at random.

The design question to pose and then *not* answer: what does randomisation buy you, and what
does it still not buy you?

Hold back the answer until week 4, but here it is so you can steer: randomisation buys you the
comparison — the treated and untreated groups are alike in every other respect, so the
difference is attributable to the feed. It does not buy you **generality**: one platform, one
country, one campaign, three months. A perfectly identified estimate of a very specific thing.

If someone asks about the near-null result, say that is exactly the week-4 discussion and resist.

### 17 — Predict (`blumenstock`)

Blumenstock, Cadamuro & On (2015), *Science*. Mobile phone metadata — who called whom, when, for
how long — used to estimate wealth across Rwanda, where reliable and frequent wealth data barely
exists. (Rwanda does run censuses; the gap is timely, granular wealth data.)

**The key point, and the reason prediction is in this course at all:** this design identifies
nothing causal, and does not need to. It has to work **out of sample**. That is a completely
different standard of success.

Say the practical consequence out loud, because it will matter in the workshop:

> If a student's project is really a prediction problem, holding it to causal standards will
> make it worse, not better.

Every year somebody has a genuinely good predictive project and spends the whole essay
apologising for not having identification. This slide is the pre-emption.

### 18 — Different goals, different standards

The payoff table. This is why naming the goal is not just terminology.

| Goal | Succeeds when | Typical failure |
| :--- | :--- | :--- |
| Describe | coverage, comparability, a defensible denominator | measuring what got recorded, not what happened |
| Explain | the comparison isolates the effect | something else moved at the same time |
| Predict | it works out of sample | it worked, until the world changed |

Each row previews a session: describe → week 3, explain → week 4, predict → the close of week 5.

**The second bullet is the practical one.** Most bad designs are a **mismatch** — a descriptive
question with a causal answer bolted on, or a causal question answered with a predictive design.
In the workshop you will diagnose half the room with exactly that sentence, so it is worth them
hearing it in week 1.

### 19 — The five questions of this course

The course map. It returns at the start of every session with the current question highlighted.
One slide a week, and it is what makes five lectures feel like one argument instead of a list.

Read the last line aloud: **by the workshop you should be able to answer all five about your own
project.** Then say the thing that makes the whole structure click —

> The five questions are also the five memos. The memos assemble into the essay. Nothing in this
> course is busywork bolted onto the assessment.

---

## 3. Block B — Evidence, variation, units (slides 20–42)

### 21 — Empirical research

Merged from two near-identical slides in the old deck.

**Define "inference" on the slide**, because it is highlighted, it names the section, and for a
mixed cohort it collides with three other meanings (statistical inference, causal inference, and
ordinary English): *using what you did observe to say something about what you did not.*

**The second half matters as much as the first.** Half of what this course is for is making them
better *readers*. Most of them will evaluate far more research than they will ever produce —
in a job, on a policy team, in a newsroom. Say that; it changes how the paper discussions land.

### 22–23 — Empirical evidence and claims (`nyt_museums`)

Let them read the headline. Then: *what would you have had to do to be able to write this?*

Reverse-engineering a claim is the skill the audit exercise reuses at the end of the session, so
flag it as a skill and not just a discussion.

Work toward **observation vs manipulation** — did someone watch what happened, or did they
intervene and see what changed?

Then put a deliberate question mark on **"gold standard"**. Experiments are not automatically
better; they are better at one specific thing (isolating an effect) and often worse at others
(realism, scale, cost, ethics, generality). Week 4 does this properly. Resist doing it now.

### 24–25 — `psycho1` / Opinions about claims?

A widely reported psychology finding at face value. Let them react before you say anything —
the reaction is the material.

**The move to teach here** is separating two objections that sound alike:

- *"I don't believe the finding"* — an opinion, and not interesting.
- *"That design could not have shown that"* — a research design objection, and the only kind
  worth learning to make.

Say this explicitly. It is the standard you will hold them to in every paper discussion and in
the workshop, and it is the difference between a commentary that is worth 5% and one that isn't.

### 26–27 — `marshmallow` / `marshmallow3`

**26.** The marshmallow test: children who wait for a second marshmallow do better later in
life. Famous, endlessly repeated in popular writing.

**27.** The replication (Watts, Duncan & Quan 2018), larger and more diverse sample: the
association is **largely attenuated** once family background is accounted for. Attenuated, not
eliminated — be accurate, someone will check.

**The design lesson:** the original comparison was between children who differed in a great many
ways besides willpower. Same data pattern, entirely different explanation — and *you cannot tell
which from the correlation alone*. That gap is what weeks 4 and 5 are about.

**First candidate for cutting** if you are behind. The point recurs with Breznau in week 5.

### 28–31 — Other types of inference; attribution science

**28.** Ask it as a real question: what if you don't have enough observations? Usual answers —
rare events, single countries, historical cases, one-off disasters. The point is not that these
are impossible, but that the variation has to come from somewhere other than "many units."

**29–31 (`derecho2022a/c/b`).** Attribution science: how can anyone say a *specific* heatwave was
made more likely by climate change, when there is only one of it?

Let them speculate before revealing. The answer — simulate the world with and without — is a
preview of potential outcomes in week 4, and it is worth flagging as such.

Then the design point: **the variation is generated by a model, not observed in the world.**
That is a legitimate design, and it makes its assumptions completely explicit — more than can be
said for a lot of observational work.

**Second candidate for cutting.** Halley makes the same point in one slide.

### 32–34 — A few questions on variation (with `halley_eclipse`)

**The Socratic centre of the session. Go slowly and let silences sit.**

Take only the first question (*why do we talk so much about variation?*) before the eclipse.

**33 (`halley_eclipse`).** Halley's eclipse map. One prediction, no sample, no statistics — and a
genuinely strong test, because the theory said exactly where the shadow would fall, and it fell
there.

Use it against the reflex that more observations means better evidence. **What makes evidence
strong is how much it could have gone the other way.** A prediction that would have been
spectacularly wrong, and wasn't, is worth more than a thousand observations consistent with
everything.

**34 (the slide returns).** Now work the rest:

- **"Few observations" as a critique** — not fatal in itself. The question is whether the
  comparison is informative, not how many rows there are.
- **Why statistics at all** — to separate signal from noise, nothing more. Statistics answers
  *"could this be chance?"* It never answers *"does this design identify anything?"* Since they
  have had a statistics course, you can put this sharply: a perfectly executed regression on a
  badly designed comparison is a precise answer to the wrong question.
- **Experiments** — they manufacture the variation instead of waiting for it.
- **Attribution science** — the variation is simulated.

### 35 — Description is not easy either

Both questions sound like lookups. Neither is.

Push on the first — *are levels of political violence increasing?*

- Increasing compared to **when**?
- Counting **what** — deaths, events, participants, incidents?
- **Per capita or absolute**?
- Recorded **by whom**, and has the recording itself changed over time?

That last one is the killer, and it is the seed of both week 2 (drift) and week 3 (measurement).
**Do not resolve any of it.** This is the trailer.

### 36 — Roadmap → **Units, observations, variation**

### 37 — Key ingredients

The only formal slide in the session, and every later week refers back to it. Walk the picture
slowly.

- **Observation** — one row.
- **Unit of analysis** — what a row *is*. Here **country-year**, not country: Spain appears
  twice.
- **Variable** — one column.
- **Variation** — the spread down a column.

**The last line is the one that matters**, and it closes the loop on the opening: *no variation,
nothing to learn.* With only failures, the outcome column does not vary, so no comparison is
available. Say that connection out loud — it turns a vocabulary slide into the Challenger lesson
restated formally.

### 38–42 — Unit of analysis

**38.** Being able to name the unit of analysis in five seconds, from a graph or an abstract, is
a genuinely useful reflex. The next three slides are practice. Make them commit to an answer
each time before you reveal.

**39 (`ua1`).** *Answer:* the country, in a single year (2006). Press on whether that makes it
country or country-year — with one year there is no time variation, so it is a cross-section.

**40 (`ua2`) — this one is in Spanish, so read it out.** *Median income per living unit, by
family type and autonomous community* (INE / El País). Green = all households, orange = large
families, red = one adult with 1–2 dependent children.

*Answer:* the unit is the surveyed **household**, displayed aggregated to region × family type.

**And read the footnote aloud, because it is the best thing on the slide:** *"the data for
Extremadura, Aragón, Canarias, Cantabria, La Rioja and Asturias are based on only 50 to 100
interviews."* Six of eighteen regions are estimated off almost nothing — and the chart gives
every dot the same visual weight. That is a free lesson about how a visualisation can hide
uncertainty, and it costs you thirty seconds.

**41 (`ua3`).** *Answer:* the individual award-winner, displayed as a smoothed 12-year moving
average by category and year.

Across all three, note how often **the graph shows one unit while the data underneath is at
another**, aggregated up for display. That gap has a name — the **ecological fallacy**: what
holds for regions need not hold for the people in them. Say it once here; it returns whenever
they aggregate.

**42.** Most designs can be adapted to different units, and the unit you choose is tied to the
question and the theory. Child, classroom, school, neighbourhood, cohort — each is a *different
question*, not a different way of asking the same one.

This deliberately sets up the hotel thesis four slides later, where a badly chosen unit quietly
changes what is being asked.

---

## 4. Block C — From data to a question (slides 43–54)

### 44 — Where does your data come from?

One frame now; a whole session next week. But plant it here, because both theses coming up are
found data, and so is almost every project they will propose.

- **Designed** — surveys, experiments, a coding scheme you wrote. Answers a question **you** had.
- **Found** — digital traces, admin records, scraped text, API dumps. Answers a question
  **someone else** had. Or no question at all.

**The sentence to land:**

> Found data was produced by someone, for their own purposes, and those purposes are still in
> the data. Facebook did not build a friendship graph so that you could measure social capital.

### 45 — Two theses from this programme

**Say they are real TFMs from this master's — and say they are anonymised and used with
permission.** Both halves matter, and say them unprompted. These students are about to hand in
five memos and a design of their own, and how you treat other people's student work is how they
will assume theirs gets treated.

Saying they are real changes how they listen. These are not published papers by professors;
they are the thing they themselves have to produce in nine months.

**The honest framing:** both started with **data already in hand and no question yet**. That is
not a sin. It is how most of them will start, and much good research starts that way. It is
simply the harder direction to work in, and it needs the discipline this course teaches.

### 46 — Thesis 1: hotel bookings

Data from a hotel: every booking, and whether the guest actually showed up. Unit of analysis:
the booking.

- **Motivation:** *"how do we increase business for the hotel?"*
- **Research question:** \_\_\_\_\_\_\_\_ ?

**Leave the blank on screen and let it sit.**

"How do we increase business" is a perfectly good **motivation** — it says why anyone should
care, and it is the reason the data exists at all. It is not a research question. Nothing in it
tells you what to compare with what.

That gap is the single most common problem in a first thesis draft, and it gets its name a few
slides later ("motivation is not a question", slide 52). Here they feel it before it has a
label, which is the right order.

**Do not mock the original.** Almost every first draft starts exactly here, including good ones.

### 47 — Thesis 1: what can these data answer?

Work it live. The framing: the motivation is fixed, the data is fixed — now what question is
actually *available*?

**Which goal could this be?**

- **Describe** — what share of bookings are no-shows, and how does that vary by season, booking
  channel, price, lead time?
- **Explain** — does requiring a deposit reduce no-shows? Now you need a comparison, and the
  hotel introduced the policy for a reason, which is precisely the problem.
- **Predict** — flag likely no-shows in advance so the room can be resold. Perfectly good, needs
  no causal claim at all, and is probably what the hotel actually wants.

**Unit:** booking, guest, room-night, season. A repeat guest with ten bookings is ten
observations — is that what you want? If the question is about *guests*, the booking is the
wrong row.

**Missing:** people who wanted to book and couldn't, people who looked and booked elsewhere,
people who never considered this hotel. Callback to the bombers — the data contains only those
who made it into the system.

**The last bullet is the payoff.** The hotel wanted more business. What the data supports is
mostly a no-show question. Those are not the same thing, and noticing the gap — rather than
quietly answering the easier question and presenting it as the harder one — is the skill this
whole course is training.

### 48–49 — Thesis 2: music collaborations

**48.** Spotify API data reshaped into a network: artists as nodes, a shared track as an edge.
Variables available: time, genre.

Note what happened, because it is subtle and common: **the structure of the analysis (a network)
was decided before the question was.** The tool came first.

*(The image is a Spotify marketing banner — decorative. If you find a real collaboration-network
figure, swap it in.)*

**49.** Push on these:

- Is the unit the **artist**, the **collaboration**, or the **genre**?
- Are genres becoming more connected over time — or is Spotify's catalogue just growing?
- Who decides what counts as a **genre** here?
- If the platform changes how it tags artists, what happens to your trend?

**Questions two and four are the same question in two forms, and it is the important one:**

> Is this a fact about music, or a fact about Spotify?

Genre labels are Spotify's, produced for recommendation, not for research. If they retag, the
measured trend moves without anything happening in the world.

**That is exactly the Google Flu problem — next week's paper.** Say so. It makes the reading feel
like it was assigned for a reason.

### 50 — What both have in common

The takeaway both examples were building toward:

- Both are **found** data.
- Both entered the process **in the middle** — at the data, not at the question.
- Neither dataset came with a **goal**, a **unit**, or a **question**.

> That is what the rest of this course supplies.

**Do not let them conclude that starting from data is illegitimate.** Most of them will do it.
The point is that the goal, the unit and the question then have to be supplied deliberately, by
them, rather than inherited from whoever built the dataset.

### 51–54 — The research process

**51.** The textbook diagram. Show it, then immediately qualify it: **the red arrow is the honest
part — nobody does this once, in order.** Both theses just discussed entered this diagram in the
middle, at "Measure", and had to work backwards. The diagram does not show that.

Also worth saying here: this is, more or less, the structure of the final essay.

**52.** Motivation is not a question. Good questions are answerable and relevant.

Run the four examples quickly as a group:

- *"What is the best Netflix show?"* — not answerable empirically; it is a question about taste.
- *"What can we do to help poor countries develop?"* — a motivation, not a question, and far too
  big. Compare directly with the hotel.
- *"What are the shopping patterns of the Spanish population?"* — answerable, descriptive, but
  vague: which patterns, measured how, over what period?
- *"Do individuals from minorities support the use of violence?"* — answerable, but needs a lot
  of conceptual work before it means anything. Which minorities? What counts as support? What
  counts as violence? That is week 3.

**53.** The rest of the pipeline, with week numbers on the right so they can see the course has a
shape.

**Spend a minute on the statistics question**, and now you can be sharp about it because they
have had a statistics course: statistics tells you whether a pattern could be chance. It cannot
tell you whether the comparison you made was the right one. Those are different problems, and
only one of them is this course.

**54.** The version to remember, and roughly the structure of the essay. Two minutes, or cut it.

---

## 5. Block D — The audit exercise (slides 55–57)

### 56 — Exercise: audit a claim

**Three claims:**

- **A.** "Remote work has made young people lonelier."
- **B.** "Four in ten Spanish adults under 30 still live with their parents."
- **C.** "Our model flags students at risk of dropping out with 87% accuracy."

**Say "invented headlines" out loud.** Otherwise the Spanish students will spend ten minutes
arguing about whether the real figure for B is 40% or 65% — which is exactly the reflex the
exercise exists to replace.

**Assign one claim per pair as you go round.** Three claims each in ten minutes is a minute per
cell and produces nothing usable. One claim, three written sentences.

The three questions stay on screen next to the claims — that is why they are on the same slide.
Requiring written sentences is what makes the debrief work: without them, the discussion is
whoever speaks first.

**Circulate and listen rather than helping.** What you overhear is the best read you will get on
this cohort before the first memos come in — and it tells you what to adjust in week 2.

**Answer key:**

**A — causal, dressed up as obvious.** Lonelier than *whom* — than they were before, or than
people still going to offices? Who *chose* remote work, and were they different already? What is
the baseline — measured against a pandemic in which everyone was lonely? Is "lonelier" measured,
or asserted?

**B — descriptive, and the hardest of the three.** Who counts as an adult under 30 — registered,
resident, students away in term time, people in shared flats who are technically registered at
their parents' address? What does "live with" mean? Which register or survey, and does it catch
people who moved back? What is the denominator? Descriptive claims *feel* like lookups and
almost never are — this is the point of week 3.

**C — predictive.** Accurate at *what*? If 8% of students drop out, a model that says "nobody
drops out" is 92% accurate. Accurate on whom — the students it was trained on, or next year's?
Does it still work when the intake changes? And the question they usually don't ask: *what
happens to a student the model flags?* A prediction that triggers an intervention changes the
thing it predicts.

### 57 — What we just did

Close the loop explicitly. They have just done, quickly and badly, the thing the course trains:

- named the **goal** before arguing about the answer;
- asked what data would have to exist;
- found the weak point in the **design**, not the finding.

**Stress the third.** "I don't believe it" is not a critique. "That design could not have shown
that" is. That standard is what you want in the memos and in the workshop.

---

## 6. Block E — Logistics and close (slides 58–69)

Twelve minutes, brisk. They will read the syllabus anyway; what they need from you is the
*reasoning*, which is not in the syllabus.

### 59–60 — Logistics, calendar

Everything lives on the website. Say that office hours are genuinely by appointment and that
using them is normal, not an admission of failure — in a cohort this size, the students who come
are the ones whose essays improve most.

The calendar is the five questions with dates attached. Point that out; it costs one sentence.

### 61 — Evaluation

**Give the reasoning, not just the numbers.** The syllabus has the numbers; what they need from
you is why it is shaped this way, and this cohort will respect being told directly.

| | % | graded as |
| :--- | ---: | :--- |
| Design memos | 15 | pair (M5: group) |
| **Workshop presentation** | **35** | group |
| Workshop feedback on your paired group | 10 | group |
| Final essay | 40 | **group only** |

- **No participation grade.** It cannot be graded fairly, least of all with forty people in an
  evening slot.
- **No graded paper commentary.** Be blunt about why: a short written commentary on a paper is
  close to the perfect task for a language model, and marking it would tell you nothing about who
  actually read it. Reading is still expected. The memos run off the papers instead.
- **The presentation is the largest single component.** It is the one thing that cannot be
  outsourced — a group in front of the room, taking questions about a design they have to have
  understood.

**Expect: "why is so much of this group work?"** Answer honestly. Because it is what can be
assessed properly at this class size, and because feedback that comes out of a document beats
feedback that comes out of whoever is most comfortable talking. Do not pretend it is
pedagogically ideal in every respect; they can see the class size as well as you can.

**Also expect: "what if my group doesn't pull their weight?"** Have an answer ready before you
walk in — 45% of the grade rides on group work, and this is a fair question, not a whine.

### 62 — The design memos

**Sell this properly or it reads as five extra assignments.**

Five short exercises, half a page, set in class and submitted **on Aula Global by Monday night**
— formally Tuesday 00:00. Due Mon Sep 21, Sep 28, Oct 5, Oct 12, and **M5 on Mon Oct 19**. Each
has its own assignment in Aula Global; set them all up before session 1.

**M1–M4 are written in pairs. M5 is written by the workshop group**, and it is due a week earlier
than the pattern would give — the group you are paired with needs time to read it before the
workshop. Flag both exceptions when you set M4, not only when you set M5.

**Say that this is the only thing they hand in all term** before the essay. That reframes it from
"five extra things" to "the assessment, spread out."

**Why they are not about their own project.** You do not have to pick a topic tonight and defend
it for five weeks. It is far easier to see that a design is broken when it is not yours — there
is nothing at stake in admitting it. M5 is the deliberate exception: their own project, and what
their paired group reviews before the workshop.

**The prompts are deliberately open about material.** Any claim, any dataset they can get to.
Tell them to pick something they actually care about, or find annoying — the skill is meant to be
transferable, and the memos are more interesting to read when the material varies.

Pairs are the students' own choice and can change week to week. Say that; it removes a whole
category of anxiety.

*Grading cost: 20 pairs × half a page ≈ 25–35 minutes a week, marked done / done-well.*

### 63 — Papers

Three papers, all short, none requiring statistics to discuss. Google Flu (Sep 22),
Müller-Crepon (Sep 29), Guess et al. (Oct 6).

**There is no enforcement, and you should say so once, plainly, without threatening.** Nothing to
hand in, no mark, no memo attached. The honest reason is worth giving: a written commentary on a
paper is close to the perfect task for a language model, and marking it would tell you nothing
about who actually read it. So the course runs on the assumption that they are adults on a
master's programme.

Then state the consequence rather than the threat: if you have not read it, a chunk of the session
is a conversation you cannot join.

**Say what the discussion will actually look like**, because it is not a seminar of twelve and
they will assume it is: you present the design in a few slides, then they work in **pairs** on
prompts that are on screen, then in small groups on "how would you redesign it." Everyone works;
nobody has to perform in front of forty people.

That last point is worth making out loud in week 1. A large chunk of the room will otherwise
spend five weeks assuming the paper discussion is a thing that happens to other people.

*Realistic expectation: not everyone will read them. Short papers are doing most of the work —
Google Flu is three pages, which makes reading it visibly cheaper than not.*

### 64 & 65 — The workshop, and the final essay

*(Two slides now — they were doing too much as one.)*

**The workshop is 35% of the grade, and the essay group is the same group** — so 75% of a
student's mark comes out of this single project. Say so here, while groups are forming, because it
changes how seriously the whole thing gets taken.

**"The threat I can't solve" is required, and it is graded positively.** A design with no
acknowledged limitation is either trivial or not understood. This removes the incentive to
oversell and produces much better discussion. Say it twice; they will not believe you the first
time.

**Paired groups.** Each group reviews one other group and is reviewed by them, working from the
partner's M5 sketch, which they get a week ahead (M5 is due Mon Oct 19). Written comments, handed in, graded on
the group. That is why the feedback is worth 10%: reviewing an assigned document produces
specific comments from everyone, whereas open feedback in the room produces vague comments from
the three people most comfortable speaking. ~12 groups gives 6 pairings.

**The essay is group-only this year**, written by the same group that presents. Expect the
question, and the answer is worth giving: the presentation, the feedback and the essay are one
piece of work seen three times, and splitting the essay off from the group that presented it would
not make sense. Same group, same grade.

**The essay is a pre-analysis plan** — the document a researcher actually writes before collecting
anything. Naming the genre matters: it stops being "an essay about research design" and becomes a
real document with a known shape. Pre-registration is covered properly in week 5. No data analysis
required; showing something is fine if it helps, but it earns nothing on its own.

**The deadline — Oct 27, the day after the workshop — needs explaining or it will be
misread.** Say plainly what it means:

> The essay should be **finished before you present it**. The presentation *is* the essay, out
> loud. The extra day exists so you can act on what your paired group and the room tell you — not
> so you can start writing on the 27th.

A group presenting a design they have not yet written up will be obvious to everyone in the room,
including them. That is the point of putting the workshop first.

Everything is submitted through Aula Global.

### 66 — Books

The annotations on the right are the point. Students at the start of a master's read a booklist
as a set of instructions, and Imai and Cunningham belong to the methods courses rather than to
this one — worth saying so, since several of them are taking those courses right now.

Salganik is first because it is the only book on the list actually about computational research
design rather than about estimation, and it is free.

### 67 — Before next week (reading + M1)

**There is no reading assigned for tonight** — students can't reliably be reached before the
course starts, so nothing was set in advance. This slide is where the reading pattern begins.

**The reading:** Lazer et al., *The Parable of Google Flu*. Three pages, no equations, and the
best cautionary tale in the field. Nothing to hand in and no mark.

Selling it beats requiring it: say it is three pages and genuinely enjoyable, and that next week's
session is half a conversation about it. That is more persuasive to adults than telling them it is
compulsory — which you cannot enforce anyway.

**M1**, in pairs, on Aula Global by **Monday night** (formally Tue 00:00): take a claim — from the
news, from anywhere — name the goal, say what data it would need, name the first thing you'd
doubt. Half a page.

It is tonight's audit exercise again, done properly and in their own time, so it costs no class
time and they have already rehearsed it. Tell them to pick a claim they find *annoying* rather
than one they think will impress, and that it has nothing to do with their final essay.

### 68 — Where we started

**Two minutes, after the administration, so the session ends on its argument rather than on a
deadline.**

Return to the Challenger chart if you still have it up. Everything today was one claim:

> What you can learn is set by which observations enter the comparison — and that is a choice
> somebody makes, usually before any data is analysed. Often, with found data, somebody who is
> not you.

Then question 1 is done, and next week is *where does the data come from?*

### 69 — Questions?

Take questions. M1 and the reading are set on slide 67 and done at home, so there is nothing to
hand out — which means you have a genuine buffer at the end of the evening. Use it for questions,
or to recover whatever ran over earlier.

**Tell them explicitly it is not binding** — they can change topic entirely next week. The point
is to have something concrete to sharpen. A blank page in week 5 is much worse than a bad topic
in week 1.

Collect on the way out.

---

## 7. Appendix slide (not shown live)

**`replicationcrisis2`** — the replication crisis in one headline. Parked because week 5 covers
it properly with Breznau et al. and the many-analysts study. Show it only if a discussion goes
that way on its own; the one line worth having ready is that a published result in a good
journal is evidence, not proof, and knowing how much to update is exactly what design training
gives you.

---

## 8. After the session — what to note down

The week-2 deck gets built from what happens tonight. Five minutes of notes afterwards is worth
more than an hour of guessing later.

- **Where did you actually end up on the clock?** Which blocks overran, and by how much.
- **Which discussions took?** The variation block and the audit exercise are the two most likely
  to run long. If they did, that is good news and week 2 should be built with more room.
- **What did you hear during the exercise?** Specifically: did anyone name the goal without
  prompting? Did anyone spot the base-rate problem in claim C?
- **What is in the M1s?** They arrive Monday night, so you have Tuesday to skim them before
  session 2. What they choose to audit tells you a lot about what landed.
- **Who spoke, and who didn't.** In a cohort this size you can fix that in week 2 if you notice
  it in week 1.
