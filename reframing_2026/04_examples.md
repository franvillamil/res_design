# Examples catalogue

Organised by what each one *teaches*, since the constraint is design lessons per minute, not
coverage. Marked **[have]** where images already exist in `slides/img/`, **[new]** where
assets need sourcing.

## Already in the course — keep

| Example | Session | What it teaches | Assets |
| :--- | :--- | :--- | :--- |
| **Challenger / O-rings** | 1 | Selection on the dependent variable; the answer is in the data you *didn't* look at | **[have]** `challenger1.jpg`, `challenger2.jpg`, `challenger_wiki.jpg` |
| **Survivorship bias (planes)** | 1 | Same lesson, one image | **[have]** `files/survivorship_bias.png` |
| **Hotel bookings vs. visits** | 1 | Measuring the wrong thing because it's the thing you have | — |
| **Attribution science** | 1 | Where is the variation when there's one case? | — |
| **Barberá Twitter ideal points** | 3 | Latent measurement from digital traces, done carefully and validated | **[have]** `barbera_tw1-4.jpg`, `barbera_etal1-2.jpg` |
| **Democratic backsliding: objective vs. V-Dem** | 3 | Same concept, two operationalizations, different answers | **[have]** `democratic_backsliding.jpg`, `economist_backsliding.png` |
| **Wartime civilian deaths** | 3 | End-to-end synthesis: concept → operationalization → measure → description | **[have]** `battledeaths.png`, `declineofwar.png` |
| **Müller-Crepon roads** | 3 | Building a relational concept out of spatial data | **[have]** `mullercrepon.png`, `carl1.png`, `carl9.png`, `carl10.png`, `guatemala_roads.png` |
| **Collider / M-bias sequence** | 4 | The one bias nobody's intuition gets right | **[have]** `collider1-5.pdf`, `collider_animation.gif`, `collider_m1-2.png` |
| **Guess et al. feed experiment** | 4 | Platform experiment: what randomization buys, what it doesn't | — (paper) |
| **Snow's cholera** | 5 | The original natural experiment | **[have]** `did_snow.pdf` |
| **Street names / TJ backlash** | 5 | DiD from the instructor's own work; students see the whole design | **[have]** `did_TJ*`, `map_graves*.pdf` |
| **Natural resources meta-analysis** | 5 | A literature as an object of study; funnel plots | **[have]** existing frames |
| **Blumenstock phone metadata** | 1, 3 | Prediction from found data; also a measurement story | **[have]** `blumenstock.png`, `blumenstock2.png`, `blumenstock3.png` |

## To add — highest value first

### 1. Google Flu Trends — Lazer, Kennedy, King & Vespignani (2014), *Science*
**Session 2. Also the assigned reading.** Three pages, no equations. Teaches drift, big-data
hubris, and the fact that a measure can silently stop meaning what it meant — partly because
the platform changed underneath it. The single best CSS design cautionary tale, and it works
as both lecture example and discussion paper.
**[new]** — the CDC vs. GFT overprediction time series plot.

### 2. Xbox election forecast — Wang, Rothschild, Goel & Gelman (2015), *IJF*
**Session 2.** A panel of Xbox gamers — overwhelmingly young, overwhelmingly male — producing
good 2012 forecasts. Teaches that **representativeness is a property of the design, not the
sample**, which is the correct antidote to "my Twitter data isn't a random sample so it's
useless" *and* to "I have 10 million observations so it doesn't matter." Rare example that
cuts both ways.
**[new]** — the demographic-skew bar chart and the forecast-vs-outcome plot.

### 3. Fragile Families Challenge — Salganik et al. (2020), *PNAS*
**Session 5, closing.** 160 teams, 12,942 predictor variables, thousands of hours, and the
best models still predicted life outcomes poorly. Teaches the limits of prediction with
unusually rich data, and closes the course on the triad from session 1. Ends on calibrated
humility instead of a method list, which is the right note for week 1 of a master's.
**[new]** — the scatter of submitted models' R² against the benchmark.

### 4. Many analysts, one dataset — Breznau et al. (2022), *PNAS*
**Session 5.** 162 researchers, same data, same hypothesis, estimates spanning both signs.
The most persuasive available argument for committing to a design before seeing results, and
it requires understanding no model at all — just the spread of the dots.
**[new]** — the specification-curve / distribution-of-estimates figure.

### 5. Algorithmic proxy failure — Obermeyer et al. (2019), *Science*
**Session 3.** An algorithm allocating extra care used *healthcare spending* as a proxy for
*health need*. Since less was historically spent on Black patients at equal sickness, the
proxy encoded the disparity. Pure construct validity, visible without any statistics, and it
connects measurement to consequences in a way this cohort will remember.
**[new]** — the two-panel cost-vs-illness-by-race figure.

### 6. Bail et al. (2018), *PNAS* — bots exposing users to opposing views
**Session 4.** A real field experiment on a platform whose result runs *against* the
intuitive mechanism: exposure to the other side increased polarization, especially among
Republicans. Excellent paired with Guess et al. for a discussion of mechanism vs. effect, and
of why a well-identified estimate can still surprise you.
**[new]** — the treatment-effect-by-party figure.

### 7. Chetty et al. (2022), *Nature* — social capital
**Session 3.** 21 billion Facebook friendships turned into a descriptive atlas of
cross-class connectedness. Proves description is a first-class goal with real design
decisions: what's the unit, what's the denominator, what population. Also a data-access and
privacy story worth one sentence.
**[new]** — one Opportunity Insights map.

### 8. Culturomics and its critique — Michel et al. (2011) / Pechenick et al. (2015)
**Session 3.** A descriptive claim about culture that turns out to be partly a claim about
what got digitised — the corpus is heavily shaped by scientific publishing over time. Ties
back to session 2's drift, and teaches corpus composition as a measurement problem.
**[new]** — an n-gram trend plus the corpus-composition-over-time figure.

### 9. MusicLab — Salganik, Dodds & Watts (2006), *Science*
**Session 4, optional.** Parallel artificial music markets: the same songs become hits in
some worlds and not others. Teaches cumulative advantage, and — more usefully here — that
**a designed digital experiment can create the counterfactual worlds observation can't give
you**. Good if the experiments block has room.
**[new]** — the download-inequality-across-worlds figure.

### 10. Emotional Contagion — Kramer, Guillory & Hancock (2014), *PNAS*
**Session 5, ethics.** Feed manipulation at scale with no meaningful consent. Pair with the
Tastes/Ties re-identification and the Netflix Prize de-anonymization. The discussion question
that works: *the analysis was fine — so what exactly was wrong?* Forces students to separate
methodological from ethical evaluation.
**[new]** — no figure needed; a headline screenshot is enough.

## Spain / Europe-specific options

Worth having one or two so the cohort sees designs on data they could actually get:

- **COVID mobility data (INE mobile-phone study)** — a designed use of found telecom data;
  good for unit of analysis (what *is* a "trip"?) and for privacy-by-aggregation.
  Relevant assets may exist: `img/acumuladaincidencia31marzo.jpeg`, `covid_*.png`.
- **Spanish street names** — already in the course, instructor's own.
- **Spanish electoral data** — `img/electoral_data_excel.jpg` already exists and is a nice
  "this is what real data actually looks like" moment for week 2 or 3.

## Swappable current-event slot

Session 2's RQ decomposition currently runs on the Charlie Kirk / political violence example.
It works, but it dates. Keep the **structure** (topic → intuition → main RQ → RQ1/RQ2/RQ3 →
theory) and mark the slot in the .tex with a comment so it's obvious what to replace annually.
Whatever is salient in Sep 2026 will do, provided it decomposes into a descriptive, a
distributional, and a causal sub-question — that three-way split is the pedagogical point,
not the topic.

## Exercises (not examples, but they need slots)

| Exercise | Session | Time | What it does |
| :--- | :--- | :--- | :--- |
| **Audit a claim** — 3 claims, pairs, name goal + data + first doubt | 1 | 15 min | Sets participation norm in hour one |
| **Concept stress-test** — define "political violence" / "household" as rule-based, then as ideal type | 3 | 10 min | Already in the deck; keep it live, don't lecture it |
| **Find the back door** — a DAG on screen, students name what to control and what not to | 4 | 15 min | The collider lesson only sticks if they try it |
| **Name the variation** — 4 published abstracts, students identify which design template | 5 | 15 min | Best possible rehearsal for the workshop |
