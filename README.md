# Codera-Hackathon
Rand-om Variables group
Variables used :  US inflation, US interest, VIX, gold, SA prime and repo, overnight fx, sa inflation, crude oil, Sa 10 year bond, Sa interest differential, 


Regression using an expanding window approsch in the model as : Each Friday, we retrain the AR(1) using all real data available up to that point, then forecast 1 month ahead — this matches the brief's "every Friday, out of sample" requirement, because the model only ever uses data that would genuinely have been known at that point in time, and never has to guess using its own past guesses.
"Training uses an expanding window starting in 1990; the model is re-estimated on every forecast Friday using all data available up to that date. The out-of-sample evaluation period is 2021–2025 (~260 weekly forecasts)."


USDZAR momentum, gold, VIX/risk sentiment, US & SA yields are the variables with most predictive value 
selected variables with clear economic channels to the rand — commodity exports (gold) and global rate differentials (US 10y, SA bond) — transformed appropriately for stationarity

Prefix	Meaning	Used for transformations used :
dl_	: 100 × log-difference ≈ weekly % change	Prices/indices like gold, VIX, USD/ZAR	A price of R30,000/oz vs a rate of 4% aren't comparable in raw units; % change puts a price on a natural, stationary scale
d_: 	first difference = weekly change in the level	Rates/yields like US 10y, SA bond	A yield is already a percentage; its meaningful move is "up 0.1 percentage points," not "up 2%". A simple difference captures that

##SUMMARY##
The goal
Every Friday from 2021 to 2025, predict what USD/ZAR will be 4 weeks later. Then check how wrong we were, and compare two forecasting methods.

The two "guessers" we compare
AR(1) — the benchmark. A dead-simple rule: "next month's rate is basically today's rate, nudged a bit." For currencies this is famously hard to beat.
Your regression — the smart model. It tries to do better by looking at other information — gold price, market fear (VIX), US and SA interest rates, and last week's currency move — to predict which way the rand will drift.
How the regression_model code works, step by step:

Step 1 — Load & check the data. Opens your merged weekly file, makes sure dates are in order and nothing's broken, and draws the USD/ZAR line as a sanity check.

Step 2 — Prepare the ingredients ("transformations"). Raw prices trend all over the place, which confuses models. So instead of using the level of each variable, we use its change (e.g. "gold went up 1.2% this week," "the US rate rose 0.1"). This is exactly what your teammate's assumption tests said to do. The thing we're trying to predict is "how much does the rand move over the next 4 weeks."

Step 3 — Pick the Fridays. Lists every Friday in 2021–2025 that has a known "4 weeks later" answer to check against. (~260 of them.)

Step 4 — The heart: the honest "no cheating" loop. For each Friday, we pretend we're standing on that day knowing only the past:

The AR(1) looks at USD/ZAR history up to that day and projects 4 weeks out.
Your regression learns the relationship between the extra variables and the rand's future move — but only from weeks where the outcome had already happened (so it can never peek at the future) — then applies that to today's numbers to make its prediction.
We record both guesses and the real value that actually occurred.
This "walk through history one Friday at a time, only using the past" is what out-of-sample means, and it's exactly what the rules demand.

Step 5 — Score them. For each model we measure the typical size of its mistakes (RMSE — lower is better). We also built a third guesser: the Combined model, which just averages the AR(1) and the regression, because averaging two okay guesses often gives a better one.

Step 6 — Charts. Forecasts vs reality, how the error evolves over time, and a bar chart of the three RMSEs with the numbers on it (a rule requirement).

Step 7 — Save the results to a file.

What has actually happened (your results)
Model	Typical error (RMSE)
AR(1) benchmark	0.5619
Your regression	0.5696
Combined	0.5625
In words: your Combined model is essentially tied with the benchmark (0.5625 vs 0.5619 — a difference of about half a cent on the rand). The extra economic variables almost match the simple rule but don't clearly beat it.

Why that's not a failure: predicting currencies a month ahead is one of the hardest problems in economics — the simple "it'll be about the same" rule is shockingly good, and most professional models can't beat it either. Matching it with a transparent, honestly-tested model is a solid, defensible result — and the rules reward your rigor and explanation, not just the RMSE.




Coeffcient results: 
"Of our five drivers, only recent USD/ZAR momentum (mean-reverting) and gold are statistically significant. The rate variables carry the expected sign but are insignificant at the monthly horizon. Notably, gold enters positively — consistent with gold acting as a global risk-off signal (rand weakens in risk-off episodes) rather than through SA's gold exports. The generally weak significance reflects the well-known difficulty of beating a random walk in exchange-rate forecasting."