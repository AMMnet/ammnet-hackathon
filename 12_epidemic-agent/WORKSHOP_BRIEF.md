# Workshop Brief

## Measles Epidemic Forecasting Challenge

We have observed the early part of a measles outbreak in one community.

The file `data/observed.csv` contains reported case counts for **periods 1–8**. Each period covers **two weeks**. The CSV calls this period index `biweek`.

Your objective is to:

> Use the available evidence to forecast reported measles cases in each of periods 9–16.

You have 16 weeks of observations and must forecast the next 16 weeks. These counts describe reported cases. They do not directly measure all infections.

The organizer has already withheld the future observations. They will be released later for evaluation. Use only released outbreak observations and do not search for or reconstruct withheld outcomes.

## What is known

- The disease is measles.
- The counts describe reported cases in one community.
- Each observation covers a two-week period.
- Periods 1–8 are available; periods 9–16 are the forecast target.

Population size, vaccination coverage, reporting completeness, intervention history, calendar dates, and the historical source are not supplied. Treat missing information as unknown rather than inventing facts.

## How to approach the challenge

Model choice is open. General measles knowledge may inform the analysis, but it must remain distinct from observations about this community.

The project should make assumptions and uncertainty visible, preserve the original data, and save enough information to repeat the work. Humans review important scientific decisions.

Begin by drafting the project rules and documenting the available evidence. Scientific planning, model implementation, and forecasting follow after human review.
