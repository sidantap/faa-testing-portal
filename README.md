# Drug and Alcohol Testing Portal

An R Shiny app for running FAA-style random drug and alcohol testing selections. It covers the full workflow: tracking the employee pool, computing each quarter's testing goals, drawing a random selection, and keeping an audit trail.

**Live demo:** https://sidantap.github.io/faa-testing-portal/

This is a from-scratch rebuild of an internal app I built at Atlas Air in 2025. It uses made-up employees and none of the original code, data or company systems. It runs entirely in your browser with [Shinylive](https://posit-dev.github.io/r-shinylive/), so there is no server and nothing to pay for. Changes you make last only until you reload the page.

## What it does

**Home**
- Seven KPI cards: active employees, annual drug and alcohol goals, last quarter's rollover, quarterly goals, and tests completed this quarter.
- **Edit annual goals** sets the yearly testing rates. **Quarterly headcount** shows how the average headcount behind the goals was calculated.
- **Generate Random Selection** draws this quarter's employees. Results can be downloaded as CSV, saved to the selection history and audit log, or cleared.

**Audit Log**
- Every saved randomizer run, with the pool size, counts and random seed, filterable by date and downloadable.
- A log of every employee change made in the session.

**Employee Management**
- Upload an employee list as CSV. It's validated, then merged by EmployeeID and every change is logged. A template download is included.
- Add an employee, edit occupation, base or status, mark someone terminated, or rehire them, each with a confirmation step.
- Filter by occupation, base and status. The master pool can be downloaded.

## Selection rules

Implemented in [`app/randomizer.R`](app/randomizer.R):

1. Only **active** employees are eligible. LOA and terminated employees are excluded.
2. **Annual goals** are the average eligible headcount across this year's draws so far, times each testing rate, rounded up. The defaults are 25% drug and 10% alcohol.
3. **Each quarter** takes a quarter of the annual goal, plus the **rollover**: people selected last quarter who never completed their test.
4. A drug + alcohol test also counts toward the drug rate, so the drug-only draw is the drug goal minus the drug + alcohol goal.
5. **Every occupation is represented**, nobody is picked twice in one draw, and a single draw never takes more than **half the pool**.
6. People can be selected again in later quarters, so every draw is genuinely random.
7. Each draw records its **random seed**, so an auditor can reproduce it exactly.

[`tests/test_randomizer.R`](tests/test_randomizer.R) checks these rules over 200 random draws on every push.

## Run it locally

```r
install.packages(c("shiny", "bslib"))
shiny::runApp("app")
```

## How it's deployed

GitHub Actions installs R, runs the tests, exports the app to static files with `shinylive::export()`, and publishes them to GitHub Pages. See [`.github/workflows/deploy.yml`](.github/workflows/deploy.yml).

## Layout

```
app/app.R             UI and server: tabs, KPIs, dialogs, uploads and downloads
app/randomizer.R      quarterly goals, rollover and the random draw
app/data/*.csv        made-up employees, selection history, test completions, audit log
tests/                rule checks run in CI
```

Built by [Sidanta Parajuli](https://sidantap.github.io/portfolio/).
