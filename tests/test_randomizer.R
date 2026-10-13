# Checks the selection rules against the demo data. Run with: Rscript tests/test_randomizer.R
setwd(file.path(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)))), "..", "app"))
source("randomizer.R")
read_data <- function(f) read.csv(file.path("data", f), stringsAsFactors = FALSE)
emp <- read_data("employees.csv"); hist <- read_data("selection_history.csv")
done <- read_data("completed_tests.csv"); audit <- read_data("audit_log.csv")
check <- function(ok, what) { if (!isTRUE(ok)) stop("FAILED: ", what, call. = FALSE); cat("ok  ", what, "\n") }

for (q in 1:4) for (seed in 1:50) {
  p <- quarter_plan(emp, hist, done, audit, 2026, q, drug_pct = 25, combo_pct = 10)
  s <- draw_selection(p, seed)
  stopifnot(!anyDuplicated(s$EmployeeID))
  stopifnot(all(s$EmployeeID %in% emp$EmployeeID[emp$Status == "Active"]))
  stopifnot(setequal(unique(s$Occupation), unique(p$eligible$Occupation)))
  stopifnot(sum(s$TestFor == COMBO) == p$need_combo, sum(s$TestFor == DRUG) >= p$need_drug)
}
check(TRUE, "200 draws: no duplicates, only active employees, every occupation, quotas met")

p1 <- quarter_plan(emp, hist, done, audit, 2026, 1, 25, 10)
check(all(p1$rollover == 0), "Q1 has no rollover")
p3 <- quarter_plan(emp, hist, done, audit, 2026, 3, 25, 10)
check(p3$rollover[["combo"]] + p3$rollover[["drug"]] == 3, "Q3 carries over the 3 untested Q2 selections")
check(identical(draw_selection(p3, 42), draw_selection(p3, 42)), "same seed gives the same draw (reproducible for audits)")
check(p3$need_drug == min(max(p3$base_drug - p3$base_combo, 0) + p3$rollover[["drug"]], p3$cap),
      "drug + alcohol tests count toward the drug rate")
