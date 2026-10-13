# Quarterly selection for FAA random drug and alcohol testing (49 CFR Part 40 style rules).
#
# Rules this models:
#   * Only active, safety-sensitive employees are eligible.
#   * Annual goals come from the average eligible headcount across the year's draws so far.
#   * Each quarter takes a quarter of the annual goal, plus any tests selected last
#     quarter that were never completed (rollover).
#   * A drug + alcohol test also counts toward the drug rate, so the drug-only draw is the
#     drug goal minus the drug + alcohol goal.
#   * Every occupation is represented, nobody is picked twice in one draw, and no single
#     draw takes more than half the pool.
#   * Employees can be selected again in later quarters, as FAA rules require.

OCCUPATIONS <- c(
  ACD = "Aircraft Dispatcher",
  ACM = "Aircraft Maintenance",
  FCR = "Flight Crewmember",
  FIN = "Flight Instructor",
  GSC = "Ground Security Coordinator"
)
COMBO <- "Drug + Alcohol"
DRUG  <- "Drug Only"

quarter_plan <- function(employees, history, completed, audit, year, quarter, drug_pct, combo_pct) {
  eligible <- employees[employees$Status == "Active", , drop = FALSE]
  headcount_now <- nrow(eligible)

  # Headcount recorded at each earlier draw this year, plus the pool as it stands now
  earlier <- audit$PoolSize[audit$Year == year & audit$Quarter < quarter]
  avg_headcount <- mean(c(earlier, headcount_now))

  annual_combo <- ceiling(avg_headcount * combo_pct / 100)
  annual_drug  <- ceiling(avg_headcount * drug_pct / 100)
  base_combo <- ceiling(annual_combo / 4)
  base_drug  <- ceiling(annual_drug / 4)

  # Rollover: selected last quarter but no completed test on record
  rollover <- c(combo = 0, drug = 0)
  if (quarter > 1) {
    last_q <- history[history$Year == year & history$Quarter == quarter - 1, , drop = FALSE]
    done <- completed$EmployeeID[completed$Year == year & completed$Quarter == quarter - 1 &
                                   completed$TestStatus == "completed"]
    missed <- last_q[!last_q$EmployeeID %in% done, , drop = FALSE]
    rollover <- c(combo = sum(missed$TestFor == COMBO), drug = sum(missed$TestFor == DRUG))
  }

  cap <- floor(headcount_now * 0.5)
  list(
    eligible = eligible, headcount_now = headcount_now, avg_headcount = avg_headcount,
    annual_combo = annual_combo, annual_drug = annual_drug,
    base_combo = base_combo, base_drug = base_drug, rollover = rollover, cap = cap,
    need_combo = min(base_combo + rollover[["combo"]], cap),
    need_drug  = min(max(base_drug - base_combo, 0) + rollover[["drug"]], cap)
  )
}

draw_selection <- function(plan, seed) {
  set.seed(seed)
  pool <- plan$eligible
  pick <- function(df, n) if (n <= 0 || nrow(df) == 0) df[0, ] else df[sample.int(nrow(df), min(n, nrow(df))), , drop = FALSE]

  # One person from every occupation first
  one_per_occ <- do.call(rbind, lapply(split(pool, pool$Occupation), pick, n = 1))
  combo <- pick(one_per_occ, min(nrow(one_per_occ), plan$need_combo))
  drug  <- one_per_occ[!one_per_occ$EmployeeID %in% combo$EmployeeID, , drop = FALSE]

  # Then fill the remaining slots from everyone else, never picking anyone twice
  rest  <- pool[!pool$EmployeeID %in% one_per_occ$EmployeeID, , drop = FALSE]
  combo <- rbind(combo, pick(rest, plan$need_combo - nrow(combo)))
  rest  <- rest[!rest$EmployeeID %in% combo$EmployeeID, , drop = FALSE]
  drug  <- rbind(drug, pick(rest, plan$need_drug - nrow(drug)))

  out <- rbind(transform(combo, TestFor = rep(COMBO, nrow(combo))),
               transform(drug,  TestFor = rep(DRUG,  nrow(drug))))
  out[order(out$Occupation, out$Name), c("EmployeeID", "Name", "Occupation", "Base", "TestFor")]
}
