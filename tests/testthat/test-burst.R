burst_regimen <- function(actuations = "8,3", window = "120,60") {
  data.frame(
    name = "burst test", periods_per_day = 1, doses_per_period = 2, min_wait = 0,
    days_per_week = 7, weekdays = "all",
    burst_actuations = actuations, burst_window = window
  )
}

at <- function(day, secs) as.POSIXct(paste0("2026-01-0", day, " 10:00:00"), tz = "UTC") + secs

test_that("every actuation in a burst is removed", {
  ts <- c(
    at(1, c(0, 30)), # 2 in a minute -- not a burst
    at(2, c(0, 20, 40)), # 3 in a minute -- burst, all removed
    at(3, c(0, 61, 122)), # 3 over 2 minutes -- not a burst, overuse kept
    at(4, c(-7200, 0, 10, 20)) # earlier dose plus a burst -- only burst actuations go
  )
  pre <- adherence_preprocess(ts, burst_regimen())
  expect_equal(pre$all_periods$opens, c(2, 0, 3, 1))
  expect_equal(pre$timestamps$excluded, c(0, 0, 5, 5, 5, 0, 0, 0, 0, 5, 5, 5))
})

test_that("burst window rule counts actuations within the window", {
  reg <- burst_regimen("8", "120")
  pre <- adherence_preprocess(at(1, seq(0, by = 15, length.out = 8)), reg)
  expect_equal(pre$all_periods$opens, 0)
  expect_equal(sum(pre$timestamps$excluded == 5), 8)

  # 7 within 2 minutes is under the threshold
  pre <- adherence_preprocess(at(1, seq(0, by = 15, length.out = 7)), reg)
  expect_equal(pre$all_periods$opens, 7)
  # the window is half-open: 8 actuations spanning exactly 120 seconds are not a burst
  pre <- adherence_preprocess(at(1, c(seq(0, 102, by = 17), 120)), reg)
  expect_equal(pre$all_periods$opens, 8)
})

test_that("burst capping is off when the regimen does not define it", {
  ts <- at(1, c(0, 20, 40))
  reg <- burst_regimen(NA, NA)
  expect_equal(adherence_preprocess(ts, reg)$all_periods$opens, 3)
  reg$burst_actuations <- NULL
  reg$burst_window <- NULL
  expect_equal(adherence_preprocess(ts, reg)$all_periods$opens, 3)
  expect_equal(adherence_preprocess(ts, regimens["daily2", ])$all_periods$opens, 1) # min_wait only
})

test_that("burst parameters are validated", {
  expect_error(adherence_preprocess(at(1, 0), burst_regimen("8,3", "120")), "equal length")
  expect_error(adherence_preprocess(at(1, 0), burst_regimen("8,x", "120,60")), "comma separated numbers")
})
