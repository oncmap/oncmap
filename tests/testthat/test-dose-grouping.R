grouping_regimen <- function(...) {
  reg <- data.frame(
    name = "2 puffs once daily", periods_per_day = 1, doses_per_period = 1, min_wait = 0,
    days_per_week = 7, weekdays = "all", burst_actuations = NA, burst_window = NA,
    actuations_per_dose = 2, dose_group_window = 60
  )
  args <- list(...)
  reg[names(args)] <- args
  reg
}

at <- function(day, hhmmss) as.POSIXct(paste0("2026-01-0", day, " ", hhmmss), tz = "UTC")

test_that("actuations in consecutive minutes form one dose", {
  ts <- c(
    at(1, c("09:00:00", "09:01:00")), # one dose of 2 -- correct
    at(2, c("09:00:00", "09:02:00")), # two minutes apart -- two doses of 1
    at(3, "09:00:00"), # one dose of 1 -- partial
    at(4, c("09:00:00", "09:00:30", "09:01:00", "09:02:00")), # chained -- one dose of 4
    at(5, "23:59:59") # keeps day 5 in range; one dose of 1
  )
  pre <- adherence_preprocess(ts, grouping_regimen())
  ap <- pre$all_periods
  expect_equal(ap$opens, c(1, 2, 1, 1, 1))
  expect_equal(ap$fulldoses, c(1, 0, 0, 1, 0))
  expect_equal(ap$actuations, c(2, 2, 1, 2, 1)) # capped per dose at 2
  expect_equal(ap$actuationsrx, rep(2, 5))
  expect_equal(pre$timestamps$dose, c(1, 1, 2, 3, 4, 5, 5, 5, 5, 6))

  adh <- report_adherence(ap, pre$timestamps, "med")$adh
  expect_equal(adh$daysadh, 2 / 5) # days 1 and 4: one dose of at least 2
  expect_equal(adh$daysdose, 5 / 5)
  expect_equal(adh$perdoses, 8 / 10)
})

test_that("a dose counts in the period of its first actuation", {
  reg <- grouping_regimen(periods_per_day = 2)
  pre <- adherence_preprocess(at(1, c("11:59:00", "12:00:00")), reg)
  expect_equal(pre$all_periods$opens, c(1, 0))
  expect_equal(pre$all_periods$actuations, c(2, 0))
})

test_that("with grouping min_wait applies between doses", {
  reg <- grouping_regimen(doses_per_period = 2, min_wait = 600)
  pre <- adherence_preprocess(at(1, c("09:00:00", "09:01:00", "09:05:00", "09:20:00")), reg)
  # 09:05 starts 4 minutes after the first dose ends -- excluded whole; 09:20 is
  # 15 minutes after it, so it counts
  expect_equal(pre$timestamps$excluded, c(0, 0, 1, 0))
  expect_equal(pre$timestamps$dose, c(1, 1, NA, 2))
  expect_equal(pre$all_periods$opens, 2)
})

test_that("bursts are removed before grouping", {
  ts <- at(1, c("09:00:00", "09:00:10", "09:00:20", "09:00:30", "09:10:00", "09:10:30"))
  pre <- adherence_preprocess(ts, grouping_regimen(burst_actuations = "3", burst_window = "60"))
  expect_equal(pre$timestamps$excluded, c(5, 5, 5, 5, 0, 0))
  expect_equal(pre$timestamps$dose, c(NA, NA, NA, NA, 1, 1))
  expect_equal(pre$all_periods$fulldoses, 1)
})

test_that("excluded manual entries are not part of a dose", {
  ts <- at(1, c("09:00:00", "09:01:00", "09:02:00"))
  pre <- adherence_preprocess(ts, grouping_regimen(), manual = c(FALSE, TRUE, FALSE), include_manual = FALSE)
  # without the 09:01 manual entry the other two are two minutes apart
  expect_equal(pre$timestamps$dose, c(1, NA, 2))
  expect_equal(pre$all_periods$opens, 2)
})

test_that("input_report shows the dose of each used row", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "date,time,drug,planned,manual",
    "1/6/26,9:00 AM,Advair,11:05 AM,0",
    "1/6/26,9:01 AM,Advair,11:05 AM,0",
    "1/6/26,5:00 PM,Advair,5:04 PM,0",
    "1/6/26,5:00 PM,Advair HFA,5:04 PM,0"
  ), f)
  rep <- process_eamd(f, regimen = grouping_regimen(periods_per_day = 2), medication = "Advair")$input_report
  expect_equal(rep$oncmap_dose, c(1, 1, 2, NA))
})

test_that("dose grouping parameters are validated", {
  ts <- at(1, "09:00:00")
  expect_error(adherence_preprocess(ts, grouping_regimen(actuations_per_dose = 0)), "at least 1")
  expect_error(adherence_preprocess(ts, grouping_regimen(dose_group_window = -1)), "at least 0")
  expect_error(adherence_preprocess(ts, grouping_regimen(dose_group_window = "x")), "number of seconds")
})

test_that("a format's min_wait overrides the regimen's", {
  f <- test_path("test_findair.csv")
  regimen <- grouping_regimen(periods_per_day = 2, min_wait = 600)
  rep <- process_eamd(f, regimen = regimen, medication = "Advair")$input_report
  # FindAir sets min_wait = 0: the 9:50 puff, 3 minutes after 9:47, is a dose of its own
  expect_equal(rep$oncmap_used[1:4], rep(TRUE, 4))
  expect_equal(rep$oncmap_dose[1:4], 1:4)

  # a format without min_wait keeps the regimen's
  formats <- input_formats["findair", ]
  formats$min_wait <- NA
  rep <- process_eamd(f, formats_def = formats, regimen = regimen, medication = "Advair")$input_report
  expect_equal(rep$oncmap_reason[2], "within min_wait of the previous actuation")
})
