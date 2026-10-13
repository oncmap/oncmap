manual_file <- function() {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "date,time,drug,planned,manual",
    "1/6/26,9:00 AM,Advair,11:05 AM,1", # manual
    "1/6/26,9:00 AM,Advair,11:05 AM,0", # same minute as the manual one
    "1/6/26,9:00 AM,Advair,11:05 AM,0",
    "1/7/26,9:00 AM,Advair,11:05 AM,1", # manual only
    "1/8/26,9:00 AM,Advair HFA,11:05 AM,0"
  ), f)
  f
}

burst3 <- data.frame(
  name = "x", periods_per_day = 1, doses_per_period = 2, min_wait = 0,
  days_per_week = 7, weekdays = "all", burst_actuations = "3", burst_window = "60"
)

test_that("read_input normalises the manual column to a logical flag", {
  res <- read_input(manual_file())
  expect_equal(res$data$manual, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_null(read_input("test_adheretech.csv")$data[["manual"]])
})

test_that("process_eamd counts manual entries unless include_manual = FALSE", {
  f <- manual_file()
  with_manual <- process_eamd(f, regimen = burst3)
  # 3 in a minute on day 1 is a burst, capped at 2
  expect_equal(with_manual$all_periods$opens, c(2, 1, 1))
  expect_equal(with_manual$input_report$oncmap_reason[3], "burst over prescribed doses per period")

  without <- process_eamd(f, regimen = burst3, include_manual = FALSE)
  # the manual entry is set aside first, so day 1 is no longer a burst
  expect_equal(without$all_periods$opens, c(2, 0, 1))
  expect_equal(without$input_report$oncmap_reason, c("manual entry excluded", NA, NA, "manual entry excluded", NA))
  expect_equal(sum(without$input_report$oncmap_used), sum(without$all_periods$opens))
})

test_that("excluded manual entries do not trigger min_wait", {
  reg <- transform(burst3, min_wait = 600, burst_actuations = NA, burst_window = NA)
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "date,time,drug,planned,manual",
    "1/6/26,9:00 AM,Advair,11:05 AM,1",
    "1/6/26,9:05 AM,Advair,11:05 AM,0"
  ), f)
  expect_equal(process_eamd(f, regimen = reg)$input_report$oncmap_reason[2], "within min_wait of the previous actuation")
  expect_equal(process_eamd(f, regimen = reg, include_manual = FALSE)$all_periods$opens, 1)
})

test_that("excluding manual entries works when every entry is manual", {
  pre <- adherence_preprocess(as.POSIXct(c("2026-01-06 09:00", "2026-01-07 09:00"), tz = "UTC"),
                              regimens[1, ], manual = c(TRUE, TRUE), include_manual = FALSE)
  expect_equal(pre$timestamps$excluded, c(6, 6))
  expect_equal(pre$all_periods$opens, c(0, 0))
})

test_that("include_manual has no effect for formats without a manual column", {
  expect_equal(process_eamd("test_adheretech.csv", include_manual = FALSE)$adh, process_eamd("test_adheretech.csv")$adh)
})

test_that("adherence_preprocess validates manual", {
  ts <- as.POSIXct("2026-01-06 09:00", tz = "UTC")
  expect_error(adherence_preprocess(ts, regimens[1, ], manual = c(TRUE, FALSE)), "same length")
  expect_error(adherence_preprocess(ts, regimens[1, ], manual = "1"), "logical vector")
})

test_that("input_choices lists the medication and manual values in a file", {
  f <- manual_file()
  expect_equal(input_choices(read_input(f)), list(format = "findair", medications = c("Advair", "Advair HFA"), manual = c(FALSE, TRUE)))
  expect_equal(input_choices(read_input("test_adheretech.csv")), list(format = "adheretech", medications = "eACT", manual = NULL))
  expect_equal(input_choices(read_input("test_ecap1.csv")), list(format = "ecap1", medications = NULL, manual = NULL))
  expect_equal(input_choices(read_input("noformat.csv")), list(format = NULL, medications = NULL, manual = NULL))
})

test_that("input_choices follows read_input's format and medication selection", {
  f <- manual_file()
  # forcing a format that does not fit leaves nothing to choose
  expect_equal(input_choices(read_input(f, include_formats = "ecap1"))$format, NULL)
  expect_equal(input_choices(read_input(f, include_formats = "findair"))$format, "findair")
  # manual flags narrow to the selected medication; medications still lists all
  hfa <- input_choices(read_input(f, medication = "Advair HFA"))
  expect_equal(hfa$medications, c("Advair", "Advair HFA"))
  expect_equal(hfa$manual, FALSE)
})

test_that("input_choices rejects anything but read_input output", {
  expect_error(input_choices("test_findair.csv"), "output of read_input")
})
