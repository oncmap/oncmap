test_that("report_input labels every input row with whether and why it was used", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "date,time,drug,planned,manual",
    "1/5/26,9:00 AM,Advair,11:05 AM,0", # before monitoring start
    "1/6/26,9:00 AM,Advair HFA,11:05 AM,0", # other medication
    "1/6/26,,Advair,11:05 AM,0", # missing time
    "1/6/26,99:00 XM,Advair,11:05 AM,0", # unparseable
    "1/6/26,9:00 AM,Advair,11:05 AM,0", # used
    "1/6/26,9:05 AM,Advair,11:05 AM,0", # within min_wait
    "1/7/26,9:00 AM,Advair,11:05 AM,0", # burst: all three removed
    "1/7/26,9:00 AM,Advair,11:05 AM,0",
    "1/7/26,9:00 AM,Advair,11:05 AM,0"
  ), f)
  reg <- data.frame(
    name = "x", periods_per_day = 1, doses_per_period = 2, min_wait = 0,
    days_per_week = 7, weekdays = "all", burst_actuations = "3", burst_window = "60"
  )
  r <- process_eamd(f, regimen = reg, medication = "Advair",
                    patinfo = list(start_date = "2026-01-06", end_date = "2026-01-07"))
  rep <- r$input_report
  expect_equal(nrow(rep), 9)
  expect_equal(rep$drug, c("Advair", "Advair HFA", rep("Advair", 7)))
  expect_equal(rep$oncmap_reason, c(
    "before monitoring start", "medication not selected", "missing timestamp",
    "unparseable timestamp", NA, NA, rep("part of an actuation burst", 3)
  ))
  expect_equal(rep$oncmap_used, is.na(rep$oncmap_reason))
  expect_equal(rep$oncmap_timestamp[5], as.POSIXct("2026-01-06 09:00:00", tz = "UTC"))
  expect_true(is.na(rep$oncmap_timestamp[2]))
  # used rows are exactly the counted actuations
  expect_equal(sum(rep$oncmap_used), sum(r$all_periods$opens))

  r <- process_eamd(f, regimen = transform(reg, min_wait = 600), medication = "Advair",
                    patinfo = list(start_date = "2026-01-06", end_date = "2026-01-07"))
  expect_equal(r$input_report$oncmap_reason[6], "within min_wait of the previous actuation")
  # infile_data is used for the report but not returned unless asked for
  expect_null(r$input_data$infile_data)
})

test_that("report_input reports format filter drops", {
  rep <- process_eamd("test_adheretech.csv")$input_report
  expect_equal(nrow(rep), nrow(read_input("test_adheretech.csv", infile_data_output = TRUE)$infile_data))
  expect_equal(rep$oncmap_reason[rep$Status == "MISSED"], rep("format filter: Status!='MISSED'", 2))
})

test_that("report_input checks its arguments", {
  input <- read_input("test_findair.csv")
  expect_error(report_input(input, data.frame()), "infile_data_output = TRUE")
  input <- read_input("test_findair.csv", infile_data_output = TRUE)
  pre <- adherence_preprocess(input$data$timestamp[-1], regimens[1, ])
  expect_error(report_input(input, pre$timestamps), "do not come from")
})
