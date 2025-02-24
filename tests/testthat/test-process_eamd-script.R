test_that("process_eamd processing", {
  input_file <- system.file("extdata", "sample-data-ecap2.csv", package = "oncmap")
  expect_equal(
    round(process_eamd(input_file)$adh, 4),
    data.frame(daysadh = 0.5833, daysdose = 0.7083, perdoses = 0.8333, n = 24)
  )

  report <- process_eamd(input_file, patinfo = list(day_start_time = "22:00"))
  expect_equal(report$report[2, ]$times, "")
  expect_equal(
    round(report$adh, 4),
    data.frame(daysadh = 0.3913, daysdose = 0.6087, perdoses = 0.8261, n = 23)
  )

  expect_equal(
    round(process_eamd(input_file, regimen = data.frame(periods_per_day = 1, doses_per_period = 2, days_per_week = 2, weekdays = "0,1"))$adh, 4),
    data.frame(daysadh = 0.2083, daysdose = 0.7083, perdoses = 1.25, n = 24)
  )

  nonmonit <- data.frame(
    start = c("2018-12-10", "2018-12-20"),
    end = c("2018-12-11", "2018-12-21")
  )
  report <- process_eamd(input_file, nonmonit = nonmonit)
  expect_equal(report$report[3, ]$times, "")
  expect_equal(
    round(report$adh, 4),
    data.frame(daysadh = 0.6, daysdose = 0.7, perdoses = 0.8, n = 20)
  )
})
