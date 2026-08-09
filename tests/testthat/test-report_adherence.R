run_test_report_adherence <- function() {
  input <- read_input("test_ecap2.xls")
  patinfo <- list(studyid = 123, start_date = "2020-03-21", end_date = "2020-07-27")
  pre <- adherence_preprocess(
    timestamps = input$data$timestamp,
    regimen = regimens[1, ],
    patinfo = patinfo
  )
  report_output <- report_adherence(
    all_periods = pre$all_periods,
    timestamps = pre$timestamps,
    med = "med",
    patinfo = patinfo
  )
  report <- report_output$report
  return(list(sum(report$dosecorr == "1"), sum(report$dosetkn == "1"), sum(report$nonmon == "1"), sum(report$doserx)))
}



test_that("report_adherence processing", {
  expect_error(report_adherence(list(1, 2, 3), timestamps = data.frame(x = 1), med = "123", patinfo = list(start_date = "2020-03-01", end_date = "2020-04-01")), "all_periods missing day")
  expect_equal(run_test_report_adherence(), list(35, 58, 0, 129))
})

test_that("adhweek covers 7 calendar days regardless of periods_per_day", {
  patinfo <- list(start_date = "2020-03-21", end_date = "2020-07-27")

  for (regimen in list(regimens[1, ], regimens[2, ])) {
    report <- process_eamd("test_ecap2.xls", regimen = regimen, patinfo = patinfo)$report
    window <- 7 * regimen$periods_per_day

    # adhweek is blank until a full 7 days of history exists, then populated
    expect_equal(sum(report$adhweek == ""), window - 1)

    # and equals the 7-day rolling opens/doserx ratio at the first filled row
    expect_equal(
      report$adhweek[window],
      as.character(round(100 * sum(report$opens[1:window]) / sum(report$doserx[1:window])))
    )
  }
})
