run_test_adherence_preprocess <- function() {
  input <- read_input("test_ecap2.xls")
  start <- as.POSIXct(c("2020-05-10", "2020-06-06"))
  end <- as.POSIXct(c("2020-05-20", "2020-06-13"))
  nonmonit <- data.frame(start = start, end = end)
  pre <- adherence_preprocess(
    input$data$timestamp,
    regimens[1, ],
    list(start_date = "2020-03-21", end_date = "2020-07-27"),
    nonmonit
  )
  return(list(sum(pre$timestamps$excluded), sum(pre$all_periods$opens), sum(pre$timestamps$excluded == 0), nrow(pre$all_periods)))
}

run_test_adherence_preprocess2 <- function() {
  input <- read_input("test_ecap2.xls")
  start <- as.POSIXct(c("2020-05-10", "2020-06-06"))
  end <- as.POSIXct(c("2020-05-20", "2020-06-13"))
  nonmonit <- data.frame(start = start, end = end)
  pre <- adherence_preprocess(
    input$data$timestamp,
    regimens[2, ],
    list(start_date = "2020-03-21", end_date = "2020-07-27"),
    nonmonit
  )
  return(list(sum(pre$timestamps$excluded), sum(pre$all_periods$opens), sum(pre$timestamps$excluded == 0), nrow(pre$all_periods)))
}


test_that("adherence_preprocess processing", {
  expect_error(adherence_preprocess(list(1, 2, 3)), "timestamps type error")
  expect_error(adherence_preprocess(as.POSIXct(NULL)), "empty timestamps list")
  expect_error(adherence_preprocess(c(as.POSIXct("2023-01-01")), regimen=c(1, 2, 3)), "regimen type error")
  expect_error(adherence_preprocess(c(as.POSIXct("2023-01-01")), regimen=data.frame(a=c(1,2), b=c(1,2))), "expecting regimen parameter as one line data.frame")
  expect_error(adherence_preprocess(c(as.POSIXct("2023-01-01")), regimen=data.frame(1, 2, 3), patinfo=c(1, 2, 3)), "patinfo type error")
  expect_equal(run_test_adherence_preprocess(), list(63, 66, 66, 129))
  expect_equal(run_test_adherence_preprocess2(), list(63, 66, 66, 258))
})
