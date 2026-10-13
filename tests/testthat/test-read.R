test_that("read_input handles a single-column input file", {
  # length() on a one-column tibble is 1 and is.na() returns an N-row matrix,
  # so a `length(x) == 1 && is.na(x)` read-error sentinel errors out on a
  # length > 1 condition instead of moving on to the next format
  expect_no_error(res <- read_input("onecol.csv"))
  expect_null(res$format)
})

test_that("read_input errors when the format filters leave nothing to match", {
  expect_error(
    read_input("test_ecap1.csv", include_formats = "nosuchformat"),
    "no input formats to check"
  )
  expect_error(
    read_input("test_ecap1.csv", exclude_formats = rownames(input_formats)),
    "no input formats to check"
  )
})

test_that("read_input skips a format whose filter cannot be applied", {
  fd <- input_formats["ecap1", ]
  fd$filter <- "NoSuchColumn == 1"
  expect_no_error(res <- read_input("test_ecap1.csv", formats_def = fd))
  expect_null(res$format)
  expect_match(res$log$message, "error applying filter", all = FALSE)
})

test_that("read_input still reports the underlying error for an unreadable file", {
  res <- read_input("noformat.badext")
  expect_equal(res$log[1, ]$message, "Unknown extension of file noformat.badext")
  expect_null(res$format)
})

test_that("read_input errors on a missing file rather than returning an empty result", {
  expect_error(read_input("no_such_file.csv"), "input file not found: no_such_file.csv")
})

test_that("the documented examples resolve their input files", {
  for (f in c("sample-data-ecap1.csv", "sample-data-ecap2.csv")) {
    input_file <- system.file("extdata", f, package = "oncmap")
    expect_true(nzchar(input_file))
    expect_false(is.null(read_input(input_file)$format))
  }
})

test_that("read_input joins split date and time columns", {
  res <- read_input("test_findair.csv")
  expect_equal(res$format, "findair")
  expect_equal(nrow(res$data), 9)
  expect_equal(res$data$timestamp_infile[1], "1/26/26 9:47 AM")
  expect_equal(res$data$timestamp[1], as.POSIXct("2026-01-26 09:47:00", tz = "UTC"))
  expect_false(anyNA(res$data$timestamp))
})

test_that("read_input drops rows missing part of a split timestamp", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "date,time,drug,planned,manual",
    "1/26/26,9:47 AM,Advair,11:05 AM,0",
    "1/27/26,,Advair,11:05 AM,0"
  ), f)
  res <- read_input(f)
  expect_equal(res$format, "findair")
  expect_equal(nrow(res$data), 1)
})
