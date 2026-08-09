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
