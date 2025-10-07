test_that("Detect formats", {
  expect_equal(read_input("test_ecap1.csv")$format, "ecap1")
  expect_equal(read_input("test_ecap2.xls")$format, "ecap2xls")
  expect_equal(read_input("test_ecap2.csv")$format, "ecap2")
  expect_equal(read_input("test_ecap3.csv")$format, "ecap3")
  # FIXME: failure unable to reproduce
  # expect_equal(read_input("test_simplemed.csv")$format, "simplemed")
  # expect_equal(read_input("test_mems.csv")$format, "mems")
  expect_equal(read_input("test_mems2.csv")$format, "mems2")
  expect_equal(read_input("test_adheretech.csv")$format, "adheretech")
  expect_equal(read_input("test_adheretech_2.csv")$format, "adheretech")
  # FIXME: NA patientid ok?
  expect_equal(read_input("test_adheretech.xlsx")$format, "adheretechxls")
  expect_equal(read_input("test_clevercap.xlsx")$format, "clevercap")
  expect_equal(read_input("noformat.csv")$format, NULL)
})
