test_that("process_eamd processing", {
  expect_equal(process_eamd("empty.csv")$log[1, ]$message, "No data.")
  expect_equal(process_eamd("noformat.badext")$log[1, ]$message, "Unknown extension of file noformat.badext")
  expect_equal(process_eamd("noformat.csv")$data, NULL)
  expect_equal(round(process_eamd("test_ecap2.xls")$adh, 4), data.frame(daysadh = 0.0495, daysdose = 0.0812, perdoses = 0.1128, n = 727))
  expect_equal(round(process_eamd("test_ecap2.xls", formats_def = input_formats[4, ])$adh, 4), data.frame(daysadh = 0.0495, daysdose = 0.0812, perdoses = 0.1128, n = 727))
  expect_equal(round(process_eamd("test_ecap2.xls", include_formats = c("ecap2xls"))$adh, 4), data.frame(daysadh = 0.0495, daysdose = 0.0812, perdoses = 0.1128, n = 727))
  expect_equal(round(process_eamd("test_ecap2.xls", exclude_formats = c("ecap1", "ecap2", "ecap3", "simplemed", "mems", "mems2", "adheretech", "adheretechxls", "ecap_old"))$adh, 4), data.frame(daysadh = 0.0495, daysdose = 0.0812, perdoses = 0.1128, n = 727))
  expect_equal(round(process_eamd("test_ecap2.xls", nonmonit = data.frame(start = "2020-03-17", end = "2020-03-19"))$adh, 4), data.frame(daysadh = 0.0495, daysdose = 0.0812, perdoses = 0.1128, n = 727))
})

test_that("process_eamd filters by medication and labels the report with it", {
  all_meds <- process_eamd("test_findair.csv")
  one_med <- process_eamd("test_findair.csv", medication = "Advair HFA")
  expect_equal(nrow(one_med$timestamps), 3)
  expect_lt(nrow(one_med$timestamps), nrow(all_meds$timestamps))
  expect_equal(unique(one_med$report$med), "Advair HFA")
  expect_equal(process_eamd("test_findair.csv", medication = "Advair HFA", med = "label")$report$med[1], "label")
})
