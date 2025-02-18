#' @title Process input file and return adherence report
#' @param infile Input CSV file name
#' @param include_formats Which formats to include in checking
#' @param exclude_formats Which formats to exclude from checking
#' @param formats_def New formats definition
#' @param infile_data_output Include infile data frame in the result
#' @param regimen Regimen - regimen definition
#' @param patinfo Patient info - patient specific information
#' @param nonmonit Non-monitored date intervals
#' @param med Medication name
#' @param adhstart Report adherence start date
#' @param adhend Report adherence end date
#' @examples
#' # print(process_eamd("tests/testthat/ecap1.csv"))
#' @export
process_eamd <- function(infile,
                    include_formats = NULL,
                    exclude_formats = NULL,
                    formats_def = NULL,
                    infile_data_output = FALSE,
                    regimen = NULL,
                    patinfo = NULL,
                    nonmonit = NULL,
                    med = "",
                    adhstart = NULL,
                    adhend = NULL) {
  data <- read_input(infile, include_formats, exclude_formats, formats_def, infile_data_output)
  if (is.null(data$data)) {
    return(data)
  }
  if (is.null(regimen)) regimen <- regimens[1,]
  if (is.null(patinfo)) patinfo <- list(day_start_time='00:00')
  pre <- adherence_preprocess(data$data$timestamp, regimen, patinfo=patinfo, nonmonit=nonmonit)
  report_output <- report_adherence(pre$all_periods, pre$timestamps, med,patinfo=patinfo, adhstart = adhstart, adhend = adhend)
  return(report_output)
}
