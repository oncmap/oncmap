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
#' @return A list containing variables:
#' \itemize{
#'   \item \code{report} - Per period adherence statistic
#'   \item \code{adh} - Summary adherence statistic
#'   \item \code{timestamps} - Raw input data
#'   \item \code{patient_id} - Inferred Patient ID
#' }
#' @examples
#' input_file <- system.file("extdata", "sample-data-ecap2.csv", package = "oncmap")
#' report <- process_eamd("tests/testthat/ecap1.csv")
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
  if (is.null(regimen)) regimen <- regimens[1, ]
  if (is.null(patinfo)) patinfo <- list(day_start_time = "00:00")
  pre <- adherence_preprocess(data$data$timestamp, regimen, patinfo = patinfo, nonmonit = nonmonit)
  report_output <- report_adherence(pre$all_periods, pre$timestamps, med, patinfo = patinfo, adhstart = adhstart, adhend = adhend)
  report_output$all_periods <- pre$all_periods
  report_output$timestamps <- pre$timestamps
  report_output$patient_id <- data$patient_id
  report_output$input_data <- data
  return(report_output)
}
