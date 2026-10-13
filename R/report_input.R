#' @title Report how each input row was used
#' @description Labels every row of the input file with whether its actuation
#'   counted towards adherence, and if not, why.
#' @param input Output of \code{\link{read_input}} called with \code{infile_data_output = TRUE}
#' @param timestamps \code{timestamps} data frame from \code{\link{adherence_preprocess}}
#'   run on \code{input$data$timestamp}
#' @return The input rows (\code{input$infile_data}) with added columns:
#' \itemize{
#'   \item \code{oncmap_timestamp} - Parsed actuation timestamp
#'   \item \code{oncmap_used} - \code{TRUE} if the actuation was counted
#'   \item \code{oncmap_reason} - Why the row was not used (\code{NA} if used)
#' }
#' @examples
#' input_file <- system.file("extdata", "sample-data-ecap2.csv", package = "oncmap")
#' report <- process_eamd(input_file)
#' head(report$input_report)
#' @export
report_input <- function(input, timestamps) {
  if (is.null(input$infile_data)) stop("input has no infile_data -- call read_input() with infile_data_output = TRUE")

  n <- nrow(input$infile_data)
  rows <- input$data$input_row
  reason <- rep(NA_character_, n)
  reason[input$dropped$input_row] <- input$dropped$reason
  oncmap_timestamp <- rep(as.POSIXct(NA, tz = "UTC"), n)
  oncmap_timestamp[rows] <- input$data$timestamp

  # adherence_preprocess() sorts the timestamps; order() is stable, so ties map
  # back to the input rows in the same order they were excluded in
  sorted <- order(input$data$timestamp)
  if (nrow(timestamps) != length(sorted) ||
      !identical(as.numeric(timestamps$timestamps), as.numeric(input$data$timestamp[sorted]))) {
    stop("timestamps do not come from input$data$timestamp")
  }
  excluded <- rep(NA, length(sorted))
  excluded[sorted] <- timestamps$excluded

  exclusion_reasons <- c(
    "1" = "within min_wait of the previous actuation",
    "2" = "before monitoring start",
    "3" = "after monitoring end",
    "4" = "non-monitored interval",
    "5" = "burst over prescribed doses per period"
  )
  excl <- excluded != 0
  reason[rows[excl]] <- exclusion_reasons[as.character(excluded[excl])]

  out <- input$infile_data
  out$oncmap_timestamp <- oncmap_timestamp
  out$oncmap_used <- is.na(reason)
  out$oncmap_reason <- reason
  out
}
