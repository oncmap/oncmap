#' @title List the medication and manual-entry choices in an input file
#' @description Returns the distinct values a UI can offer for the
#'   \code{medication} and \code{include_manual} options of
#'   \code{\link{process_eamd}}. When a choice has a single value there is
#'   nothing to choose, so a UI can default to it or hide the control.
#' @param input Output of \code{\link{read_input}}. Format selection (e.g.
#'   \code{include_formats} to force a format) is done there. If it was called
#'   with \code{medication}, \code{manual} covers only that medication's actuations
#'   while \code{medications} still lists every medication in the file.
#' @return A list of output variables
#' \itemize{
#'   \item \code{format} - Detected input format name (\code{NULL} if none matched)
#'   \item \code{medications} - Medications present (\code{NULL} if the format has no medication column)
#'   \item \code{manual} - Manual-entry flags present: \code{FALSE} device-recorded, \code{TRUE} manually
#'     entered (\code{NULL} if the format does not mark manual entries)
#' }
#' @examples
#' input_file <- system.file("extdata", "sample-data-ecap2.csv", package = "oncmap")
#' input_choices(read_input(input_file))
#' @export
input_choices <- function(input) {
  if (!is.list(input) || !all(c("format", "data", "medications") %in% names(input))) {
    stop("input must be the output of read_input()")
  }
  list(
    format = input$format,
    medications = if (length(input$medications) > 0) sort(input$medications) else NULL,
    manual = if (!is.null(input$data[["manual"]])) sort(unique(input$data[["manual"]])) else NULL
  )
}
