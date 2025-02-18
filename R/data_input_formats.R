#' @title Adherence input format definitions
#' @description Defines input format parameters to apply when reading input files.
#' @format One input format per row described by the following variables:
#' \describe{
#'   \item{\code{skip_header_lines}}{double Number of lines to skip before reading data}
#'   \item{\code{header_line_patientid}}{character Regex to apply to the header line to extract patient ID}
#'   \item{\code{patientid_filename}}{logical Patient id is embedded in the filename}
#'   \item{\code{deviceid_header}}{character Device ID column in the input data}
#'   \item{\code{headers}}{character Comma separated list of expected column headers}
#'   \item{\code{patientid_header}}{character Patient ID column in the input data}
#'   \item{\code{datetime_header}}{character Actuation Date/Time column in the input data}
#'   \item{\code{datetime_format}}{character Actuation Date/Time format}
#'   \item{\code{filter}}{character Inclusion/Exclusion filter to apply on the input data}
#'   \item{\code{tz_colon_fix}}{logical Fix for when TZ contains with ':'}
#' }
"input_formats"
