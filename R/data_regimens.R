#' @title Adherence regimens definitions
#' @description Defines built-in regimen definitions.
#' @format One regimen per row described by the following variables:
#' \describe{
#'   \item{\code{name}}{character A name of the regimen}
#'   \item{\code{doses_per_period}}{integer Number of doses per period}
#'   \item{\code{periods_per_day}}{integer Number of periods per day}
#'   \item{\code{min_wait}}{integer Minimum wait time (in seconds) between actuations}
#'   \item{\code{days_per_week}}{integer Number of active days per week}
#'   \item{\code{weekdays}}{string Specific days per week when active}
#' }
"regimens"
