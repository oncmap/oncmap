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
#'   \item{\code{burst_actuations}}{string Comma separated actuation counts that make a burst, paired with \code{burst_window} (NA disables)}
#'   \item{\code{burst_window}}{string Comma separated rolling window lengths (in seconds) for \code{burst_actuations}}
#' }
#' @details
#' Burst capping: an actuation belongs to a burst when any rolling window of
#' \code{burst_window[i]} seconds holds at least \code{burst_actuations[i]}
#' actuations, for any rule \code{i}. In a dosing period containing a burst,
#' the latest burst actuations are excluded (\code{excluded = 5}) until the
#' period counts no more than \code{doses_per_period}. For example
#' \code{burst_actuations = "8,3"}, \code{burst_window = "120,60"} flags 8 or
#' more actuations within 2 minutes, or 3 or more within 1 minute.
"regimens"
