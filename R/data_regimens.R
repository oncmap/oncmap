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
#'   \item{\code{actuations_per_dose}}{integer Prescribed actuations per dose (e.g. inhaler puffs); default 1}
#'   \item{\code{dose_group_window}}{integer Actuations at most this many seconds after the previous one form one dose (NA disables)}
#' }
#' @details
#' Burst removal: an actuation belongs to a burst when any rolling window of
#' \code{burst_window[i]} seconds holds at least \code{burst_actuations[i]}
#' actuations, for any rule \code{i}. Every actuation in a burst is removed
#' from processing (\code{excluded = 5}). For example
#' \code{burst_actuations = "8,3"}, \code{burst_window = "120,60"} flags 8 or
#' more actuations within 2 minutes, or 3 or more within 1 minute.
#'
#' Dose grouping: with \code{dose_group_window} set, counted actuations chain
#' into one dose while each is at most \code{dose_group_window} seconds after
#' the previous one (\code{60} joins actuations in consecutive minutes for
#' minute-resolution data). A dose counts in the period of its first actuation.
#' \code{opens} then counts doses; a period is correct (\code{dosecorr}) when it
#' has \code{doses_per_period} doses each of at least \code{actuations_per_dose}
#' actuations, and \code{perdoses} counts actuations capped per dose at
#' \code{actuations_per_dose} against the prescribed actuations. \code{min_wait}
#' then applies between doses -- from the end of one to the start of the next --
#' instead of between actuations. Bursts are removed before grouping.
"regimens"
