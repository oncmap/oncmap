#' @title Pre-process time data for adherence
#' @param timestamps Input timestamps - vector of timestamps
#' @param regimen Regimen - regimen definition
#' @param patinfo Patient info - patient specific information
#' @param nonmonit Non-monitored date intervals
#' @param manual Logical vector, parallel to \code{timestamps}, marking manually entered actuations (\code{NULL} if unknown)
#' @param include_manual Count manual entries. When \code{FALSE} they are excluded (code 6) and ignored
#'   entirely -- they neither trigger \code{min_wait} nor take part in burst detection
#' @return A list of output variables
#' \itemize{
#'   \item \code{all_periods} - Processed timestamps into periods applying input parameters: \code{opens}
#'     counts doses, \code{fulldoses} doses of at least \code{actuations_per_dose} actuations,
#'     \code{actuations} counted actuations capped per dose at \code{actuations_per_dose}, and
#'     \code{actuationsrx} the prescribed actuations
#'   \item \code{timestamps} - Sorted timestamps with an \code{excluded} code: 0 counted,
#'     1 within \code{min_wait} of the previous actuation, 2 before start date, 3 after end date,
#'     4 non-monitored interval, 5 burst over the prescribed doses per period (see \code{\link{regimens}}),
#'     6 manual entry excluded; and \code{dose}, the dose each counted actuation belongs to (see
#'     \code{dose_group_window} in \code{\link{regimens}})
#' }
#' @importFrom readr read_csv cols
#' @importFrom methods is
#' @importFrom dplyr between if_else select mutate
#' @import lubridate
#' @export
adherence_preprocess <- function(timestamps, regimen, patinfo = list(), nonmonit = data.frame(),
                                 manual = NULL, include_manual = TRUE) {
  # Examples:
  #     timestamps <- input$data$timestamp
  #     regimen <- regimens[1,]
  #     patinfo <- list(start_date='2020-03-21',end_date='2020-07-27')
  #     nonmonit <- data.frame(start=c("2018-12-10","2018-12-20"),end=c("2018-12-11","2018-12-21"))

  # validate inputs

  # timestamps -- list of timestamps of openings
  if (!is(timestamps, "POSIXct")) stop("timestamps type error")
  if (length(timestamps) == 0) stop("empty timestamps list")
  if (!is.null(manual) && (!is.logical(manual) || length(manual) != length(timestamps))) {
    stop("manual must be a logical vector the same length as timestamps")
  }

  debug_print("timestamps check \u2713")

  #  regimen --
  if (!is(regimen, "data.frame")) stop("regimen type error")
  if (nrow(regimen) != 1) stop("expecting regimen parameter as one line data.frame")
  if (is(regimen$periods_per_day, "NULL")) regimen$periods_per_day <- 1
  if (is(regimen$doses_per_period, "NULL")) regimen$doses_per_period <- 1
  if (is(regimen$min_wait, "NULL")) regimen$min_wait <- 600 # seconds
  if (is(regimen$weekdays, "NULL")) regimen$weekdays <- "all"
  if (tolower(regimen$weekdays) == "all") regimen$weekdays <- "0,1,2,3,4,5,6"
  if (is(regimen$burst_actuations, "NULL")) regimen$burst_actuations <- NA
  if (is(regimen$burst_window, "NULL")) regimen$burst_window <- NA
  if (is(regimen$actuations_per_dose, "NULL") || is.na(regimen$actuations_per_dose)) regimen$actuations_per_dose <- 1
  if (is(regimen$dose_group_window, "NULL")) regimen$dose_group_window <- NA
  actuations_per_dose <- suppressWarnings(as.numeric(regimen$actuations_per_dose))
  dose_group_window <- suppressWarnings(as.numeric(regimen$dose_group_window))
  if (is.na(actuations_per_dose) || actuations_per_dose < 1) stop("regimen actuations_per_dose must be a number of at least 1")
  if (!is.na(regimen$dose_group_window) && (is.na(dose_group_window) || dose_group_window < 0)) {
    stop("regimen dose_group_window must be NA or a number of seconds of at least 0")
  }
  grouping <- !is.na(dose_group_window)
  burst_actuations <- if (is.na(regimen$burst_actuations)) numeric(0) else suppressWarnings(as.numeric(strsplit(as.character(regimen$burst_actuations), ",")[[1]]))
  burst_window <- if (is.na(regimen$burst_window)) numeric(0) else suppressWarnings(as.numeric(strsplit(as.character(regimen$burst_window), ",")[[1]]))
  if (length(burst_actuations) != length(burst_window) || anyNA(burst_actuations) || anyNA(burst_window)) {
    stop("regimen burst_actuations and burst_window must be comma separated numbers of equal length")
  }

  # TODO: validate weekdays is a string with list of comma separated numbers 0..6


  debug_print("regimen check \u2713")

  #  patinfo --
  if (!is(patinfo, "list")) stop("patinfo type error")
  # if (nrow(patinfo)!=1) stop("expecting patinfo parameter as one line data.frame")

  # check the class to equal the expected instead/in-addition to missing
  # start_date depends on start_time -- if min(timestamp) time is after start_time then take current date
  #                                     otherwise the previous
  if (is(patinfo$day_start_time, "NULL")) patinfo$day_start_time <- "00:00"
  day_start_hour <- as.numeric(strptime(patinfo$day_start_time, format = "%H:%M", tz = "UTC") - strptime("00:00", format = "%H:%M", tz = "UTC"))

  if (is(patinfo$start_date, "NULL")) {
    patinfo$start_date <- as.POSIXct(format(as.POSIXct(min(timestamps)) - day_start_hour * 60 * 60, format = "%Y-%m-%d"), tz = "UTC")
    debug_print(paste("Defaulting patinfo$start_date:", patinfo$start_date))
  }
  if (is(patinfo$end_date, "NULL")) {
    patinfo$end_date <- as.POSIXct(format(as.POSIXct(max(timestamps), tz = "UTC") - day_start_hour * 60 * 60, format = "%Y-%m-%d"), tz = "UTC")
    debug_print(paste("Defaulting patinfo$end_date:", patinfo$end_date))
  }
  if (is(patinfo$start_date, "character")) patinfo$start_date <- as.POSIXct(patinfo$start_date, tz = "UTC")
  if (is(patinfo$end_date, "character")) patinfo$end_date <- as.POSIXct(patinfo$end_date, tz = "UTC")

  # TODO: warn if time exists -- we are stripping

  debug_print("patinfo check \u2713")

  # nonmonit
  # TODO: decide if include nonmonit in patinfo?
  # TODO: validate nonmonit is a data.frame of start/stop dates and POSIXct (convert if Date)
  if (!is.null(nonmonit)) {
    if (!is(nonmonit, "data.frame")) stop("nonmonit type error")
    if (is(nonmonit$start, "character")) nonmonit$start <- as.POSIXct(nonmonit$start, tz = "UTC")
    if (is(nonmonit$end, "character")) nonmonit$end <- as.POSIXct(nonmonit$end, tz = "UTC")
  }

  debug_print("nonmonit check \u2713")

  if (!is.null(manual)) manual <- manual[order(timestamps)]
  timestamps_ordered <- timestamps[order(timestamps)]
  ordering_changes <- any(!(timestamps == timestamps_ordered))
  timestamps <- timestamps_ordered

  debug_print(paste0("timestamp ordering -- changes detected: ", ordering_changes))

  # PROCESSING

  # CREATE SPREADSHEET -- expected days, periods

  periods_per_day <- regimen$periods_per_day
  day_periods_seq <- seq(periods_per_day)
  period_len <- 3600 * 24 / periods_per_day # seconds

  # For the days, any openings up to 23 hours 59 minutes later will go on that day so:
  # If a day starts at 10 pm, 2020-01-01 would include any openings from 2020-01-01 10:00:00 pm to 2020-01-02 9:59:59
  #   2020-01-01 11pm -- open -- does it go towards  2020-01-01
  #   2020-01-01 9pm -- open -- does it go toward 2020-12-31

  day_start_sec <- day_start_hour * 3600

  # create a list of dates between start_date and end_date

  all_dates <- seq(from = patinfo$start_date, to = patinfo$end_date, by = "day")
  # trim time
  all_dates <- as.POSIXct(format(all_dates, format = "%Y-%m-%d"), tz = "UTC")

  # cartesian product of dates and periods
  all_periods <- expand.grid(period = day_periods_seq, day = all_dates)
  all_periods$start <- all_periods$day + day_start_sec + (all_periods$period - 1) * period_len
  all_periods$end <- all_periods$start + period_len - 1

  # add day of week info -- 0(Sun),1(Mon).. 6(Sat)
  all_periods$day_dow <- wday(all_periods$day) - 1

  all_periods$nonmon <- 0 # placeholder

  # determine if active day of week
  active_weekdays_list <- regimen$weekdays
  active_days <- sum(2^as.numeric(unlist(strsplit(active_weekdays_list, ","))))
  all_periods$active_day <- bitwAnd(2^all_periods$day_dow, active_days) != 0

  # number of expected openings -- if active_day then expected doses per day
  all_periods$doserx <- all_periods$active_day * regimen$doses_per_period


  # PROCESS TIMESTAMPS FOR EXCLUSIONS

  # 6. excluded manual entries are set aside before any other rule sees them;
  # exclusions 1-5 are computed on the remaining actuations only
  all_timestamps <- timestamps
  considered <- if (is.null(manual) || include_manual) rep(TRUE, length(timestamps)) else !manual
  timestamps <- timestamps[considered]

  # excluded <- rep(0, length(timestamps))
  # 1. remove the second if too close together
  # excluded <- if_else((timestamps - dplyr::lag(timestamps, default = as.POSIXct("1970-01-01"))) < (regimen$min_wait), 1, 0) # this '-' is using "auto" units -- we need "secs" explicitely
  # with dose grouping min_wait applies between doses instead (below) -- per
  # actuation it would exclude every actuation after the first of a dose
  excluded <- if_else(difftime(timestamps, dplyr::lag(timestamps, default = as.POSIXct("1970-01-01", tz = "UTC")), units = "secs") < (regimen$min_wait), 1, 0)
  if (grouping) excluded <- rep(0, length(timestamps))

  debug_print(paste0("timestamp exclude openings too close to the previous ones (within ", regimen$min_wait, " seconds) -- number of exclusions: ", sum(excluded == 1)))

  # 2. remove before/after if start_date/end_date exist
  # 4. exclude non_monit_dates

  if (!is.null(nonmonit) && !is.null(nonmonit$start) && !is.null(nonmonit$end)) {
    nonmonit_start <- as.POSIXct(format(nonmonit$start, format = "%Y-%m-%d"), tz = "UTC")
    nonmonit_start <- nonmonit_start + seconds(day_start_sec)
    nonmonit_end <- as.POSIXct(format(nonmonit$end, format = "%Y-%m-%d"), tz = "UTC")
    nonmonit_end <- as.POSIXct(nonmonit_end + seconds(day_start_sec + 24 * 3600 - 1), tz = "UTC")
    all_periods$nonmon <- if_else(apply(all_periods, 1, function(x) any(nonmonit_start <= as.POSIXct(x["start"], tz = "UTC") & as.POSIXct(x["end"], tz = "UTC") <= nonmonit_end)), 1, 0)
  } else {
    nonmonit_start <- c()
    nonmonit_end <- c()
    # all_periods$nonmon remains all 0
  }

  # The monitored window is shifted by day_start_time: the first period starts at
  # (start_date + starttime) and the last one ends 24 hours after (end_date + starttime).
  # Both bounds must use the shifted times, otherwise actuations falling between
  # midnight and starttime on the first day are neither excluded nor counted into
  # any period -- they would silently vanish from the adherence denominator.
  start_date_shifted <- patinfo$start_date + lubridate::seconds(day_start_sec)
  end_date_plus_day <- patinfo$end_date + lubridate::seconds(day_start_sec) + lubridate::days(1)
  excluded <- if_else(`&`((excluded == 0), (timestamps < start_date_shifted)), 2, excluded) # before start date
  excluded <- if_else(`&`((excluded == 0), (timestamps >= end_date_plus_day)), 3, excluded) # after end date
  excluded <- if_else(`&`((excluded == 0), (vapply(timestamps, function(x) any(nonmonit_start <= x & x < nonmonit_end), logical(1)))), 4, excluded)

  debug_print(paste("patinfo$end_date", patinfo$end_date))
  debug_print(paste("end_date_plus_day", end_date_plus_day))

  debug_print(paste0("timestamp exclude openings before, after monitored interval(between ", patinfo$start_date, " and ", patinfo$end_date, ") and within non-monitored intervals -- number of exclusions: ", sum(excluded == 2), ",", sum(excluded == 3), ",", sum(excluded == 4)))

  # 5. cap bursts at the prescribed doses per period
  # detection uses all actuations, so it does not depend on min_wait exclusions
  in_burst <- rep(FALSE, length(timestamps))
  t_secs <- as.numeric(timestamps)
  for (r in seq_along(burst_actuations)) {
    # index of the last actuation in [t_i, t_i + window)
    window_last <- findInterval(t_secs + burst_window[r], t_secs, left.open = TRUE)
    for (i in which(window_last - seq_along(t_secs) + 1 >= burst_actuations[r])) {
      in_burst[i:window_last[i]] <- TRUE
    }
  }
  if (any(in_burst)) {
    # periods are contiguous, so every counted actuation falls in one of them
    period_idx <- findInterval(t_secs, as.numeric(all_periods$start))
    for (p in unique(period_idx[excluded == 0 & in_burst])) {
      counted <- which(excluded == 0 & period_idx == p)
      # the cap is the prescribed actuations for the period
      excess <- length(counted) - all_periods$doserx[p] * actuations_per_dose
      if (excess <= 0) next
      burst_counted <- counted[in_burst[counted]]
      excluded[utils::tail(burst_counted, min(excess, length(burst_counted)))] <- 5
    }
  }

  debug_print(paste0("timestamp exclude burst actuations over the prescribed doses per period -- number of exclusions: ", sum(excluded == 5)))

  # 7. group counted actuations into doses: an actuation within dose_group_window
  # seconds of the previous one joins its dose. Without grouping every actuation
  # is a dose of its own.
  dose <- rep(NA_integer_, length(timestamps))
  counted <- which(excluded == 0)
  if (length(counted) > 0) {
    if (grouping) {
      dose[counted] <- cumsum(c(TRUE, diff(t_secs[counted]) > dose_group_window))
      # min_wait between doses: a dose starting within min_wait of the end of the
      # previous dose is excluded whole
      dose_start <- vapply(split(t_secs[counted], dose[counted]), min, numeric(1))
      dose_end <- vapply(split(t_secs[counted], dose[counted]), max, numeric(1))
      too_close <- which(c(FALSE, dose_start[-1] - dose_end[-length(dose_end)] < regimen$min_wait))
      excluded[counted[dose[counted] %in% too_close]] <- 1
      dose[excluded != 0] <- NA
      dose <- match(dose, unique(dose[!is.na(dose)])) # renumber 1..n
    } else {
      dose[counted] <- seq_along(counted)
    }
  }

  debug_print(paste0("timestamp group into doses -- number of doses: ", max(c(0, dose), na.rm = TRUE)))

  excluded_considered <- excluded
  excluded <- rep(6, length(all_timestamps))
  excluded[considered] <- excluded_considered
  dose_considered <- dose
  dose <- rep(NA_integer_, length(all_timestamps))
  dose[considered] <- dose_considered
  timestamps <- all_timestamps

  debug_print(paste0("timestamp exclude manual entries -- number of exclusions: ", sum(excluded == 6)))

  # MERGE SPREADSHEET, TIMESTAMPS and EXCLUSIONS

  # a dose counts in the period its first actuation falls in
  n_periods <- nrow(all_periods)
  doses <- data.frame(start = numeric(0), size = numeric(0))
  if (any(!is.na(dose))) {
    doses <- data.frame(
      start = vapply(split(as.numeric(timestamps[!is.na(dose)]), dose[!is.na(dose)]), min, numeric(1)),
      size = as.numeric(table(dose[!is.na(dose)]))
    )
  }
  dose_period <- findInterval(doses$start, as.numeric(all_periods$start))
  in_period <- dose_period > 0 & doses$start <= as.numeric(all_periods$end)[pmax(dose_period, 1)]
  dose_period <- dose_period[in_period]
  doses <- doses[in_period, ]

  # number of actual openings -- doses, each of one or more actuations
  all_periods$opens <- tabulate(dose_period, n_periods)
  all_periods$fulldoses <- tabulate(dose_period[doses$size >= actuations_per_dose], n_periods)
  all_periods$actuations <- vapply(seq_len(n_periods), function(p) sum(pmin(doses$size[dose_period == p], actuations_per_dose)), numeric(1))
  all_periods$actuationsrx <- all_periods$doserx * actuations_per_dose

  return(list(all_periods = all_periods, timestamps = data.frame(timestamps = timestamps, excluded = excluded, dose = dose)))
}
