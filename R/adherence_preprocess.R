#' @title Pre-process time data for adherence
#' @param timestamps Input timestamps - vector of timestamps
#' @param regimen Regimen - regimen definition
#' @param patinfo Patient info - patient specific information
#' @param nonmonit Non-monitored date intervals
#' @importFrom readr read_csv cols
#' @importFrom methods is
#' @importFrom dplyr between if_else select mutate
#' @import lubridate
#' @export
adherence_preprocess <- function(timestamps, regimen, patinfo=list(), nonmonit=data.frame()) {

  # Examples:
  #     timestamps <- input$data$timestamp
  #     regimen <- regimens[1,]
  #     patinfo <- list(start_date='2020-03-21',end_date='2020-07-27')
  #     nonmonit <- data.frame(start=c("2018-12-10","2018-12-20"),end=c("2018-12-11","2018-12-21"))

  # validate inputs

  # timestamps -- list of timestamps of openings
  if (!is(timestamps, "POSIXct")) stop("timestamps type error")
  if (length(timestamps) == 0) stop("empty timestamps list")

  debug_print("timestamps check \u2713")

  #  regimen --
  if (!is(regimen, "data.frame")) stop("regimen type error")
  if (nrow(regimen) != 1) stop("expecting regimen parameter as one line data.frame")
  if (is(regimen$periods_per_day, "NULL")) regimen$periods_per_day <- 1
  if (is(regimen$doses_per_period, "NULL")) regimen$doses_per_period <- 1
  if (is(regimen$min_wait, "NULL")) regimen$min_wait <- 600 # seconds
  if (is(regimen$weekdays, "NULL")) regimen$weekdays <- "all"
  if (tolower(regimen$weekdays) == "all") regimen$weekdays <- "0,1,2,3,4,5,6"

  # TODO: validate weekdays is a string with list of comma separated numbers 0..6


  debug_print("regimen check \u2713")

  #  patinfo --
  if (!is(patinfo, "list")) stop("patinfo type error")
  # if (nrow(patinfo)!=1) stop("expecting patinfo parameter as one line data.frame")

  # check the class to equal the expected instead/in-addition to missing
  # start_date depends on start_time -- if min(timestamp) time is after start_time then take current date
  #                                     otherwise the previous
  if (is(patinfo$day_start_time, "NULL")) patinfo$day_start_time <- "00:00"
  day_start_hour <- as.numeric(strptime(patinfo$day_start_time, format = "%H:%M", tz="UTC") - strptime("00:00", format = "%H:%M", tz="UTC"))

  if (is(patinfo$start_date, "NULL")) {
    # patinfo$start_date <- as.POSIXct(format(as.POSIXct(min(timestamps)) - 24 * 60 * 60, format = "%Y-%m-%d"))
    patinfo$start_date <- as.POSIXct(format(as.POSIXct(min(timestamps)) - day_start_hour * 60 * 60, format = "%Y-%m-%d"),tz="UTC")
    # patinfo$start_date <- as.POSIXct(format(as.POSIXct(min(timestamps)) , format = "%Y-%m-%d"))
    print(paste("Defaulting patinfo$start_date:",patinfo$start_date))
  }
  if (is(patinfo$end_date, "NULL")) {
    # patinfo$end_date <- as.POSIXct(format(as.POSIXct(max(timestamps)) + 24 * 60 * 60, format = "%Y-%m-%d"))
    patinfo$end_date <- as.POSIXct(format(as.POSIXct(max(timestamps)) + day_start_hour * 60 * 60, format = "%Y-%m-%d"))
    # patinfo$end_date <- as.POSIXct(format(as.POSIXct(max(timestamps)) , format = "%Y-%m-%d"))
    print(paste("Defaulting patinfo$end_date:",patinfo$end_date))
  }
  if (is(patinfo$start_date, "character")) patinfo$start_date <- as.POSIXct(patinfo$start_date,tz="UTC")
  if (is(patinfo$end_date, "character")) patinfo$end_date <- as.POSIXct(patinfo$end_date,tz="UTC")

  # TODO: warn if time exists -- we are stripping

  debug_print("patinfo check \u2713")

  # nonmonit
  # TODO: decide if include nonmonit in patinfo?
  # TODO: validate nonmonit is a data.frame of start/stop dates and POSIXct (convert if Date)
  if (!is.null(nonmonit)) {
    if (!is(nonmonit, "data.frame")) stop("nonmonit type error")
    if (is(nonmonit$start, "character")) nonmonit$start <- as.POSIXct(nonmonit$start,tz="UTC")
    if (is(nonmonit$end, "character")) nonmonit$end <- as.POSIXct(nonmonit$end,tz="UTC")
  }

  debug_print("nonmonit check \u2713")

  # TODO: Timezones

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
  #   2020-01-01 11pm -- open -- does it go towards 2020-01-02 or 2020-01-01? - 2020-01-01
  #   2020-01-01 9pm -- open -- does it go toward 2020-01-01 or 2020-12-31? 12-31-2020

  day_start_sec <- day_start_hour * 3600

  # create a list of dates between start_date and end_date

  # add 2 hours to account for DST changes -- to make sure we indeed capture only the dates between start_date and end_date (including)
  all_dates <- seq(from = patinfo$start_date + 7200, to = patinfo$end_date + 7200, by = "day")
  # trim time
  all_dates <- as.POSIXct(format(all_dates, format = "%Y-%m-%d"),tz="UTC")

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

  # excluded <- rep(0, length(timestamps))
  # 1. remove the second if too close together
  # excluded <- if_else((timestamps - dplyr::lag(timestamps, default = as.POSIXct("1970-01-01"))) < (regimen$min_wait), 1, 0) # this '-' is using "auto" units -- we need "secs" explicitely
  excluded <- if_else(difftime(timestamps, dplyr::lag(timestamps, default = as.POSIXct("1970-01-01",tz="UTC")),units = "secs") < (regimen$min_wait), 1, 0)

  debug_print(paste0("timestamp exclude openings too close to the previous ones (within ", regimen$min_wait, " seconds) -- number of exclusions: ", sum(excluded == 1)))

  # 2. remove before/after if start_date/end_date exist
  # 4. exclude non_monit_dates

  if (!is.null(nonmonit) && !is.null(nonmonit$start) && !is.null(nonmonit$end)) {
    nonmonit_start <- as.POSIXct(format(nonmonit$start, format = "%Y-%m-%d"),tz="UTC")
    nonmonit_start <- nonmonit_start + seconds(day_start_sec)
    nonmonit_end <- as.POSIXct(format(nonmonit$end, format = "%Y-%m-%d"),tz="UTC")
    nonmonit_end <- as.POSIXct(nonmonit_end + seconds(day_start_sec + 24 * 3600 - 1), tz="UTC")
    all_periods$nonmon <- if_else(apply(all_periods, 1, function(x) any(nonmonit_start <= as.POSIXct(x["start"],tz="UTC") & as.POSIXct(x["end"],tz="UTC") <= nonmonit_end)), 1, 0)
  } else {
    nonmonit_start <- c()
    nonmonit_end <- c()
    # all_periods$nonmon remains all 0
  }

  end_date_plus_day <- patinfo$end_date + lubridate::days(1)
  excluded <- if_else(`&`((excluded == 0), (timestamps < patinfo$start_date)), 2, excluded)
  excluded <- if_else(`&`((excluded == 0), (timestamps >= end_date_plus_day)), 3, excluded)
  excluded <- if_else(`&`((excluded == 0), (sapply(timestamps, function(x) any(nonmonit_start <= x & x < nonmonit_end)))), 4, excluded)

  debug_print(paste0("timestamp exclude openings before, after monitored interval(between ", patinfo$start_date, " and ", patinfo$end_date, ") and within non-monitored intervals -- number of exclusions: ", sum(excluded == 2), ",", sum(excluded == 3), ",", sum(excluded == 4)))

  # MERGE SPREADSHEET, TIMESTAMPS and EXCLUSIONS

  # number of actual openings
  select_timestamps <- if_else(excluded == 0, timestamps, as.POSIXct("1970-01-01",tz="UTC"))
  all_periods$opens <- apply(all_periods, 1, function(x) sum(between(select_timestamps, as.POSIXct(x["start"],tz="UTC"), as.POSIXct(x["end"],tz="UTC"))))

  return(list(all_periods = all_periods, timestamps = data.frame(timestamps = timestamps, excluded = excluded)))
}
