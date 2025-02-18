#' @title Report standarized output of the adherence processing
#' @param all_periods output of pre_adherence processing
#' @param timestamps timestamps dataframe from pre_adherence to calculate times and diffs in the report
#' @param med Medication name
#' @param patinfo Patient info - patient specific information
#' @param adhstart Report adherence start date
#' @param adhend Report adherence end date
#' @importFrom readr read_csv cols
#' @importFrom methods is
#' @importFrom dplyr between select mutate if_else summarise %>%
#' @importFrom hms as_hms
#' @import zoo
#' @export
report_adherence <- function(all_periods, timestamps, med, patinfo=list(), adhstart = NULL, adhend = NULL) {
  # TODO: if incoming dates in Date -- convert to POSIXct

  if (is(patinfo$start_date, "character")) patinfo$start_date <- as.POSIXct(patinfo$start_date,tz="UTC")
  if (is(patinfo$end_date, "character")) patinfo$end_date <- as.POSIXct(patinfo$end_date,tz="UTC")
  # if (is(patinfo$start_date, "NULL"))  patinfo$start_date <- as.POSIXct(format(as.POSIXct(min(timestamps))-24*60*60,format = "%Y-%m-%d"))
  # if (is(patinfo$end_date, "NULL"))  patinfo$end_date <- as.POSIXct(format(as.POSIXct(max(timestamps))+24*60*60,format = "%Y-%m-%d"))

  # calculate adherence between adhstart .. adhend
  if (is.null(adhstart)) adhstart <- patinfo$start_date
  if (is.null(adhend)) adhend <- patinfo$end_date

  if (is.null(adhstart)) adhstart <- as.Date(min(timestamps$timestamps),tz="UTC")
  if (is.null(adhend)) adhend <- as.Date(max(timestamps$timestamps),tz="UTC")

  if (is(adhstart, "character")) adhstart <- as.POSIXct(adhstart,tz="UTC")
  if (is(adhend, "character")) adhend <- as.POSIXct(adhend,tz="UTC")
  if (is(adhstart, "Date")) adhstart <- as.POSIXct(as.character(adhstart), format = "%Y-%m-%d",tz="UTC")
  if (is(adhend, "Date")) adhend <- as.POSIXct(as.character(adhend), format = "%Y-%m-%d",tz="UTC")
  adhend_date_plus_day <- adhend + lubridate::days(1)

  all_periods$med <- med
  all_periods$studyid <- patinfo$studyid

  if (!("day" %in% colnames(all_periods))) stop("all_periods missing day")

  all_periods$studyday <- as.numeric(1 + round((all_periods$day - min(all_periods$day)) / 86400))
  all_periods$mtime <- format(all_periods$start, format = "%H:%M")

  # diff for non excluded timestamps
  timestamps_diff <- timestamps[timestamps$excluded == 0, ] - dplyr::lag(timestamps[timestamps$excluded == 0, ])
  colnames(timestamps_diff) <- c("diff", "ex")
  timestamps <- merge(timestamps, timestamps_diff, by = "row.names", all = TRUE)

  # Row.names        timestamps.x excluded.x     timestamps.y excluded.y
  # drop rownames
  rownames(timestamps) <- timestamps$Row.names
  timestamps <- timestamps[, -which(names(timestamps) %in% c("Row.names", "ex"))]
  timestamps <- timestamps[order(timestamps$timestamps), ]
  timestamps$diff <- sub(".000000", "", as_hms(timestamps$diff))

  excluded <- timestamps$excluded

  all_periods$times <- apply(
    all_periods, 1,
    function(x) {
      times <- substring(timestamps[dplyr::between(
        if_else(excluded == 0, timestamps$timestamps, as.POSIXct("1970-01-01",tz="UTC")),
        as.POSIXct(x["start"],tz="UTC"),
        as.POSIXct(x["end"],tz="UTC")
      ), ]$timestamps, 12)
      mtime1 <- substring(as.character(all_periods[1,]$start),12)
      paste0(ifelse(times<mtime1,paste0(times,"*"),times), collapse = ",")
    }
  )

  all_periods$diffs <- apply(
    all_periods, 1,
    function(x) {
      paste0(sub(" ", "", timestamps[dplyr::between(
        if_else(excluded == 0, timestamps$timestamps, as.POSIXct("1970-01-01",tz="UTC")),
        as.POSIXct(x["start"],tz="UTC"),
        as.POSIXct(x["end"],tz="UTC")
      ), ]$diff), collapse = ",")
    }
  )

  if (is(all_periods$doserx, "NULL")) all_periods$doserx <- 1

  all_periods$dosecorr <- if_else("&"(all_periods$opens == all_periods$doserx, all_periods$nonmon == 0), "1",
    if_else(all_periods$nonmon == 0, "0", " ")
  )

  all_periods$dosetkn <- if_else("&"(all_periods$opens >= all_periods$doserx, all_periods$nonmon == 0), "1",
    if_else(all_periods$nonmon == 0, "0", " ")
  )

  # adhweek” = (sum(“opens” over past 7 days)/sum(“doserx” over past 7 days))
  # all_periods$adhweek <-
  adhweek <- all_periods %>%
    mutate(adhweek_numerator = rollapply(opens, 7, sum, align = "right", fill = NA)) %>%
    mutate(adhweek_denominator = rollapply(doserx, 7, sum, align = "right", fill = NA)) %>%
    mutate(adhweek = round(100 * if_else(adhweek_denominator == 0, 0, adhweek_numerator / adhweek_denominator))) %>%
    mutate(adhweek = if_else(is.na(adhweek), "", as.character(adhweek))) %>%
    select(adhweek)

  all_periods <- cbind(all_periods, adhweek)

  # if “opens” > “doserx” AND “nonmon” = 0
  all_periods$overuse <- if_else("&"(all_periods$opens > all_periods$doserx, all_periods$nonmon == 0), "1",
    if_else(all_periods$nonmon == 0, "0", " ")
  )

  # if any of the “diff” variables < or > X AND “nonmon” = 0
  # all_periods$timing <-

  adh_df <-
    all_periods %>%
    dplyr::filter(day >= adhstart) %>%
    dplyr::filter(day < adhend_date_plus_day) %>%
    dplyr::filter(nonmon == 0) %>%
    dplyr::summarise(
      daysadh = sum(as.numeric(dosecorr)) / dplyr::n(),
      daysdose = sum(as.numeric(dosetkn)) / dplyr::n(),
      perdoses = sum(as.numeric(opens)) / sum(as.numeric(doserx)),
      n = dplyr::n()
    )
  # daysadh <- sum("dosecorr\" WHEN \"nonmon\" = 0)" / sum(nonmon = 0)
  # daysdose <- sum("dosetkn\" WHEN \"nonmon\" = 0)" / sum(nonmon = 0)
  # perdoses <- sum("opens"\" WHEN \"nonmon\" = 0)" / sum("doserx"\"" WHEN "nonmon\""=0

  return(list(report = all_periods, adh = adh_df, adhstart=adhstart, adhend_date_plus_day=adhend_date_plus_day ))
}
