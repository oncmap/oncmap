## code to prepare `regimens` dataset goes here

regimens <- rbind(
  "daily1" = data.frame(
    name = "Once Daily",
    periods_per_day = 1,
    doses_per_period = 1,
    min_wait = 600,
    days_per_week = 7,
    weekdays = "all"
  ),
  "daily12hr" = data.frame(
    name = "12hr daily",
    periods_per_day = 2,
    doses_per_period = 1,
    min_wait = 600,
    days_per_week = 7,
    weekdays = "all"
  ),
  "daily2" = data.frame(
    name = "2 doses daily",
    periods_per_day = 1,
    doses_per_period = 2,
    min_wait = 600,
    days_per_week = 7,
    weekdays = "all"
  ),
  "3daweekmtuw" = data.frame(
    name = "Three days a week (M,Tu,W)",
    periods_per_day = 1,
    doses_per_period = 2,
    min_wait = 600,
    days_per_week = 3,
    weekdays = "1,2,3"
  ),
  "3daweekmwf" = data.frame(
    name = "Three days a week (M,W,F)",
    periods_per_day = 1,
    doses_per_period = 2,
    min_wait = 600,
    days_per_week = 3,
    weekdays = "1,3,5"
  )
)

usethis::use_data(regimens, overwrite = TRUE)
