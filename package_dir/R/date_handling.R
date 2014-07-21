
## Get time as year/month.
split_on_decimal <- function(x) strsplit(x=x,split='\\.')

year_decimal_to_decimal <- function(year) {
	split_year <- split_on_decimal(as.character(year))
	split_year <- sapply(split_year, function(x) {
  if (length(x) == 1)
    return(0.0)
  else
    return(as.numeric(paste0('0.',x[2])))
	})
	return(split_year)
}

year_fraction_to_month <- function(x) {
  x <- round(x*12+1)
  return(x)
}


date_to_subyear_interval_factory <- function(map, leap_year_map=map) {

  # Remember these:
  regular_year_map = map; leap_year_map = leap_year_map

  # Function def:
  date_to_subyear_interval <- function(date) {
    if (is.na(date)) return(NA)
    if (!require(lubridate)) stop("Need to install 'lubridate' package.")
    day <- yday(date)
    if (is.na(day)) stop(paste("Date is invalid: ", date, "\n", sep=''))
    if (leap_year(date)) {
      return(leap_year_map[day,'biweek'])
    } else {
      return(regular_year_map[day,'biweek'])
    }
    stop("Something is rotten in Denmark.")
  }

  return(date_to_subyear_interval)
}


dat <- data.frame(biweeks = 1:26, num_days = 14)
dat[26,'num_days'] <- 15
dat[['num_days_leap_year']] <- dat[['num_days']]
dat[5,'num_days_leap_year'] <- 15

no_leap_year <- data.frame(
  julian_day = 1:365,
  biweek = rep(x=dat[['biweeks']], times=dat[['num_days']])
)

leap_year <- data.frame(
  julian_day = 1:366,
  biweek = rep(x=dat[['biweeks']], times=dat[['num_days_leap_year']])
)

date_to_biweek <- date_to_subyear_interval_factory(
  map = no_leap_year, leap_year_map = leap_year)


batchify <- function(date, breaks) {
  if (length(date) == 0) stop("Pass at least one date to 'batchify'.")
  if (length(date) > 1) 
    sapply(date, batchify, breaks=breaks)
  else {
    batch <- which(date > breaks)
    if (length(batch) >= 1)
      return(max(batch))
    else if (identical(batch, vector(mode='integer',length=0)))
      return(0)
    else 
      stop("Something is really rotten in Denmark.")
  }
}





