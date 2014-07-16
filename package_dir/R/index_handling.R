


first_index <- function(dates) {
  if (!is.null(dates) && !all(is.na(dates))) {
    first_index <- which(dates == min(dates, na.rm=TRUE))
    return(first_index[1])
  } else {
    return(1)
  }
}

last_index <- function(dates) {
  if (!is.null(dates) && !all(is.na(dates))) {
    last_index <- which(dates == max(dates, na.rm=TRUE))
    return(last_index[1])
  } else {
    return(1)
  }
}



