age <- function(date, birthday, breaks=NULL) {
	if (date < birthday) return(-1)
	if (is.null(breaks)) {
    if (year(date) == year(birthday)) return(0)
    breaks <- birthday + years(1:(year(date) - year(birthday)))
  }
  relevant <- (breaks > birthday) & (breaks <= date)
  number_of_breaks <- sum(relevant)
  return(number_of_breaks)
}




