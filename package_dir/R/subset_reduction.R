## 1) Subset a set of measurements measured with ?temporal? data points
## 2) Apply a function to the subset
## 3) Return the result


subset_reduction <- function(condition, measurements, f, ...) {
	if (any(is.na(condition))) {
		stop("Condition can not be ambiguous.")
	}
	sub <- measurements[condition]
	if (identical(vector(mode='integer', length=0),sub)) {
		return(NA)
	} else {
		summary <- f(sub, ...)
		return(summary)
	}
}





