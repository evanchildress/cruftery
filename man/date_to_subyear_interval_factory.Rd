\name{date_to_subyear_interval_factory}
\alias{date_to_subyear_interval_factory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
date_to_subyear_interval_factory(map, leap_year_map = map)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map}{
%%     ~~Describe \code{map} here~~
}
  \item{leap_year_map}{
%%     ~~Describe \code{leap_year_map} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (map, leap_year_map = map) 
{
    regular_year_map = map
    leap_year_map = leap_year_map
    date_to_subyear_interval <- function(date) {
        if (is.na(date)) 
            return(NA)
        if (!require(lubridate)) 
            stop("Need to install 'lubridate' package.")
        day <- yday(date)
        if (is.na(day)) 
            stop(paste("Date is invalid: ", date, "\n", sep = ""))
        if (leap_year(date)) {
            return(leap_year_map[day, "biweek"])
        }
        else {
            return(regular_year_map[day, "biweek"])
        }
        stop("Something is rotten in Denmark.")
    }
    return(date_to_subyear_interval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
