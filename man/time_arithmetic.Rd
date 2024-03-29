% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-add_time.R
\name{time_arithmetic}
\alias{time_arithmetic}
\alias{add_time}
\alias{subtract_time}
\alias{\%+time\%}
\alias{\%-time\%}
\title{Add / Subtract (For Time Series)}
\usage{
add_time(index, period)

subtract_time(index, period)

index \%+time\% period

index \%-time\% period
}
\arguments{
\item{index}{A date or date-time vector. Can also accept a character representation.}

\item{period}{A period to add. Accepts character strings like "5 seconds", "2 days",
and complex strings like "1 month 4 days 34 minutes".}
}
\value{
A \code{date} or datetime (\code{POSIXct}) vector the same length as \code{index} with the
time values shifted +/- a period.
}
\description{
The easiest way to add / subtract a period to a time series date or date-time vector.
}
\details{
A convenient wrapper for \code{lubridate::period()}. Adds and subtracts a period from a
time-based index. Great for:
\itemize{
\item Finding a timestamp n-periods into the future or past
\item Shifting a time-based index. Note that \code{NA} values may be present where dates don't exist.
}

\strong{Period Specification}

The \code{period} argument accepts complex strings like:
\itemize{
\item "1 month 4 days 43 minutes"
\item "second = 3, minute = 1, hour = 2, day = 13, week = 1"
}
}
\examples{


# ---- LOCATING A DATE N-PERIODS IN FUTURE / PAST ----

# Forward (Plus Time)
"2021" \%+time\% "1 hour 34 seconds"
"2021" \%+time\% "3 months"
"2021" \%+time\% "1 year 3 months 6 days"

# Backward (Minus Time)
"2021" \%-time\% "1 hour 34 seconds"
"2021" \%-time\% "3 months"
"2021" \%-time\% "1 year 3 months 6 days"

# ---- INDEX SHIFTING ----

index_daily <- tk_make_timeseries("2016", "2016-02-01")

# ADD TIME
# - Note `NA` values created where a daily dates aren't possible
#   (e.g. Feb 29 & 30, 2016 doesn't exist).
index_daily \%+time\% "1 month"

# Subtracting Time
index_daily \%-time\% "1 month"




}
\seealso{
Other Time-Based vector functions:
\itemize{
\item \code{\link[=between_time]{between_time()}} - Range detection for date or date-time sequences.
}

Underlying function:
\itemize{
\item \code{\link[lubridate:period]{lubridate::period()}}
}
}
