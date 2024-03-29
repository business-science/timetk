% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-filter_by_time.R
\name{filter_by_time}
\alias{filter_by_time}
\title{Filter (for Time-Series Data)}
\usage{
filter_by_time(.data, .date_var, .start_date = "start", .end_date = "end")
}
\arguments{
\item{.data}{A tibble with a time-based column.}

\item{.date_var}{A column containing date or date-time values to filter.
If missing, attempts to auto-detect date column.}

\item{.start_date}{The starting date for the filter sequence}

\item{.end_date}{The ending date for the filter sequence}
}
\value{
Returns a \code{tibble} or \code{data.frame} that has been filtered.
}
\description{
The easiest way to filter time-based \emph{\strong{start/end ranges}} using shorthand timeseries notation.
See \code{\link[=filter_period]{filter_period()}} for applying filter expression by period (windows).
}
\details{
\strong{Pure Time Series Filtering Flexibilty}

The \code{.start_date}  and \code{.end_date} parameters are designed with flexibility in mind.

Each side of the \code{time_formula} is specified as the character
\code{'YYYY-MM-DD HH:MM:SS'}, but powerful shorthand is available.
Some examples are:
\itemize{
\item \strong{Year:} \verb{.start_date = '2013', .end_date = '2015'}
\item \strong{Month:} \verb{.start_date = '2013-01', .end_date = '2016-06'}
\item \strong{Day:} \verb{.start_date = '2013-01-05', .end_date = '2016-06-04'}
\item \strong{Second:} \verb{.start_date = '2013-01-05 10:22:15', .end_date = '2018-06-03 12:14:22'}
\item \strong{Variations:} \verb{.start_date = '2013', .end_date = '2016-06'}
}

\strong{Key Words: "start" and "end"}

Use the keywords "start" and "end" as shorthand, instead of specifying the
actual start and end values. Here are some examples:
\itemize{
\item \strong{Start of the series to end of 2015:} \verb{.start_date = 'start', .end_date = '2015'}
\item \strong{Start of 2014 to end of series:} \verb{.start_date = '2014', .end_date = 'end'}
}

\strong{Internal Calculations}

All shorthand dates are expanded:
\itemize{
\item The \code{.start_date} is expanded to be the \emph{first date} in that period
\item The \code{.end_date} side is expanded to be the \emph{last date} in that period
}

This means that the following examples are equivalent (assuming your
index is a POSIXct):
\itemize{
\item \code{.start_date = '2015'} is equivalent to \code{.start_date = '2015-01-01 + 00:00:00' }
\item \code{.end_date = '2016'} is equivalent to \verb{2016-12-31 + 23:59:59'}
}
}
\examples{
library(dplyr)

# Filter values in January 1st through end of February, 2013
FANG \%>\%
    group_by(symbol) \%>\%
    filter_by_time(.start_date = "start", .end_date = "2013-02") \%>\%
    plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

}
\references{
\itemize{
\item This function is based on the \code{tibbletime::filter_time()} function developed by Davis Vaughan.
}
}
\seealso{
Time-Based dplyr functions:
\itemize{
\item \code{\link[=summarise_by_time]{summarise_by_time()}} - Easily summarise using a date column.
\item \code{\link[=mutate_by_time]{mutate_by_time()}} - Simplifies applying mutations by time windows.
\item \code{\link[=pad_by_time]{pad_by_time()}} - Insert time series rows with regularly spaced timestamps
\item \code{\link[=filter_by_time]{filter_by_time()}} - Quickly filter using date ranges.
\item \code{\link[=filter_period]{filter_period()}} - Apply filtering expressions inside periods (windows)
\item \code{\link[=slice_period]{slice_period()}} - Apply slice inside periods (windows)
\item \code{\link[=condense_period]{condense_period()}} - Convert to a different periodicity
\item \code{\link[=between_time]{between_time()}} - Range detection for date or date-time sequences.
\item \code{\link[=slidify]{slidify()}} - Turn any function into a sliding (rolling) function
}
}
