% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dates.R
\name{tk_get_timeseries_variables}
\alias{tk_get_timeseries_variables}
\title{Get date or datetime variables (column names)}
\usage{
tk_get_timeseries_variables(data)
}
\arguments{
\item{data}{An object of class \code{data.frame}}
}
\value{
\code{tk_get_timeseries_variables} returns a vector containing column names of date-like classes.
}
\description{
Get date or datetime variables (column names)
}
\details{
\code{tk_get_timeseries_variables} returns the column names of date or datetime variables
in a data frame.
Classes that meet criteria for return include those that inherit
\code{POSIXt}, \code{Date}, \code{zoo::yearmon}, \code{zoo::yearqtr}. Function was adapted from \code{padr:::get_date_variables()}.
See \href{https://github.com/EdwinTh/padr/blob/master/R/helpers.R}{padr helpers.R}
}
\examples{
library(dplyr)

FANG \%>\%
    tk_get_timeseries_variables()

}
