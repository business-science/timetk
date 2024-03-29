% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-tk_get_frequency.R
\name{tk_get_frequency}
\alias{tk_get_frequency}
\alias{tk_get_trend}
\title{Automatic frequency and trend calculation from a time series index}
\usage{
tk_get_frequency(idx, period = "auto", message = TRUE)

tk_get_trend(idx, period = "auto", message = TRUE)
}
\arguments{
\item{idx}{A date or datetime index.}

\item{period}{Either "auto", a time-based definition (e.g. "2 weeks"),
or a numeric number of observations per frequency (e.g. 10).}

\item{message}{A boolean. If \code{message = TRUE}, the frequency or trend is output
as a message along with the units in the scale of the data.}
}
\value{
Returns a scalar numeric value indicating the number of observations in the frequency or trend span.
}
\description{
Automatic frequency and trend calculation from a time series index
}
\details{
A \emph{frequency} is loosely defined as the number of observations that comprise a cycle
in a data set. The \emph{trend} is loosely defined as time span that can
be aggregated across to visualize the central tendency of the data.
It's often easiest to think of frequency and trend in terms of the time-based units
that the data is already in. \strong{This is what \code{tk_get_frequency()} and \code{time_trend()}
enable: using time-based periods to define the frequency or trend.}

\strong{Frequency}:

As an example, a weekly cycle is often 5-days (for working
days) or 7-days (for calendar days). Rather than specify a frequency of 5 or 7,
the user can specify \code{period = "1 week"}, and
\code{tk_get_frequency()} will detect the scale of the time series and return 5 or 7
based on the actual data.

The \code{period} argument has three basic options for returning a frequency.
Options include:
\itemize{
\item \code{"auto"}: A target frequency is determined using a pre-defined template (see \code{template} below).
\item \verb{time-based duration}: (e.g. "1 week" or "2 quarters" per cycle)
\item \verb{numeric number of observations}: (e.g. 5 for 5 observations per cycle)
}

When \code{period = "auto"}, the \code{tk_time_scale_template()} is used to calculate the frequency.

\strong{Trend}:

As an example, the trend of daily data is often best aggregated by evaluating
the moving average over a quarter or a month span. Rather than specify the number
of days in a quarter or month, the user can specify "1 quarter" or "1 month",
and the \code{time_trend()} function will return the correct number of observations
per trend cycle. In addition, there is an option, \code{period = "auto"}, to
auto-detect an appropriate trend span depending on the data. The \code{template}
is used to define the appropriate trend span.

\strong{Time Scale Template}

The \code{tk_time_scale_template()} is a Look-Up Table used by the trend and period to find the
appropriate time scale. It contains three features: \code{time_scale}, \code{frequency}, and \code{trend}.

The algorithm will inspect
the scale of the time series and select the best frequency or trend that matches the scale and
number of observations per target frequency. A frequency is then chosen on be the
best match.

The predefined template is stored in a function \code{tk_time_scale_template()}.
You can modify the template with \code{set_tk_time_scale_template()}.
}
\examples{

library(dplyr)

idx_FB <- FANG \%>\%
    filter(symbol == "FB") \%>\%
    pull(date)

# Automated Frequency Calculation
tk_get_frequency(idx_FB, period = "auto")

# Automated Trend Calculation
tk_get_trend(idx_FB, period = "auto")

# Manually Override Trend
tk_get_trend(idx_FB, period = "1 year")

}
\seealso{
\itemize{
\item Time Scale Template Modifiers: \code{\link[=get_tk_time_scale_template]{get_tk_time_scale_template()}}, \code{\link[=set_tk_time_scale_template]{set_tk_time_scale_template()}}
}
}
