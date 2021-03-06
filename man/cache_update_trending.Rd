% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_helpers.R
\name{cache_update_trending}
\alias{cache_update_trending}
\title{Update data for trending shows on trakt.tv}
\usage{
cache_update_trending(n = 100)
}
\arguments{
\item{n}{\verb{[100]} Number of shows to get, passed to \code{tRakt::shows_trending()} as \code{limit}.}
}
\value{
Nothing
}
\description{
Update data for trending shows on trakt.tv
}
\examples{
cache_update_trending(20)
}
