% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tract.R
\name{get_census_tract_id}
\alias{get_census_tract_id}
\title{Get census tract identifier}
\usage{
get_census_tract_id(x, year = as.character(2010:2023), quiet = FALSE)
}
\arguments{
\item{x}{a vector of s2 cell identifers (\code{s2_cell} object)}

\item{year}{a character data year passed to tigris to get state and tract boundaries}

\item{quiet}{silence progress messages?}
}
\description{
The identifier of the closest census tract geography (retrieved from the US Census API)
for the supplied year are returned for each s2 geohash.
}
\details{
\code{tigris::tracts()} powers this, so set \code{options(tigris_use_cache = TRUE)}
to benefit from its caching.
According to \url{https://github.com/walkerke/tigris}, available years for tracts
and states are 1990, 2000, 2010 - 2022
}
\examples{
get_census_tract_id(s2::as_s2_cell(c("8841b399ced97c47", "8841b38578834123")), year = "2020")
}
\references{
\url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch10GARM.pdf}
}
