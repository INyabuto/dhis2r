% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_endpoint.R
\name{api_query}
\alias{api_query}
\title{Parse DHIS2 endpoint query}
\usage{
api_query(...)
}
\arguments{
\item{...}{DHIS2 query parameters.}
}
\value{
A character string.
}
\description{
This function parses DHIS2 endpoint queries into a character string.
}
\examples{
api_query(fields = "name", totalPages = TRUE)
api_query(fields = api_fields("name", "id", organisationUnit = c("name")))
}
