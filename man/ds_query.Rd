% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_query.R
\name{ds_query}
\alias{ds_query}
\title{Run a query against a data set in Domo.}
\usage{
ds_query(ds, query, return_data = TRUE)
}
\arguments{
\item{ds}{Data set id from Domo.}

\item{query}{DQL query to process}

\item{return_data}{TRUE if you want a tibble returned, FALSE for the original call}
}
\value{
List object containing data set meta data.
}
\description{
Run a query against a data set in Domo.
}
\examples{
ds_query('32e44f16-c76b-4de3-ac0e-1b7d2ad2f15d','select * from table limit 10')
}
