\name{ds_get}
\alias{ds_get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download data from Domo.
}
\description{
This function uses Domo's public APIs to download a data set from a specific instance's data center.
}
\usage{
ds_get(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ds}{
		The data set id of an existing data set in Domo. Can be found in the URL for the data set in the data center.
	}
	\item{r_friendly_names}{
		Should column names be modified to match R convention.
	}
	\item{...}{
		Additional parameters passed along to the httr::GET call that retrieves data from Domo.
	}
}
\details{
This function provides an easy way to get a data set from Domo into your local R environment. Data set column names in Domo can have spaces and special characters, the r_friendly_names parameter is set to modify column names by default and can be turned off by sending in FALSE. 

The user can pass parameters to the \code{httr::GET} call that retrieves the data from Domo. The most frequent reason this is necessary is to modify column types as the data is being read. Examples will be shown later.
}
\value{
Returns a tibble representation of the data as it exists in Domo. If \code{r_friendly_names} is \code{FALSE}, the tibble will have the column names as seen in Domo. If it is \code{TRUE}, the names will be modified to follow standard R naming conventions.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Domo Inc.
}
\note{
Data sets in Domo can be quite large. If download times seem to be excessive, it may be necessary to aggregate the data in a DataFlow before downloading it in R. You could also use ds_query as a way to summarize the data before it is downloaded locally.
}
\seealso{
	ds_query
}

\examples{
# No example provided at this time.
}
