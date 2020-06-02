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

The user can pass parameters to the httr::GET call that retrieves the data from Domo. The most frequent reason this is necessary is to modify column types as the data is being read. Examples will be shown later.
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
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
