% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs.R
\name{county_choropleth_acs}
\alias{county_choropleth_acs}
\title{Create a US County choropleth from ACS data}
\usage{
county_choropleth_acs(
  variable = NULL,
  tableId = NULL,
  column_idx = NULL,
  endyear,
  span = 5,
  title = NULL,
  census_api_key = NULL,
  ...
)
}
\arguments{
\item{variable}{The variable you wish to plot. A list of available census 
variables can be obtained using tidycensus::load_variables()}

\item{tableId}{Alternatively, you may specify the ACS table you wish to plot. 
If the table has more than one variable inside it, you must also specify the 
index of the column you wish to plot.}

\item{column_idx}{The index of the desired column within the table.}

\item{endyear}{The end year of the survey to use.}

\item{span}{Either 1, 3, or 5, the ACS vintage you wish to use.}

\item{title}{A title for the plot; if not specified, a title will be assigned based on the variable.}

\item{census_api_key}{Optional. Census API keys can be obtained at: https://api.census.gov/data/key_signup.html}

\item{...}{Other arguments passed to county_choropleth; see ?county_choropleth()}
}
\value{
A choropleth.
}
\description{
Creates a choropleth of US counties using the US Census' American Community Survey (ACS) data.
}
\examples{
\dontrun{
#  Median household income, zooming in on all counties in New York, New Jersey and Connecticut
county_choropleth_acs(variable = "B19013_001", num_colors=1, 
state_zoom=c("new york", "new jersey", "connecticut"))
}
}
