% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_usa_demographics.R
\name{get_state_demographics}
\alias{get_state_demographics}
\title{Get a handful of demographic variables on US States from the US Census Bureau as a data.frame.}
\usage{
get_state_demographics(endyear = 2013, span = 5)
}
\arguments{
\item{endyear}{The end year for the survey}

\item{span}{The span of the survey}
}
\description{
The data comes from the American Community Survey (ACS). The variables are total population
and median household income.
}
\examples{
\dontrun{
# get some demographic data on US states from the 2010 5-year ACS
df = get_state_demographics(endyear=2010, span=5)

# A choropleth map shows the location of the values.
# Set the 'value' column to be the column we want to render.
df$value = df$median_hh_income
state_choropleth(df)
}
}
