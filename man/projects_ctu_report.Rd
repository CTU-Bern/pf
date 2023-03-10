% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_ctu_report.R
\name{projects_ctu_report}
\alias{projects_ctu_report}
\title{function to get the projects in a specific year}
\usage{
projects_ctu_report(all_tabs, selected_year)
}
\arguments{
\item{all_tabs}{result from e.g. getPFData}

\item{selected_year}{year for which projects should be summarized}
}
\value{
projects_year
}
\description{
This code has been adapted for the purpose of producing the graphs for the CTU Yearly report
}
\examples{
all_tabs <-  getPFData()
x <- projects_ctu_report(all_tabs,selected_year)
}
