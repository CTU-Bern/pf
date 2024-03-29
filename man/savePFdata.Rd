% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/savePFdata.R
\name{savePFdata}
\alias{savePFdata}
\title{save projectfacts data to R drive}
\usage{
savePFdata(all_tabs, dir = "R:/Projectfacts/ODBC", dated = FALSE)
}
\arguments{
\item{all_tabs}{data from e.g. ODBC}

\item{dir}{directory in which to save the data}

\item{dated}{logical - whether to include the date in the filename}
}
\value{
data
}
\description{
save projectfacts data to R drive
}
\examples{
# load the data from R
# all_tabs <- getPFData()
## download data via ODBC
# all_tabs <- getPFData(NULL)
# all_tabs$customer <- decodeCustomFields(all_tabs$customer, all_tabs$customfields)
# all_tabs$project <- decodeCustomFields(all_tabs$project, all_tabs$customfields)
# all_tabs$ticket <- decodeCustomFields(all_tabs$ticket, all_tabs$customfields)
# all_tabs$worker <- decodeCustomFields(all_tabs$worker, all_tabs$customfields)
## save the data to R
# savePFdata(all_tabs)
}
