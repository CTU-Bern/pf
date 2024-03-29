% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructParents.R
\name{constructFinanceArticleParents}
\alias{constructFinanceArticleParents}
\title{Finance article hierarchy}
\usage{
constructFinanceArticleParents(financearticle)
}
\arguments{
\item{financearticle}{e.g. \code{all_tabs$financearticle}}
}
\value{
data dataframe of project names and parents
}
\description{
Finance article hierarchy
}
\examples{
all_tabs <- getPFData()
financeParents <- constructFinanceArticleParents(all_tabs$financearticle)
}
