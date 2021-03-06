% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UniChar.R
\name{vtest.UniChar}
\alias{vtest.UniChar}
\title{v.test}
\usage{
vtest.UniChar(obj, y, label)
}
\arguments{
\item{obj}{of UniChar class}

\item{y}{a data frame representing groups from a clustering process on obj$data}

\item{label}{a specific label variable}
}
\value{
\item{result}{ a dataframe with test values}
}
\description{
calculate test values according to the labeled data and the group y
}
\examples{
autos = readxl::read_excel("Autos.xlsx")
tmp = sep_data(autos)
clus = kmeans(autos[tmp[[2]]], 4)
y <- clus$cluster
obj <- Perform_UniChar(autos,y)
vtest.UniChar(obj, y, 'make')
}
