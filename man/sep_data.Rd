% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UniChar.R
\name{sep_data}
\alias{sep_data}
\title{sep_data}
\usage{
sep_data(df)
}
\arguments{
\item{df}{a data frame}
}
\value{
\item{list_categ_num}{  a list of 2 vectors. Labels of categorical and numerical variables}
}
\description{
Function to split categorical and numerical variable labels.
Used to instance categorical and numerical data in the UniChar class.
}
\examples{
df <- read_excel('Autos.xlsx')
sep_data(df)
}
