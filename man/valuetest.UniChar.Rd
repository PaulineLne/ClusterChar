% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UniChar.R
\name{valuetest.UniChar}
\alias{valuetest.UniChar}
\title{Test value}
\usage{
\method{valuetest}{UniChar}(obj, y)
}
\arguments{
\item{obj}{of UniChar class}

\item{y}{a vector of groups issued from a clustering process}
}
\value{
test_values
}
\description{
Caracterisation function of groups with test value indicator.
Applied on numerical features of obj.
}
\examples{
#Import data
df<- read_excel('Autos.xlsx')
X<-df[,c('length','width','height','engine-size','compression-ratio','horsepower','city-mpg','highway-mpg')]

#Clustering
clus<- kmeans(X,3)
y <-clus$cluster

#Create object UniChar
obj <- Perform_UniChar(df,y)

#Function value test
vt <- valuetest(obj, y)
print(vt)
}
