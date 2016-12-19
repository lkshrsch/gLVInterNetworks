\name{gLVInterNetworks-package}
\alias{gLVInterNetworks-package}
\alias{gLVInterNetworks}
\docType{package}
\title{
\packageTitle{gLVInterNetworks}
}
\description{
\packageDescription{gLVInterNetworks}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{gLVInterNetworks}
\packageIndices{gLVInterNetworks}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{gLVInterNetworks}
Maintainer: \packageMaintainer{gLVInterNetworks}
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
}
\examples{
library(gLVInterNetworks)
data <- gLVgenerateData(species = 2, number_of_non_diagonal_coefficients = 2, timepoints = 100, noise = 0.01, testData = 20)
\dontrun{plot(data, type = "l")}
lr <- gLVlinearRegression(data, regularization = TRUE, alpha = 0)
\dontrun{summary(lr)}
\dontrun{plot(lr, type = "l")}
\dontrun{points(data)}
nlr <- gLVnonlinearRegression(data, parms0 = lr$Parms)
\dontrun{summary(nlr)}
\dontrun{plot(nlr, type = "l")}
\dontrun{points(data)}
\dontrun{par(mfrow = c(1,2))}
\dontrun{plotGraph(data, vsize = 0.2, main = "Original interaction network", verbose = TRUE )}
\dontrun{plotGraph(nlr, vsize = 0.2, main = "Inferred interaction network", verbose = TRUE)}
ident <- sensitivityAnalysis(nlr$Parms) 
## Print summary of sensitivity matrix
summary(ident$sens)
## Print collinearity index for all parameters together
ident$coll[ident$coll[,"N"]==length(data$Parms),]
}
