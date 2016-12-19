\name{sensitivityAnalysis}
\alias{sensitivityAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute parameter correlations and multicollinearity for the model output}
\description{
This function is a wrapper function for sensFun and Collin from package FME. It calculates both a sensitivity matrix S_ij and multicollinearity index for all parameter combinations. 
}
\usage{
sensitivityAnalysis(Parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Parms}{
Numeric vector or matrix with the parameter coefficients to test
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List containing:
\item{sens}{Matrix containing sensitivity output values for each parameter and each model variable. The sensitivity matrix S_ij contains elements dy_i/dpar_j*parscale_j/varscale_i. 
The scale used to change the value of each parameter can be seen using the summary function on the sens table, and it is set to be the same value of the parameter itself}
\item{coll}{Table with collinearity index for each possible parameter subset. }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
For details in output interpretation see package FME
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{The function needs the original data as a global variable called "data". Please make sure that when using this function to perform identifiability analysis 
on a set of
parameters, that the observation matrix or table used to estimate the parameter set is accessible under the name "data"}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

