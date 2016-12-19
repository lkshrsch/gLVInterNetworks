\name{linear_Regression}
\alias{linear_Regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter estimation of algebraic linear discrete gLV model}
\description{
Given multivariatic time series data, this function fits a linear and discrete generalized Lotka Volterra 
model of the form \delta x_i = \alpha_i + \sum{\beta_{ij} * x_j}
}
\usage{
linear_Regression(data, regularization = FALSE, alpha = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Matrix or table containing time series of measurements in longitudinal form where first column corresponds to the time 
points and subsequent columns correspond to each model variable}
  \item{regularization}{
Boolean flag if regularization of the parameter matrix should be forced}
  \item{alpha}{
Regularization parameter for the elastic net. It ranges from 0 (= Ridge regression) to 1 (= LASSO regression) with values 
in between corresponding to both L1 and L2 penalties weighted by alpha}
}
\details{
Some theory and formulas on elastic net}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
