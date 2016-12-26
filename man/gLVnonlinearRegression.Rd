\name{gLVnonlinearRegression}
\alias{gLVnonlinearRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter estimation through gradient search of continuous nonlinear gLV model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gLVnonlinearRegression(data, parms0 = NULL, ftol = 1e-8 , ptol = 1e-8, maxiter = 100, lowerbound = rep(-20,ncol(data$obs)*(ncol(data$obs)-1)), upperbound = rep(20,ncol(data$obs)*(ncol(data$obs)-1)), method = "Marq")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data input containing a time series of observations in longitudinal matrix form}
  \item{parms0}{
Optional. Starting parameter vector. Default = Zero vector}
  \item{ftol}{
Minimum change in the value of the objective function (sum of squared residuals) 
between two consecutive steps before 
stopping iterative optimization}
  \item{ptol}{
Minimum change in the value of parameters being estimated between two consecutive 
steps before stopping iterative optimization}
 \item{maxiter}{
Maximal number of iterations allowed before breaking the optimization algorithm}
 \item{lowerbound}{
Numerical vector of equal length as parameter vector describing lower bound for constrained parameter search
}
 \item{upperbound}{
Numerical vector of equal length as parameter vector describing upper bound for constrained parameter search
}
 \item{method}{
Method used for optimization of the objective function. Default is "Marq" for Leverberg-Marquandt
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Parms}{Estimated parameter matrix }
\item{SSR}{The value of the sum of squared residuals of the found solution}
\item{residual_SD}{The estimated standard deviation of the data stochasticity 
in respect to the solution found}
\item{SE}{A matrix contatining the standard errors of each estimated 
parameter}
\item{residuals_t.test}{The result of a t test with the null hypothesis of the 
residuals from the model solution being normally distributed with standard error 
'residual_SD'}
\item{message}{A text message related to the reason why the optimization algorithm 
stopped}
\item{obs}{A matrix with the calculated abundances on each timestep according to the 
estimated parameters}
\item{Fit}{The object returned by the optimization algorithm. It is a list 
containing further details of the optimization algorithm run}
\item{df}{The degrees of freedom}
\item{quantitative}{Only when validating against in Silico data: The ratio of 
estimated parameters that contain the original value within the 95\% confidence 
interval given by their standard error (displayed in 'SE')}
\item{qualitative1}{Only when validating against in Silico data: The ratio of 
correctly retrieved edges from the original interaction network}
\item{qualititative2}{Only when validating against in Silico data: The ratio of 
correctly retrieved edges from the original interaction network after setting 
estimated parameters that are not significantly different from zero to zero (i.e. 
parameters that contain the zero in their 95\% confidence interval as given by 
their standard error)}
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
