\name{gLVgenerateData}
\alias{gLVgenerateData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate random data for gLV fitting
}
\description{
Generates random data simulating time series of cell abundances governed by Lotka Volterra dynamics
}
\usage{
gLVgenerateData(species, number_of_interactions, timepoints, noise, testData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
Integer describing the number of independent cellular subcommunities 
}
  \item{number_of_interactions}{
Integer describing the number of non-zero interactions present in the simulated system. These are assigned randomly between the nodes or subcommunities
}
  \item{timepoints}{
Numeric vector containing the timepoints for which to compute the solutions of the model
}
  \item{noise}{
The standard deviation of the normally distributed stochastic factor added to the solution of the model at each time step
}
  \item{testData}{
Number of observations used as test dataset for validation on untrained data, taken from the last measurements of the time series. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a matrix. The first column displays the time points, and the remaining columns correspond each to a independent variable in the system.
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
Lukas Hirsch
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (species, number_of_interactions, timepoints, 
    noise) 
{
    "requires inSilico_bio, discrete, addNoise_res"
    raw_data <- inSilico_bio(species, number_of_interactions)
    Parms <- raw_data[[2]]
    res <- raw_data[[1]]
    threshold = 3
    if (any(which(round(diff(abs(rowMeans(res[, -1])), 2), threshold) == 
        0))) {
        end <- min(which(round(diff(abs(rowMeans(res[, -1])), 
            2), threshold) == 0))
    }
    else {
        end <- nrow(res)
    }
    res <- solveLV_bio(Parms, seq(0, end, by = 0.01), res[1, 
        -1])
    discretization <- nrow(res)/timepoints
    res <- discrete(res, discretization)
    test_data <- res[(nrow(res) - 19):nrow(res), ]
    obs <- res[1:(nrow(res) - 20), ]
    obs <- addNoise_res(obs, noise)
    timepoints <- nrow(obs)
    dimensions <- ncol(obs[, -1])
    k <- sum(any(Parms == 0))/length(Parms)
    data <- list(species = species, timepoints = timepoints, 
        Parms = Parms, noise = noise, sparsity = k, obs = obs, 
        testData = test_data)
    class(data) <- "Sim_data"
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
