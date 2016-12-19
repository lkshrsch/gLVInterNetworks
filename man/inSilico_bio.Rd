\name{inSilico_bio}
\alias{inSilico_bio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate in silico data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inSilico_bio(species, number_of_interactions, events, mode = 1, times = 1:100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
%%     ~~Describe \code{species} here~~
}
  \item{number_of_interactions}{
%%     ~~Describe \code{number_of_interactions} here~~
}
  \item{events}{
%%     ~~Describe \code{events} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{times}{
%%     ~~Describe \code{times} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (species, number_of_interactions, events, 
    mode = 1, times = 1:100) 
{
    "requires: random_interaction_matrix_bio, solveLV"
    "mode:0 = linear ODE, 1 = normal, 2 = mixed feeding, 3 = events"
    n = number_of_interactions
    if (n > (species^2 - species)) {
        print(paste0("Error. For ", species, " species, the number of interactions can<b4>t be greater than ", 
            species^2 - species))
        return()
    }
    res <- matrix(0, nrow = 2)
    while (class(res)[1] != "deSolve") {
        interactions <- random_interaction_matrix_bio(species, 
            number_of_interactions)
        growth <- rep(0, nrow(interactions))
        var_means <- runif(n = species, 0.1, 0.8)
        for (i in 1:nrow(interactions)) {
            growth[i] <- -1 * (interactions[i, ] \%*\% var_means)
        }
        Parms <- cbind(growth, interactions)
        options(warn = 2)
        x0 <- var_means + rnorm(n = species, mean = var_means, 
            sd = 0.01)
        if (mode == 3) {
            print("Events")
            names(x0) <- as.character(1:length(x0))
            res <- tryCatch(solveLV_event(events = events, times = times, 
                Parms, States = x0, c = 2 * c, lambda = lambda, 
                cycle = cycle), error = function(e) return("error"))
            s <- S(t = times, c = 2 * c, lambda = lambda, cycle)
        }
        if (mode == 1) {
            res <- tryCatch(solveLV_bio(times = times, Parms, 
                States = x0), warning = function(w) return("error"), 
                error = function(e) return("error"))
        }
        if (mode == 0) {
            print("linear ODE")
            res <- tryCatch(solveLV_bio_linear(Parms, States = x0, 
                times = times))
        }
    }
    options(warn = 1)
    return(list(res, Parms))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
