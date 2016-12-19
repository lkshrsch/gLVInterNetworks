\name{random_interaction_matrix_bio}
\alias{random_interaction_matrix_bio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a random parameter matrix}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
random_interaction_matrix_bio(species, number_of_non_diagonal_coefficients, times = 1:100, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
%%     ~~Describe \code{species} here~~
}
  \item{number_of_non_diagonal_coefficients}{
%%     ~~Describe \code{number_of_non_diagonal_coefficients} here~~
}
  \item{times}{
%%     ~~Describe \code{times} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
function (species, number_of_non_diagonal_coefficients, times = 1:100, 
    plot = FALSE) 
{
    "Generate random parameters for simulation studies on multispecies gLV"
    "For lotka volterra function without substrate, so diagonal entries less negative??"
    n = number_of_non_diagonal_coefficients
    if (n > (species^2 - species)) {
        print(paste0("Error. For ", species, " species, the number of interactions can<b4>t be greater than ", 
            species^2 - species))
        return()
    }
    interaction <- diag(sample(x = runif(n = species, min = -2, 
        max = -1)), nrow = species)
    if (abs(n) > 0) {
        coordinates <- which(interaction == 0, arr.ind = T)
        rows = sample(x = nrow(coordinates), size = n, replace = FALSE)
        for (i in 1:length(rows)) {
            interaction[coordinates[rows[i], 1], coordinates[rows[i], 
                2]] <- rnorm(1, 0, 4)
        }
    }
    colnames(interaction) = NULL
    return((interaction))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
