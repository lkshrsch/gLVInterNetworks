pkgname <- "gLVInterNetworks"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gLVInterNetworks')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("addNoise_res")
### * addNoise_res

flush(stderr()); flush(stdout())

### Name: addNoise_res
### Title: Add stochasticity to in silico generated data
### Aliases: addNoise_res
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (res, sd) 
{
    noise <- matrix(rnorm(0, sd, n = length(res[, -1])), nrow = nrow(res), 
        ncol = ncol(res[, -1]))
    res[, -1] <- res[, -1] + noise
    return(res)
  }



cleanEx()
nameEx("assessment")
### * assessment

flush(stderr()); flush(stdout())

### Name: assessment
### Title: Assess results from inference algorithm
### Aliases: assessment
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (result, Parms) 
{
    "still need to check how many zeros are in Parms and correct for qualitative..."
    quantitative <- correctly_intervalized_estimates(c(result$Parms), 
        c(result$SE), c(Parms))
    quantitative <- sum(quantitative)/length(quantitative)
    qualitative <- compare_matrix(c(result$Parms), c(Parms))
    qualitative <- sum(qualitative)/length(qualitative)
    is_not_significant <- correctly_intervalized_estimates(c(result$Parms), 
        c(result$SE), rep(0, length(Parms)))
    is_zero <- c(Parms) == 0
    qualitative2 <- is_not_significant == is_zero
    qualitative2 <- sum(qualitative2)/length(qualitative2)
    result$quantitative = quantitative
    result$qualitative1 = qualitative
    result$qualitative2 = qualitative2
    return(result)
  }



cleanEx()
nameEx("compare_matrix")
### * compare_matrix

flush(stderr()); flush(stdout())

### Name: compare_matrix
### Title: Compare two interaction matrices
### Aliases: compare_matrix
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, b) 
{
    av = c(a)
    bv = c(b)
    for (i in 1:length(av)) {
        av[i] = interaction_compare(av[i], bv[i])
    }
    return(av)
  }



cleanEx()
nameEx("correctly_intervalized_estimates")
### * correctly_intervalized_estimates

flush(stderr()); flush(stdout())

### Name: correctly_intervalized_estimates
### Title: Check if confidence intervals of parameter estimates contain
###   true coefficients
### Aliases: correctly_intervalized_estimates
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (background, SE, Parms) 
{
    CI95 <- SE * 2
    up_estimates <- background + CI95
    down_estimates <- background - CI95
    return(down_estimates < c(Parms) & c(Parms) < up_estimates)
  }



cleanEx()
nameEx("discrete")
### * discrete

flush(stderr()); flush(stdout())

### Name: discrete
### Title: Function to discretize in silico generated data
### Aliases: discrete
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (observations, n) 
{
    "make realistic observations by discretization of simulated data."
    result <- observations[seq(1, nrow(observations), by = n), 
        ]
    return(result)
  }



cleanEx()
nameEx("draw_interaction_network")
### * draw_interaction_network

flush(stderr()); flush(stdout())

### Name: draw_interaction_network
### Title: Add stochasticity to in silico generated data
### Aliases: draw_interaction_network
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(network, main=NULL){
  
  igraph::plot.igraph(network,layout = igraph::layout.fruchterman.reingold , 
vertex.label.color="black",edge.color="black", edge.curved=TRUE, main=main)
  
}



cleanEx()
nameEx("gLVInterNetworks-package")
### * gLVInterNetworks-package

flush(stderr()); flush(stdout())

### Name: gLVInterNetworks-package
### Title: Inference of interaction networks based on generalised Lotka
###   Volterra dynamics
### Aliases: gLVInterNetworks-package gLVInterNetworks
### Keywords: package

### ** Examples

library(gLVInterNetworks)
data <- gLVgenerateData(species = 2, number_of_interactions = 2, timepoints = 100, noise = 0.01, testData = 20)
## Not run: plot(data, type = "l")
lr <- gLVlinearRegression(data, regularization = TRUE, alpha = 0)
## Not run: summary(lr)
## Not run: plot(lr, type = "l")
## Not run: points(data)
nlr <- gLVnonlinearRegression(data, parms0 = lr$Parms)
## Not run: summary(nlr)
## Not run: plot(nlr, type = "l")
## Not run: points(data)
## Not run: par(mfrow = c(1,2))
## Not run: plotGraph(data, vsize = 0.2, main = "Original interaction network", verbose = TRUE )
## Not run: plotGraph(nlr, vsize = 0.2, main = "Inferred interaction network", verbose = TRUE)
ident <- sensitivityAnalysis(nlr$Parms) 
## Print summary of sensitivity matrix
summary(ident$sens)
## Print collinearity index for all parameters together
ident$coll[ident$coll[,"N"]==length(data$Parms),]



cleanEx()
nameEx("gLVgenerateData")
### * gLVgenerateData

flush(stderr()); flush(stdout())

### Name: gLVgenerateData
### Title: Generate random data for gLV fitting
### Aliases: gLVgenerateData
### Keywords: ~kwd1 ~kwd2

### ** Examples

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



cleanEx()
nameEx("gLVnonlinearRegression")
### * gLVnonlinearRegression

flush(stderr()); flush(stdout())

### Name: gLVnonlinearRegression
### Title: Parameter estimation through gradient search of continuous
###   nonlinear gLV model
### Aliases: gLVnonlinearRegression
### Keywords: ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



cleanEx()
nameEx("generate_interaction_network")
### * generate_interaction_network

flush(stderr()); flush(stdout())

### Name: generate_interaction_network
### Title: Add stochasticity to in silico generated data
### Aliases: generate_interaction_network
### Keywords: internal

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
## The function is currently defined as



cleanEx()
nameEx("inSilico_bio")
### * inSilico_bio

flush(stderr()); flush(stdout())

### Name: inSilico_bio
### Title: Generate in silico data
### Aliases: inSilico_bio
### Keywords: internal ~kwd2

### ** Examples

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
            growth[i] <- -1 * (interactions[i, ] %*% var_means)
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



cleanEx()
nameEx("interaction_compare")
### * interaction_compare

flush(stderr()); flush(stdout())

### Name: interaction_compare
### Title: Compare interaction parameters
### Aliases: interaction_compare
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, b) 
{
    if (a > 0 && b > 0) {
        return(TRUE)
    }
    else if (a == 0 && b == 0) {
        return(TRUE)
    }
    else if (a < 0 && b < 0) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
  }



cleanEx()
nameEx("log_slope_luk")
### * log_slope_luk

flush(stderr()); flush(stdout())

### Name: log_slope_luk
### Title: Estimates the slope of subsequential measurements
### Aliases: log_slope_luk
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, b) 
{
    return(tryCatch(log(abs(b)) - log(abs(a)), error = function(e) {
        print(paste0("Tried slope with: ", a, " ", b))
        return("log slope error")
    }))
  }



cleanEx()
nameEx("modfit_pars_estimate_MLE_sequential")
### * modfit_pars_estimate_MLE_sequential

flush(stderr()); flush(stdout())

### Name: modfit_pars_estimate_MLE_sequential
### Title: Parameter estimation in sequential manner using sensitivity
###   analysis
### Aliases: modfit_pars_estimate_MLE_sequential
### Keywords: internal

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
## The function is currently defined as



cleanEx()
nameEx("obs2data_bio")
### * obs2data_bio

flush(stderr()); flush(stdout())

### Name: obs2data_bio
### Title: Correctly format observations into input data for the model
### Aliases: obs2data_bio
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (res) 
{
    obs = t(res[, -1])
    X = rbind(1, obs)
    X = X[, 1:ncol(X) - 1]
    Y <- slope(res[, -1])
    Y = t(Y)
    Y[is.nan(Y)] <- 0
    Y[is.infinite(Y)] <- 0
    return(list(Y, X))
  }



cleanEx()
nameEx("parameter_matrix")
### * parameter_matrix

flush(stderr()); flush(stdout())

### Name: parameter_matrix
### Title: Correctly format a parameter vector into a matrix as input for
###   the model
### Aliases: parameter_matrix
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pars, numberSpecies) 
{
    "set parameters as matrix format as required by solveLV function"
    par_matrix = matrix(data = pars, nrow = numberSpecies)
    return(par_matrix)
  }



cleanEx()
nameEx("plot.Inference_run")
### * plot.Inference_run

flush(stderr()); flush(stdout())

### Name: plot.Inference_run
### Title: Plot function for objects returned by the inference function
### Aliases: plot.Inference_run
### Keywords: internal

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    matplot(solveLV_bio(Parms = x$inference$Parms, times = x$data$obs[, 
        1], States = x$data$obs[1, -1])[, -1], type = "l")
  }



cleanEx()
nameEx("plot.Sim_data")
### * plot.Sim_data

flush(stderr()); flush(stdout())

### Name: plot.Sim_data
### Title: Plot function for objects returned by in silico data generation
###   function
### Aliases: plot.Sim_data

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    matplot(x$obs[, -1])
  }



cleanEx()
nameEx("plotGraph")
### * plotGraph

flush(stderr()); flush(stdout())

### Name: plotGraph
### Title: Plot interaction network
### Aliases: plotGraph
### Keywords: ~kwd2

### ** Examples

data <- gLVgenerateData(2,2,100,0.1, 20)
#plotGraph(data)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



cleanEx()
nameEx("points.Sim_data")
### * points.Sim_data

flush(stderr()); flush(stdout())

### Name: points.Sim_data
### Title: Add points corresponding to the original observations on top of
###   the model solutions
### Aliases: points.Sim_data
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, index=NULL , ...){

  if(!is.null(index)){
    points(x = x$obs[,1], y= x$obs[,index+1], col=index)
  }else{
  for(i in 1:ncol(x$obs[,-1])){
    points(x = x$obs[,1], y= x$obs[,i+1], col=i)
  }
  }
}


cleanEx()
nameEx("prediction")
### * prediction

flush(stderr()); flush(stdout())

### Name: prediction
### Title: Predictions for parameterized models on new variable states
### Aliases: prediction
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (result, data) 
{
    x0 <- data$testData[1, -1]
    pred <- solveLV_bio(result$Parms, data$testData[1:nrow(data$testData), 
        1], x0)
    Coef_of_determination <- R_squared(pred, data$testData)
    result$R_squared_test_Data <- Coef_of_determination
    r <- c()
    for (i in 1:10) {
        x0 <- runif(n = data$species, 0, 1)
        new <- solveLV_bio(data$Parms, times = 0:50, States = x0)
        pred2 <- solveLV_bio(result$Parms, new[, 1], x0)
        r[i] <- R_squared(pred2, new)
    }
    result$R_squared_new_Data <- mean(r)
    return(result)
  }



cleanEx()
nameEx("print.summary.Inference_run")
### * print.summary.Inference_run

flush(stderr()); flush(stdout())

### Name: print.summary.Inference_run
### Title: Output summary description from an inference run
### Aliases: print.summary.Inference_run
### Keywords: internal

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, ...){
  
  printCoefmat(x = TAB, P.values = TRUE, has.Pvalue = TRUE)
  
}




cleanEx()
nameEx("random_interaction_matrix_bio")
### * random_interaction_matrix_bio

flush(stderr()); flush(stdout())

### Name: random_interaction_matrix_bio
### Title: Generate a random parameter matrix
### Aliases: random_interaction_matrix_bio
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (species, number_of_interactions, times = 1:100, 
    plot = FALSE) 
{
    "Generate random parameters for simulation studies on multispecies gLV"
    "For lotka volterra function without substrate, so diagonal entries less negative??"
    n = number_of_interactions
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



cleanEx()
nameEx("regularized_linear_regression")
### * regularized_linear_regression

flush(stderr()); flush(stdout())

### Name: regularized_linear_regression
### Title: Compute regularized parameter estimates for the linear model
### Aliases: regularized_linear_regression
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(X,Y, alpha, cvfolds){
  
  
  #require(glmnet)
  
  sol = matrix(0, nrow = nrow(Y), ncol = nrow(X))
  
  # X <- X[-1,]  # Get rid of intercept , it is automatically placed by glmnet
  
  
  mse = 0
  
  
  
  for(i in 1:nrow(Y)){
    
    
    cv <- glmnet::cv.glmnet(t(X)[,-1],Y[i,], alpha=alpha ,nfolds = cvfolds,standardize=T)
    
    small.lambda.index <- which(cv$lambda == cv$lambda.1se)
    small.lambda.betas <- cv$glmnet.fit$beta[,small.lambda.index]
    small.lambda.betas <- c(cv$glmnet.fit$a0[small.lambda.index],small.lambda.betas)
    sol[i,] = small.lambda.betas
    mse[i] = cv$cvm[small.lambda.index]
    #print(cv$lambda.1se)
  }
  
  mse = mean(mse)

  return(list(sol,mse))
  
}



cleanEx()
nameEx("slope")
### * slope

flush(stderr()); flush(stdout())

### Name: slope
### Title: Calculate slope estimate for time series observations
### Aliases: slope
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (abundances) 
{
    "Get log slopes of measured time points. For discrete LV linear algebraic system"
    "Input: observations without time points == Abundances"
    slopes = matrix(0, nrow = nrow(abundances) - 1, ncol = ncol(abundances))
    for (column in 1:ncol(abundances)) {
        for (row in 1:(nrow(abundances) - 1)) {
            try(slopes[row, column] <- log_slope_luk(abundances[row, 
                column], abundances[row + 1, column]))
        }
    }
    return(slopes)
  }



cleanEx()
nameEx("solveLV_Sens_bio")
### * solveLV_Sens_bio

flush(stderr()); flush(stdout())

### Name: solveLV_Sens_bio
### Title: Wrapper for numerical computation of solutions of the gLV
###   equations for sensitivity analysis
### Aliases: solveLV_Sens_bio
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as



cleanEx()
nameEx("solveLV_bio")
### * solveLV_bio

flush(stderr()); flush(stdout())

### Name: solveLV_bio
### Title: Wrapper for numerical computation of solutions of the gLV model
###   without substrate addition
### Aliases: solveLV_bio
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



cleanEx()
nameEx("summary.Inference_run")
### * summary.Inference_run

flush(stderr()); flush(stdout())

### Name: summary.Inference_run
### Title: Summary
### Aliases: summary.Inference_run
### Keywords: internal ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

summary.Inference_run <- 
function(object, ...){
  tval <- c(run$Parms/run$SE)
  TAB <- cbind(Estimate = c(run$Parms), StdErr = c(run$SE), t.value = tval, p.value = 2*pt(-abs(tval), df = run$df))
  class(TAB) <- 'summary.Inference_run'
  return(TAB)
}




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
