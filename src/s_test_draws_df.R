rm(list=ls())
library(tidyverse)
library(reshape2)
library(posteriordb)
library(posterior)
source("r_star_monitor.R")
source("monitornew.R")

pd <- pdb_default() # Posterior database connection
pn <- posterior_names(pd)
po <- pdb_posterior("eight_schools-eight_schools_noncentered", pdb = pd)
rpd <- reference_posterior_draws(po)

temp_df <- as_draws_df(rpd)
temp_df$.chain

split_chains <- function(x) {
  x <- as.matrix(x)
  niter <- NROW(x)
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

split_chains(temp_df$`theta[1]`)

# splits chains in `draws_df` object
split_chains_draws_df <- function(x) {
  nchain <- nchains(x)
  k <- 1
  for(i in 1:nchain) {
    tmp <- x$.chain[x$.chain == i]
    niter <- length(tmp)
    if(niter > 1) {
      half <- niter / 2
      tmp[1:floor(half)] <- k
      k <- k + 1
      tmp[ceiling(half + 1):niter] <- k
      k <- k + 1
    }
    if(i == 1)
      chains <- tmp
    else
      chains <- c(chains, tmp)
  }
  x$.chain <- chains
  return(x)
}

test <- split_chains_draws_df(x)

#' Estimate R star
#'
#' Compute estimate of R* using a ML classifier
#'
#' @param x a draws_df object from posterior package
#'
#' @param split_chains a Boolean (defaults to true) indicating whether to split
#' chains into two equal halves
#'
#' @param uncertainty whether to provide a list of R* values (if true) or a single value (if false) (defaults to false)
#'
#' @param method ML classifer available in Caret R package (defaults to rf model)
#'
#' @param hyperparameters hyperparameter settings for ML classifier given as a list (defaults
#' to NULL)
#'
#' @param nsim number of values in list of R* vals
#'
#' @param training_percent proportion of iterations used to train GBM model
#' (default is 0.7)
#'
#' @return estimated R* value
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
r_star_draws_df <- function(x, split_chains=T, uncertainty=F, method=NULL, hyperparameters=NULL,
                            training_percent=0.7, nsim=1000, ...){
  x <- as_draws_df(x)
  if(split_chains)
    x <- split_chains_draws_df(x)

  # if only 1 param, add in a column of random noise for classifiers
  nparams <- nvariables(x)
  if(nparams==1)
    x$V_new <- rnorm(nrow(x))

  # create training / testing sets
  x$.chain <- as.factor(x$.chain)
  rand_samples <- sample(1:nrow(x), training_percent * nrow(x))
  training_data <- x[rand_samples, ]
  testing_data <- x[-rand_samples, ]

  # choose hyperparameters
  if(is.null(method)) {
    method <- "rf"
    caret_grid <- tibble(mtry=floor(sqrt(nparams)))
  } else if(is.null(hyperparameters) && method=="gbm") {
    caret_grid <- tibble(interaction.depth=c(3),
                         n.trees = 50,
                         shrinkage=c(0.1),
                         n.minobsinnode=10)
  } else {
    caret_grid <- hyperparameters
  }

  # remove iteration / draws columns and fit classifier
  training_data <- training_data[, c(variables(training_data), ".chain")]
  fit <- train(.chain ~ .,
               data = training_data,
               method = method,
               trControl = trainControl(method = 'none'),
               tuneGrid = caret_grid,
               ...)

  # calculate classification accuracy then R*
  nchains <- length(unique(testing_data$.chain))
  if(uncertainty){
    probs <- predict(object=fit, newdata=testing_data, type = "prob")
    m_accuracy <- matrix(nrow = nrow(probs),
                         ncol = nsim)
    for(j in 1:nrow(probs)){
      vals <- rmultinom(nsim, 1, prob = probs[j, ])
      test <- apply(vals, 2, function(x) which(x==1))
      m_accuracy[j, ] <- if_else(test == testing_data$.chain[j], 1, 0)
    }
    return(colMeans(m_accuracy) * nchains)
  } else{
    plda <- predict(object=fit, newdata=testing_data)
    res <- tibble(predicted=plda, actual=testing_data$.chain)
    accuracy <- mean(res$predicted == res$actual)
    return(accuracy * nchains)
  }
}

start <- Sys.time()
vals1 <- r_star_draws_df(temp_df, uncertainty=T, method="gbm")
end <- Sys.time()
start-end
start <- Sys.time()
vals2 <- r_star_draws_df(temp_df, uncertainty=T, method="rf")
end <- Sys.time()
start-end
start <- Sys.time()
vals3 <- r_star_draws_df(temp_df, uncertainty=T, method="knn",
                         hyperparameters = tibble(k=5))
end <- Sys.time()
start-end
r_star(as_draws_array(temp_df))


# build simple example
x <- example_draws("eight_schools")
r_star_draws_df(x)
r_star_draws_df(x, split_chains = F)
r_star_draws_df(x, method = "gbm", verbose=F)

hist(r_star_draws_df(x, uncertainty = T))
hist(r_star_draws_df(x, uncertainty = T, nsim = 100))


