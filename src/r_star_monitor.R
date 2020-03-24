library(caret)
library(gbm)
library(tidyverse)

#' Estimate R star
#'
#' Compute estimate of R* using a gradient-boosted model (GBM)
#'
#' @param x a 3D array of samples of dimensions: # iter x # chains x # parameters
#'
#' @param split_chains a Boolean (defaults to true) indicating whether to split
#' chains into two equal halves
#'
#' @param training_percent proportion of iterations used to train GBM model
#' (default is 0.7)
#'
#' @param caret_default GBM hyperparameter settings given as a list (defaults
#' are interaction.depth=3, n.trees=50, shrinkage=0.1, nminobsinmode=10)
#'
#' @return estimated R* value
#' 
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
r_star <- function(x, split_chains=T, training_percent=0.7, caret_default=NA){
  
  if(split_chains)
    x <- split_data(x)
  
  nparams <- dim(x)[3]
  nchains <- dim(x)[2]
  niter <- dim(x)[1]
  m_flattened <- matrix(nrow = nrow(x) * nchains,
                        ncol = (nparams + 1))
  k <- 1
  for(i in 1:nchains){
    for(j in 1:nrow(x)){
      m_flattened[k, 1:nparams] <- x[j, i, ]
      m_flattened[k, (nparams + 1)] <- i
      k <- k + 1
    }
  }
  m_flattened <- m_flattened %>% 
    as.data.frame()
  colnames(m_flattened)[nparams + 1] <- "chain"
  r <- m_flattened %>% 
    mutate(chain=as.factor(chain))
  
  rand_samples <- sample(1:nrow(r), training_percent * nrow(r))
  training_data <- r[rand_samples, ]
  testing_data <- r[-rand_samples, ]
  
  if(is.na(caret_default))
    caretGrid <- expand.grid(interaction.depth=c(3), 
                             n.trees = 50,
                             shrinkage=c(0.1),
                             n.minobsinnode=10)
  else
    caretGrid <- expand.grid(caret_grid_default)
  
  gbmFit <- train(chain ~ ., data = training_data, 
                   method = "gbm",
                   trControl = trainControl(method = 'none'), 
                   tuneGrid = caretGrid, verbose=FALSE)
  plda <- predict(object=gbmFit, newdata=testing_data)
  a_accuracy <- 
    tibble(predicted=plda, actual=testing_data$chain) %>%
    mutate(correct=if_else(predicted==actual, 1, 0)) %>% 
    summarise(mean(correct)) %>% 
    pull()
  return(a_accuracy * n_distinct(training_data$chain))
}

#' Compute estimate of R* distribution using a gradient-boosted model (GBM)
#'
#' @param x a 3D array of samples of dimensions: # iter x # chains x # parameters
#'
#' @param split_chains a Boolean (defaults to true) indicating whether to split
#' chains into two equal halves
#'
#' @param training_percent proportion of iterations used to train GBM model
#' (default is 0.7)
#'
#' @param caret_default GBM hyperparameter settings given as a list (defaults
#' are interaction.depth=3, n.trees=50, shrinkage=0.1, nminobsinmode=10)
#'
#' @param nsim number of samples of R* to generate from GBM fit (defaults to
#' 1000)
#'
#' @return distribution of R* values
#' 
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
r_star_dist <- function(x, split_chains=T, training_percent=0.7, caret_default=NA, nsim=1000){
  
  if(split_chains)
    x <- split_data(x)
  
  nparams <- dim(x)[3]
  nchains <- dim(x)[2]
  niter <- dim(x)[1]
  m_flattened <- matrix(nrow = nrow(x) * nchains,
                        ncol = (nparams + 1))
  k <- 1
  for(i in 1:nchains){
    for(j in 1:nrow(x)){
      m_flattened[k, 1:nparams] <- x[j, i, ]
      m_flattened[k, (nparams + 1)] <- i
      k <- k + 1
    }
  }
  m_flattened <- m_flattened %>% 
    as.data.frame()
  colnames(m_flattened)[nparams + 1] <- "chain"
  r <- m_flattened %>% 
    mutate(chain=as.factor(chain))
  
  rand_samples <- sample(1:nrow(r), training_percent * nrow(r))
  training_data <- r[rand_samples, ]
  testing_data <- r[-rand_samples, ]
  
  if(is.na(caret_default))
    caretGrid <- expand.grid(interaction.depth=c(3), 
                             n.trees = 50,
                             shrinkage=c(0.1),
                             n.minobsinnode=10)
  else
    caretGrid <- expand.grid(caret_grid_default)
  
  gbmFit1 <- train(chain ~ ., data = training_data, 
                   method = "gbm",
                   trControl = trainControl(method = 'none'), 
                   tuneGrid = caretGrid, verbose=FALSE)
  plda <- predict(object=gbmFit1, newdata=testing_data, type = "prob")
  
  mAccuracy <- matrix(nrow = nrow(plda),
                      ncol = nsim)
  for(j in 1:nrow(plda)){
    test <- apply(rmultinom(nsim, 1, prob = plda[j, ]), 2, function(x) which(x==1))
    mAccuracy[j, ] <- if_else(test==testing_data$chain[j], 1, 0)
  }
  return(colMeans(mAccuracy) * n_distinct(testing_data$chain))
}

split_data <- function(x){
  # split Markov chains
  # Args:
  #   sims: a 3D array of samples (# iter * # chains * # parameters)
  #         where # iter should be an even integer
  dims <- dim(x)
  nparams <- dim(x)[3]
  nchains <- dim(x)[2]
  niter <- dim(x)[1]
  
  if (niter == 1L) return(x)
    half <- niter / 2
  if(half%%1!=0){
    print("Warning: dropping first iteration size since # iter
          is not an even integer")
    x <- x[-1, , ]
    niter <- niter - 1
  }
  half <- floor(half)
  full_data <- array(dim=c(half, 2 * nchains, nparams))
  k <- 1
  for(i in 1:nchains){
    first_half <- x[1:half, i, ]
    second_half <- x[(half + 1):niter, i, ]
    full_data[, k, ] <- first_half
    k <- k + 1
    full_data[, k, ] <- second_half
    k <- k + 1
  }
  return(full_data)
}
