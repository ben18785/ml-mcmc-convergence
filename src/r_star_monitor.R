library(caret)
library(gbm)
library(tidyverse)
library(posterior)

#' Estimate R star
#'
#' Compute estimate of R* using a ML classifier
#'
#' @param x a 3D array of samples of dimensions: # iter x # chains x # parameters
#'
#' @param split_chains a Boolean (defaults to true) indicating whether to split
#' chains into two equal halves
#'
#' @param training_percent proportion of iterations used to train GBM model
#' (default is 0.7)
#'
#' @param method ML classifer available in Caret R package (defaults to rf model)
#'
#' @param caret_default hyperparameter settings for ML classifier given as a list (defaults
#' to NULL)
#'
#' @param uncertainty whether to provide a list of R* values (if true) or a single value (if false) (defaults to false)
#'
#' @param nsim number of values in list of R* vals
#'
#' @return estimated R* value
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
r_star <- function(x, split_chains=T, training_percent=0.7, caret_default=NULL, method=NULL,
                   uncertainty=F, nsim=1000){

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
  # if only 1 param, add in a column of random noise since gbm requires >1 dims
  if(nparams==1)
    r <- r %>%
      mutate(V_new=rnorm(nrow(r)))

  rand_samples <- sample(1:nrow(r), training_percent * nrow(r))
  training_data <- r[rand_samples, ]
  testing_data <- r[-rand_samples, ]

  if(is.null(method))
    method <- "rf"
  if(is.null(caret_default)&&method=="gbm")
    caretGrid <- tibble(interaction.depth=c(3),
                             n.trees = 50,
                             shrinkage=c(0.1),
                             n.minobsinnode=10)
  else if(is.null(caret_default))
    caretGrid <- tibble(mtry=floor(sqrt(nparams)))
  else
    caretGrid <- expand.grid(caret_default)

  fit <- train(chain ~ ., data = training_data,
                    method = method,
                    trControl = trainControl(method = 'none'),
                    tuneGrid = caretGrid, verbose=FALSE)

  if(uncertainty){
    plda <- predict(object=fit, newdata=testing_data, type = "prob")

    mAccuracy <- matrix(nrow = nrow(plda),
                        ncol = nsim)
    for(j in 1:nrow(plda)){
      test <- apply(rmultinom(nsim, 1, prob = plda[j, ]), 2, function(x) which(x==1))
      mAccuracy[j, ] <- if_else(test==testing_data$chain[j], 1, 0)
    }
    return(colMeans(mAccuracy) * n_distinct(testing_data$chain))
  } else{
    plda <- predict(object=fit, newdata=testing_data)
    a_accuracy <-
      tibble(predicted=plda, actual=testing_data$chain) %>%
      mutate(correct=if_else(predicted==actual, 1, 0)) %>%
      summarise(mean(correct)) %>%
      pull()
    return(a_accuracy * n_distinct(training_data$chain))
  }
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

#' Compute estimate of multivariate Rhat distribution
#'
#' @param x a 3D array of samples of dimensions: # iter x # chains x # parameters
#'
#' @param split_chains a Boolean (defaults to true) indicating whether to split
#' chains into two equal halves
#'
#' @return single Rhat value
#'
#' @references Stephen Brooks, Andrew Gelman (1998), General Methods for Monitoring
#' Convergence of Iterative Simulations
#' Journal of Computational and Graphical Statistics, Volume 7, Number 4, Pages 434455
r_hat_multivariate <- function(x, split_chains=T){

  if(split_chains)
    x <- split_data(x)

  niter <- dim(x)[1]
  nchains <- dim(x)[2]
  nparams <- dim(x)[3]

  chains_l <- seq(1, nchains, 1)
  phi_j_bar <- map(chains_l, ~colMeans(x[, ., ]))
  phi_diff <- map(chains_l, ~x[ , ., ] - phi_j_bar[[.]])
  phi_m <- map(chains_l, ~t(phi_diff[[.]])%*%phi_diff[[.]])
  for(i in chains_l){
    if(i==1)
      phi_tot <- phi_m[[i]]
    else
      phi_tot <- phi_tot + phi_m[[i]]
  }
  W <- (1/(nchains * (niter - 1))) * phi_tot
  phi_bar <- apply(x, c(3), mean)
  phi_diff1 <- map(chains_l, ~phi_j_bar[[.]] - phi_bar)
  temp <- map(chains_l, ~as.matrix(phi_diff1[[.]], nrow=nparams) %*% phi_diff1[[.]])
  for(i in chains_l){
    if(i==1)
      phi_tot1 <- temp[[i]]
    else
      phi_tot1 <- phi_tot1 + temp[[i]]
  }
  B <- niter / (nchains - 1) * phi_tot1
  W_minus_1_B_n <- solve(W) %*% B / niter
  y <- eigen(W_minus_1_B_n)
  R_hat <- (nchains + 1) / nchains * max(abs(y$values)) + (niter - 1) / niter
  return(R_hat)
}

#' Plot R* versus parameter quantiles
#'
#' @param index index of parameter in x
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
#' 20)
#'
#' @return distribution of R* values
#'
#' @references Ben Lambert, Aki Vehtari (2020) R*: A robust MCMC convergence
#' diagnostic with uncertainty using gradient-boosted machines
#' \emph{arXiv preprint} \code{arXiv:TBD}
plot_r_star_quantiles <- function(index, x, split_chains=T, training_percent=0.7, caret_default=NULL, nsim=20){
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

  if(is.null(caret_default))
    caretGrid <- tibble(mtry=floor(sqrt(nparams)))
  else
    caretGrid <- expand.grid(caret_default)

  gbmFit1 <- train(chain ~ ., data = training_data,
                   method = "rf",
                   trControl = trainControl(method = 'none'),
                   tuneGrid = caretGrid, verbose=FALSE)
  plda <- predict(object=gbmFit1, newdata=testing_data, type = "prob")

  mAccuracy <- matrix(nrow = nrow(plda),
                      ncol = nsim)
  for(j in 1:nrow(plda)){
    test <- apply(rmultinom(nsim, 1, prob = plda[j, ]), 2, function(x) which(x==1))
    mAccuracy[j, ] <- if_else(test==testing_data$chain[j], 1, 0)
  }
  mAccuracy <- as.data.frame(mAccuracy)
  mAccuracy$param <- testing_data[, index]

  lbreaks <- quantile(mAccuracy$param, seq(0, 1, 0.1))
  mAccuracy$param_cuts <- cut(mAccuracy$param, breaks=lbreaks, include.lowest = T, right=T, ordered_result = T)
  cuts <- sort(unique(mAccuracy$param_cuts))
  r_star <- matrix(ncol = length(cuts),
                   nrow=nsim)
  for(i in seq_along(cuts)){
    temp <- mAccuracy %>%
      filter(param_cuts==cuts[i])
    r_star[, i] <- colMeans(temp[, 1:nsim]) * n_distinct(testing_data$chain)
  }
  colnames(r_star) <- seq(0.1, 1, 0.1)
  r_mean <-
    r_star %>%
    melt(id.vars=NULL) %>%
    group_by(Var2) %>%
    summarise(value=mean(value))
  r_full <-
    r_star %>%
    melt(id.vars=NULL) %>%
    bind_rows(r_mean) %>%
    mutate(variable=as.numeric(as.character(Var2)))

  ggplot(filter(r_full, !is.na(Var1)),
         aes(x=variable, y=value)) +
    geom_jitter(height = 0.02, width = 0.02, colour="grey") +
    ylab(TeX("$R*$")) +
    xlab("Quantile") +
    geom_hline(yintercept = 1, linetype=2) +
    theme(text = element_text(colour="black", size=11),
          axis.text = element_text(colour="black", size=10)) +
    geom_line(data=filter(r_full, is.na(Var1))) +
    xlim(0, 1.01)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
