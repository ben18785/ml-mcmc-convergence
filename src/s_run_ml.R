setwd("/Volumes/Samsung1.5TB/Github/ml-mcmc-convergence/src")
rm(list=ls())
library(tidyverse)
library(reshape2)
library(rstan)
library(latex2exp)
library(caret)
library(gbm)
options(mc.cores=4)
rstan_options(auto_write = TRUE)
source("monitornew.R")
source("r_star_monitor.R")
source("eight_schools.data.R")
eight_schools <- list(J=J, y=y, sigma=sigma)
model_cp <- stan_model("eight_schools_cp.stan")
model_ncp <- stan_model("eight_schools_ncp.stan")


# helper functions ------
f_ar1 <- function(rho, sigma, L){
  x <- vector(length = L)
  x[1] <- rnorm(1, 0, sd=sigma)
  for(i in 2:L)
    x[i] = rho * x[i - 1] + rnorm(1, 0, sd=sigma)
  return(x)
}

f_generate_lower_var_four <- function(var_ratio, rho, sigma, L){
  x <- matrix(nrow = L, ncol = 4)
  for(i in 1:3)
    x[, i] <- f_ar1(rho, sigma, L)
  z <- f_ar1(rho, sigma * sqrt(var_ratio), L)
  x[, 4] <- z
  return(x)
}

f_data_ar1 <- function(){
  temp <- f_generate_lower_var_four(1/3, 0.3, 1, 1000)
  a_array <- array(dim=c(1000, 4, 1))
  a_array[,,1] <- temp
  return(a_array)
}

f_replicate_hyper <- function(m_parameters, f_data_generator){
  
  a_array <- f_data_generator()
  
  laccuracy <- vector(length = nrow(m_parameters))
  for(i in 1:nrow(m_parameters))
    laccuracy[i] <- r_star(a_array,
                           caret_default=expand.grid(interaction.depth=m_parameters$int.depth[i],
                                                     n.trees = m_parameters$n.trees[i],
                                                     shrinkage=0.1,
                                                     n.minobsinnode=10))
  return(laccuracy)
}


f_replicate_r_star <- function(nreplicates, m_parameters, f_data_generator){
  r_star_vals <- matrix(nrow = nreplicates, ncol=nrow(m_parameters))
  for(i in 1:nreplicates){
    print(i)
    
    temp <- f_replicate_hyper(m_parameters, f_data_generator)
    r_star_vals[i, ] <- temp
  }
  r_star_vals <- r_star_vals %>% 
    t() %>% 
    as.data.frame()
  r_star_vals1 <- m_parameters %>% cbind(r_star_vals)
  return(r_star_vals1)
}

f_data_8_schools <- function(){
  fit_ncp <- sampling(
    model_ncp, data = eight_schools,
    iter = 2000, chains = 4, refresh = 0,
    control = list(adapt_delta = 0.95)
  )
  x_ncp <- rstan::extract(fit_ncp, permuted=F)
}

f_data_cauchy <- function(){
  fit_nom <- stan(file = 'cauchy_alt_1.stan',
                  refresh = 0)
  x <- rstan::extract(fit_nom, permuted=F)
  return(x)
}

f_data_normal <- function(){
  N <- 250
  A <- rWishart(1, 250, diag(N))[,,1]
  model <- stan_model("mvt_250_ncp.stan")
  fit <- sampling(model, data=list(N=N, A=A), iter=1000, chains=4)
  x <- rstan::extract(fit, permuted=F)
  return(x)
}

f_data_normal_10000 <- function(){
  fit <- readRDS("../output/mvt_wide_1000.rds")
  x <- rstan::extract(fit, permuted=F)
  return(x)
}

r_star_ml_variety <- function(x, method_, caretGrid){
  
  split_chains=T
  training_percent=0.7
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
  
  if(method_!="multinom")
    gbmFit <- train(chain ~ ., data = training_data, 
                    trControl = trainControl(method = 'none'),
                    method=method_,
                    tuneGrid = caretGrid)
  else
    gbmFit <- train(chain ~ ., data = training_data, 
                    trControl = trainControl(method = 'none'),
                    method=method_,
                    tuneGrid = caretGrid,
                    MaxNWts=10000
    )
  plda <- predict(object=gbmFit, newdata=testing_data)
  a_accuracy <- 
    tibble(predicted=plda, actual=testing_data$chain) %>%
    mutate(correct=if_else(predicted==actual, 1, 0)) %>% 
    summarise(mean(correct)) %>% 
    pull()
  return(a_accuracy * n_distinct(training_data$chain))
}

f_replicate_gridded <- function(x, method_, caretGrids_){
  r_stars <- vector(length = nrow(caretGrids_))
  times <- vector(length = nrow(caretGrids_))
  for(i in 1:nrow(caretGrids_)){
    start.time <- Sys.time()
    r_stars[i] <- r_star_ml_variety(x, method_, caretGrids_[i, ])
    end.time <- Sys.time()
    times[i] <- end.time - start.time
  }
  return(list(r_star=r_stars, time=times))
}

f_run_all <- function(methods, list_of_caret_grids, f_data_generator){
  x <- f_data_generator()
  r_stars <- vector(length = length(methods))
  times <- vector(length = length(methods))
  hypers <- vector(length = length(methods), mode="list")
  for(i in seq_along(r_stars)){
    print(methods[i])
    a_grid <- list_of_caret_grids[[i]]
    a <- f_replicate_gridded(x, methods[i], a_grid)
    r_stars[i] <- max(a$r_star)
    a_ind <- which.max(a$r_star)
    times[i] <- a$time[a_ind]
    hypers[[i]] <- a_grid[a_ind, ]
  }
  return(tibble(r_star=r_stars, method=methods, time=times, hyper=hypers))
}

f_run_all_replicates <- function(niterations, methods, list_of_caret_grids, f_data_generator){
  for(i in 1:niterations){
    print(i)
    temp_df <- f_run_all(methods, list_of_caret_grids, f_data_generator) %>% mutate(iter=i)
    if(i==1)
      big_df <- temp_df
    else
      big_df <- big_df %>% bind_rows(temp_df)
  }
  return(big_df)
}

tunegrid_gbm <- expand.grid(interaction.depth=c(3, 7), 
                            n.trees = c(50, 100),
                            shrinkage=0.1,
                            n.minobsinnode=10)
tunegrid_rf <- tibble(mtry = 1:2)
tunegrid_knn <- tibble(k = c(5, 10, 15, 20, 40))
tunegrid_svm <- tibble(C = c(0.25, 0.5, 0.75))
tunegrid_multinom <- tibble(decay=c(0.1, 0.2, 0.5, 1))
tunegrid_xgbtree <- expand_grid(nrounds = c(1, 10, 100),
                                max_depth = c(1, 4),
                                eta = c(.1, .4),
                                gamma = 0,
                                colsample_bytree = .7,
                                min_child_weight = 1,
                                subsample = c(.8, 1))

# command line arguments ------
args <- commandArgs(trailingOnly=TRUE)
model_num <- as.numeric(args[1])
num_iter <- as.numeric(args[2])

if(model_num==1){
  f_data_gen <- f_data_ar1
  name <- "ar1"
}else if(model_num==2){
  f_data_gen <- f_data_8_schools
  name <- "8_schools"
}else if(model_num==3){
  f_data_gen <- f_data_cauchy
  name <- "cauchy"
} else if(model_num==4){
  f_data_gen <- f_data_normal
  name <- "normal_250"
} else if(model_num==5){
  f_data_gen <- f_data_normal_10000
  name <- "normal_10000"
}

temp_df <- f_run_all_replicates(num_iter, c("gbm", "xgbTree", "rf", "knn", "svmLinear", "multinom"),
                                list(tunegrid_gbm, tunegrid_xgbtree, tunegrid_rf, tunegrid_knn, tunegrid_svm, tunegrid_multinom),
                                f_data_gen)
saveRDS(temp_df, paste0("../data/ml_comp_", name, ".rds"))
