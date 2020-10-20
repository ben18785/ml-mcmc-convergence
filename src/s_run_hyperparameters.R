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
    laccuracy[i] <- r_star(a_array, method="gbm",
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
                  refresh = 0, iter=2000)
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


# command line arguments ------
args <- commandArgs(trailingOnly=TRUE)
model_num <- as.numeric(args[1])
num_iter <- as.numeric(args[2])

if(model_num==1){
  m_parameters <- expand_grid(int.depth=seq(1, 7, 2),
                            n.trees=c(1, 10, 50, 100))
  f_data_gen <- f_data_ar1
  name <- "ar1"
}else if(model_num==2){
  m_parameters <- expand_grid(int.depth=c(1, 3, 5),
                              n.trees=c(1, 50, 200))
  f_data_gen <- f_data_8_schools
  name <- "8_schools"
}else if(model_num==3){
  m_parameters <- expand_grid(int.depth=c(3, 7, 11, 13),
                              n.trees=c(50, 100, 400, 1000))
  f_data_gen <- f_data_cauchy
  name <- "cauchy"
}else if(model_num==4){
  m_parameters <- expand_grid(int.depth=c(3, 7, 11),
                              n.trees=c(50, 100, 400))
  f_data_gen <- f_data_normal
  name <- "normal_250"
}

r_star_vals <- f_replicate_r_star(num_iter, m_parameters, f_data_gen)
saveRDS(list(r_star=r_star_vals, hyperparameters=m_parameters), paste0("../data/hypers_", name, ".rds"))