library(testthat)
library(metamisc)
library(pROC)
source("get_performance_by_study.R")
source("evaluate_scenario.R")


generate_data <- function(n) {
  
  p_signal <- 10    #number of real predictors
  p_noise <-  10 #number of noise predictors 
  prob_p  <-  0.1 # probability of a predictor to be 1
  base_prev  <-  0.3 # baseline probability of a positive outcome if all risks are 0
  beta_signal <- 1
  
  alldata <-  rbinom(n*(p_signal+p_noise),1,prob_p)
  X <-  matrix(alldata, nrow = n, ncol = p_signal + p_noise)
  W_ <-  c(rep(beta_signal, p_signal), rep(0, p_noise))
  b0 <-  log(base_prev/(1- base_prev))
  lp <-  X %*% W_ + b0 
  y_prob <-  1/(1+exp(-lp))
  
  
  
  #generate outcome from the probabilities calculated based on individual risk 
  y <-  rbinom(n,1,y_prob)
  data <- cbind(y, X) |> data.frame()
  data$study <- 1:10 # 10 studies
  return(data)
}

analysis_model <- function(data) {
  data_without_stdudy <- data[,!(names(data) %in% "study")]
  model <- glm(data = data_without_stdudy, formula = y ~ ., family = "binomial")
}

train_data <- generate_data(n = 1000)
test_data <- generate_data(n = 10000)
model <- analysis_model(train_data)
performance <- get_performance_by_study(model, test_data, "study")


test_that("meta_analyse_metric", {
  meta_analysis <- meta_analyse_metric(performance_data = performance, "cstat")
  expect_equal(class(meta_analysis), c("rma.uni", "rma" ))
})

test_that("meta_analyse_metric", {
  meta_analysis <- meta_analyse_metric(performance_data = performance, "cstat")
  expect_equal(class(meta_analysis), c("rma.uni", "rma" ))
})
