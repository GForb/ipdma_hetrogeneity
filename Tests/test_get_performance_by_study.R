library(testthat)
library(metamisc)
library(pROC)
source("get_performance_by_study.R")

y <- rbinom(100, 1, 0.5)
y_hat <- runif(100) 

performance <- calc_performance(y, y_hat)

generate_data <- function(n, beta_signal) {
  p_signal <- 10    #number of real predictors
  p_noise <-  10 #number of noise predictors 
  prob_p  <-  0.1 # probability of a predictor to be 1
  base_prev  <-  0.3 # baseline probability of a positive outcome if all risks are 0
  
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

train_data <- generate_data(1000, 0.7)
data_without_stdudy <- train_data[,!(names(train_data) %in% "study")]
model <- glm(data = data_without_stdudy, formula = y ~ ., family = "binomial")
test_data <- generate_data(10000, 0.7)

test_that("get_performance", {
  performance <- get_performance(model = model, test_data = test_data)
  expect_equal(ncol(performance), 3)
  expect_equal(nrow(performance), 3)
  
})


test_that("get_performance_for_a_study", {
  study1_performance <- get_performance_for_a_study(model = model, 
                                                    test_data = test_data,
                                                    study_var =  "study",
                                                    study_id =  1)
  expect_equal(ncol(study1_performance), 4)
  expect_equal(nrow(study1_performance), 3)
  
  
})

test_that("get_performance", {
  studies_performance <- get_performance_by_study(model = model, 
                                                  test_data = test_data,
                                                  study_var =  "study")
  expect_equal(nrow(studies_performance), 30)
  expect_equal(ncol(studies_performance), 4)
  
  
})


