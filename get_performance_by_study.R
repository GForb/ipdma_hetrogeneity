
# Pipeline for conducting ipd meta analysis

get_performance_by_study <- function(model, test_data, study_var) {
  studies <- test_data[,study_var] |> 
    unique() 
  
  performance_by_study <- lapply(studies, 
                                 get_performance_for_a_study, 
                                 model = model, 
                                 test_data = test_data, 
                                 study_var = study_var)
    performance_by_study <-  as.data.frame(do.call(rbind, performance_by_study))

  return(performance_by_study)
}

get_performance_for_a_study <- function(study_id, study_var, test_data, model) {
  study_test_data <- test_data |> 
     subset(subset = test_data[,study_var] ==study_id)
  performance <- get_performance(model = model, test_data = study_test_data)
  performance$study <- study_id
  return(performance)
}

get_performance <- function(model, test_data) {
  y <- test_data[,1]
  y_hat <- predict(newdata  = test_data, object = model)
  model_performance <- calc_performance(y, y_hat)
  return(model_performance)
}

calc_performance <- function(y, y_hat) {
  predictions <- data.frame(y, y_hat)
  
  #performance vectors
  calib_alpha <- glm(y ~ 1, offset = predictions$y_hat,  data = predictions, family = "binomial") |>  
    summary() 
  calib_alpha <- calib_alpha$coefficients[1, 1:2]

  
  calib_slope <- glm(y ~y_hat, data = predictions, family = "binomial") |> 
    summary()
  calib_slope <- calib_slope$coefficients[2, 1:2]

  cstat <- pROC::roc(y, y_hat, plot = FALSE, ci = TRUE, quiet = TRUE)$ci[1:3]
  cstat_se <- (cstat[2] - cstat[1])/qnorm(0.975)
  cstat <- c(cstat[2], cstat[1])

  #Creating a dataframe of performance measures
  model_performance <- rbind(cstat, calib_alpha, calib_slope) |> data.frame()
  model_performance$metric <- rownames(model_performance)
  rownames(model_performance) <- NULL
  colnames(model_performance) <- c("est", "se", "metric")
  return(model_performance)
}
