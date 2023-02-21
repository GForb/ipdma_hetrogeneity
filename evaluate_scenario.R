

evaluate_scenario <- function(data_generating_function, analysis_mdoel, train_n, test_n, ...) {
  train_data <- data_generating_function(n = train_n, ...)
  test_data <- data_generating_function(n = test_n, ...)
  model <- analysis_model(train_data)
  performance <- get_performance_by_study(model, test_data, "study")
  metrics <- performance$metric |> unique()
  print(metrics)
  lapply(metrics, meta_analyse_metric, performance_data = performance)

}

meta_analyse_metric <- function(metric, performance_data) {
  metric_data <- performance_data[metric == metric,]
  meta_analysis <- metafor::rma(yi = est, sei = se, method = "REML",
      test = "knha", data = metric_data)
  return(meta_analysis)
}

