coef_path_plot <- function(mod_type, lambda_value, impact_vars) {

  model <- 
    glmnet(x, y, alpha = mod_type, penalty.factor = penalty)
  
  beta_mat <- 
    as.matrix(model$beta)
  
  coef_df <- 
    as.data.frame(beta_mat)
  
  lambda_vals <- 
    tibble(lambda = model$lambda, lambda_index = colnames(coef_df))
  
  coef_df$predictor <- 
    rownames(coef_df)
  
  coef_long <- 
    coef_df %>%
    pivot_longer(
      cols = -predictor,
      names_to = "lambda_index",
      values_to = "coefficient"
    ) %>% 
    left_join(
      lambda_vals, 
      by = "lambda_index", 
      keep = FALSE) %>% 
    mutate(
      log_lambda = log(lambda)
    )
  
  selected_vars <- 
    colnames(x[,!startsWith(colnames(x), "donor_name") & colnames(x) != "year_std"])
  
  coef_filtered <- coef_long %>%
    filter(predictor %in% selected_vars)
  
  ggplot() +
    geom_line(
      data = coef_filtered[!coef_filtered[["predictor"]] %in% impact_vars, ],
      aes(x = log_lambda, y = coefficient, group = predictor),
      size = 0.5, color = grey, alpha = 0.25) +
    geom_line(
      data = coef_filtered[coef_filtered[["predictor"]] %in% impact_vars, ], 
      aes(x = log_lambda, y = coefficient, color = predictor, linetype = predictor), 
    ) +
    geom_vline(xintercept = log(lambda_value)) +
    labs(
      x = "log(Lambda)",
      y = "Coefficient"
    ) +
    theme_minimal()
  
}
