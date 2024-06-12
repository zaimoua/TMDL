# regression_analysis.R
library(ggplot2)
library(data.table)
library(plotly)

# Helper function to calculate overall p-value of a regression model
lmp <- function(modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm'")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  return(p)
}

# Function to perform regression analysis
perform_regression <- function(data, response, explanatory1, explanatory2 = NULL, include_interaction = FALSE, polynomial_degree = 1) {
  if (!response %in% names(data)) {
    stop(paste("Response variable", response, "not found in data"))
  }
  if (!explanatory1 %in% names(data)) {
    stop(paste("Explanatory variable", explanatory1, "not found in data"))
  }
  if (!is.null(explanatory2) && !explanatory2 %in% names(data)) {
    stop(paste("Explanatory variable", explanatory2, "not found in data"))
  }
  
  # Filter out rows with NaN or NA values in the relevant columns
  data <- data[!is.na(data[[response]]) & !is.na(data[[explanatory1]]), ]
  if (!is.null(explanatory2)) {
    data <- data[!is.na(data[[explanatory2]]), ]
  }
  
  if (nrow(data) == 0) {
    stop("No valid data available after removing NaN/NA values")
  }
  
  if (polynomial_degree > 1) {
    data[[explanatory1]] <- poly(data[[explanatory1]], polynomial_degree, raw = TRUE)
    if (!is.null(explanatory2)) {
      data[[explanatory2]] <- poly(data[[explanatory2]], polynomial_degree, raw = TRUE)
    }
  }
  
  if (is.null(explanatory2)) {
    formula <- as.formula(paste(response, "~", explanatory1))
  } else {
    if (include_interaction) {
      formula <- as.formula(paste(response, "~", explanatory1, "*", explanatory2))
    } else {
      formula <- as.formula(paste(response, "~", explanatory1, "+", explanatory2))
    }
  }
  
  model <- lm(formula, data = data)
  return(model)
}

# Function to create a regression plot
create_regression_plot <- function(data, response, explanatory1, explanatory2 = NULL, param_labels, include_interaction = FALSE, polynomial_degree = 1) {
  response_label <- param_labels[[response]]
  explanatory1_label <- param_labels[[explanatory1]]
  
  if (is.null(response_label)) response_label <- response
  if (is.null(explanatory1_label)) explanatory1_label <- explanatory1
  
  if (polynomial_degree > 1) {
    data[[explanatory1]] <- poly(data[[explanatory1]], polynomial_degree, raw = TRUE)
    if (!is.null(explanatory2)) {
      data[[explanatory2]] <- poly(data[[explanatory2]], polynomial_degree, raw = TRUE)
    }
  }
  
  if (is.null(explanatory2)) {
    plot <- ggplot(data, aes_string(x = explanatory1, y = response)) +
      geom_point(color = "blue", size = 3) +
      geom_smooth(method = "lm", color = "red") +
      labs(x = explanatory1_label, y = response_label) +
      theme_minimal(base_size = 15)
  } else {
    explanatory2_label <- param_labels[[explanatory2]]
    if (is.null(explanatory2_label)) explanatory2_label <- explanatory2
    if (include_interaction) {
      plot <- ggplot(data, aes_string(x = explanatory1, y = response, color = explanatory2)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        labs(x = explanatory1_label, y = response_label, color = explanatory2_label) +
        theme_minimal(base_size = 15)
    } else {
      plot <- ggplot(data, aes_string(x = explanatory1, y = response)) +
        geom_point(color = "blue", size = 3) +
        geom_smooth(method = "lm", color = "red") +
        labs(x = explanatory1_label, y = response_label) +
        theme_minimal(base_size = 15)
    }
  }
  
  return(plot)
}

# Function to create diagnostic plots
create_diagnostic_plots <- function(model) {
  plot_list <- list()
  
  # Residuals vs Fitted
  plot_list$resid_vs_fitted <- ggplot(model, aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Fitted values", y = "Residuals") +
    theme_minimal(base_size = 15)
  
  # Normal Q-Q
  plot_list$qq_plot <- ggplot(model, aes(sample = .stdresid)) +
    stat_qq() +
    stat_qq_line() +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme_minimal(base_size = 15)
  
  # Scale-Location
  plot_list$scale_location <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(se = FALSE, color = "red") +
    labs(x = "Fitted values", y = "Square root of |Standardized Residuals|") +
    theme_minimal(base_size = 15)
  
  return(plot_list)
}
