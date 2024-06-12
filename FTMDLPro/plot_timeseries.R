library(ggplot2)

# Function to create a time series plot with additional customization options
create_timeseries_plot <- function(data, time_var, y_var, param_labels, plot_type = "line", point_color = "blue", 
                                   title = "Time Series Plot", subtitle = NULL) {
  # Debugging: Print the first few rows of the data
  print("Data head in create_timeseries_plot:")
  print(head(data))
  
  # Debugging: Print the column names of the data
  print("Column names in data in create_timeseries_plot:")
  print(names(data))
  
  # Check if the columns exist in the data
  if (!(time_var %in% names(data))) {
    stop(paste("The time variable", time_var, "is not present in the data."))
  }
  
  if (!(y_var %in% names(data))) {
    stop(paste("The parameter", y_var, "is not present in the data."))
  }
  
  # Get label for y-axis
  y_label <- param_labels[[y_var]]
  if (is.null(y_label)) {
    y_label <- y_var  # Fallback to the parameter name if no label is found
  }
  
  # Create the plot base
  plot <- ggplot(data, aes_string(x = time_var, y = y_var)) +
    labs(title = title, subtitle = subtitle, x = "Time", y = y_label) +
    theme_minimal(base_size = 15) +  # Set base font size for the plot
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  # Center and bold title
      plot.subtitle = element_text(hjust = 0.5, size = 15),  # Center subtitle
      axis.title.x = element_text(face = "bold", size = 14),  # Bold x-axis label
      axis.title.y = element_text(face = "bold", size = 14),  # Bold y-axis label
      axis.text = element_text(size = 12),  # Increase axis text size
      panel.grid.major = element_line(color = "gray", size = 0.5),  # Customize grid lines
      panel.grid.minor = element_line(color = "gray", size = 0.25),
      legend.position = "bottom",  # Position the legend at the bottom
      legend.title = element_blank()  # Remove legend title
    )
  
  # Add plot type
  if (plot_type == "line") {
    plot <- plot + geom_line(color = point_color, size = 1) +  # Set line size
      geom_point(color = point_color, size = 3)  # Add points to the line plot
  } else if (plot_type == "scatter") {
    plot <- plot + geom_point(color = point_color, size = 3)  # Set point size
  }
  
  return(plot)
}
