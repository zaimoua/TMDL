library(ggplot2)

# Function to create XY plot with additional customization options
create_xy_plot <- function(data, x_var, y_var, param_labels, plot_type = "scatter", point_color = "blue", 
                           title = "XY Plot", subtitle = NULL, add_trend_line = FALSE, trend_line_type = "lm") {
  # Check if the columns exist in the data
  if (!(x_var %in% names(data)) | !(y_var %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }
  
  # Get labels for x and y axes
  x_label <- param_labels[[x_var]]
  y_label <- param_labels[[y_var]]
  
  # Create the plot base
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
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
  if (plot_type == "scatter") {
    plot <- plot + geom_point(color = point_color, size = 3)  # Set point size
    if (add_trend_line) {
      plot <- plot + geom_smooth(method = trend_line_type, se = FALSE, linetype = "dashed", color = "red")  # Add a trend line
    }
  } else if (plot_type == "line") {
    plot <- plot + geom_line(color = point_color, size = 1) +  # Set line size
      geom_point(color = point_color, size = 3)  # Add points to the line plot
  }
  
  return(plot)
}
