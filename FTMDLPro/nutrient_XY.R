library(ggplot2)

# Function to create nutrient XY plot with additional customization options
create_nutrient_xy_plot <- function(data, x_var, y_var, param_labels, data_type = "raw", add_trend_line = FALSE) {
  # Check if the columns exist in the data
  if (!(x_var %in% names(data)) | !(y_var %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }
  
  # Get labels for x and y axes
  x_label <- param_labels[[x_var]]
  y_label <- param_labels[[y_var]]
  
  # Create the plot base
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    labs(x = x_label, y = y_label) +
    theme_minimal(base_size = 15) +  # Set base font size for the plot
    theme(
      axis.title.x = element_text(face = "bold", size = 14),  # Bold x-axis label
      axis.title.y = element_text(face = "bold", size = 14),  # Bold y-axis label
      axis.text = element_text(size = 12),  # Increase axis text size
      panel.grid.major = element_line(color = "gray", size = 0.5),  # Customize grid lines
      panel.grid.minor = element_line(color = "gray", size = 0.25),
      legend.position = "bottom",  # Position the legend at the bottom
      legend.title = element_blank()  # Remove legend title
    )
  
  # Add plot type
  plot <- plot + geom_point(color = "blue", size = 3)  # Set point size
  if (add_trend_line) {
    plot <- plot + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red")  # Add a trend line
  }
  
  return(plot)
}
