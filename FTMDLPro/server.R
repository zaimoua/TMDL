library(shiny)
library(openxlsx)
library(shinyFiles)
library(RSQLite)
library(data.table)
library(ggplot2)
library(plotly)

source('labels.R')
source('data_functions.R')
source('plot_XY.R')
source('nutrient_XY.R')
source('plot_timeseries.R')
source('regression_analysis.R')

# Define server logic
server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  output_file <- reactiveVal(NULL)
  params <- reactiveVal(NULL)  # Define params as a reactive value
  
  observe({
    param_group <- input$param_group
    param_choices <- switch(param_group,
                            "Nutrients" = c("CHLA", "CHLAC", "TN", "TP", "COLOR", "ALK", "COND", "TEMP", "DO", "BOD", "DOSAT", "PORTH", "NH4", "NO3O2", "UNNH4", "PORD", "TKN", "TORTH"),
                            "Bacteria" = c("ENCOC", "FCOLI", "ECOLI"),
                            "Metals" = c("FE", "PB", "CU", "AG", "HARD"))
    
    params(param_choices)  # Update params with the choices
    
    updateSelectizeInput(session, "parameters", choices = param_choices, selected = head(param_choices, 5))
    updateSelectizeInput(session, "x_param", choices = param_choices)
    updateSelectizeInput(session, "y_param", choices = param_choices)
    updateSelectizeInput(session, "ts_param", choices = param_choices)
    updateSelectizeInput(session, "regression_response", choices = param_choices)
    updateSelectizeInput(session, "regression_explanatory1", choices = param_choices)
    updateSelectizeInput(session, "regression_explanatory2", choices = param_choices)
  })
  
  observeEvent(input$runButton, {
    WBID <- as.character(input$wbid)
    PARAM <- input$parameters
    LAKEWATCH <- input$lakewatch == "Yes"
    year_range <- input$year_range
    station_id <- input$station_id
    IWR <- 'IWR65.sqlite'  # Use relative path to the database file
    
    res <- tryCatch({
      data_extraction(WBID = WBID, PARAM = PARAM, LAKEWATCH = LAKEWATCH, IWR = IWR, year_range = year_range, station_id = station_id)
    }, error = function(e) {
      showNotification(paste("Error in data extraction:", e$message), type = "error")
      NULL
    })
    
    if (is.null(res)) return(NULL)
    
    results(res)
    output_file(file.path(tempdir(), paste0(input$param_group, "_Data_", WBID, ".xlsx")))
    
    wb <- createWorkbook()
    addWorksheet(wb, "Raw Data")
    addWorksheet(wb, "Yearly Geomeans")
    
    writeData(wb, sheet = "Raw Data", x = res$rawdata)
    writeData(wb, sheet = "Yearly Geomeans", x = res$geomeans)
    
    saveWorkbook(wb, file = output_file(), overwrite = TRUE)
    
    output$completionMessage <- renderText({
      paste("Data extraction completed. The file is ready for download.")
    })
    
    output$data_overview <- renderUI({
      tagList(
        h4("Raw Data Overview"),
        tableOutput("raw_table"),
        h4("Yearly Geomeans Overview"),
        tableOutput("geomean_table")
      )
    })
    
    output$raw_table <- renderTable({
      head(res$rawdata)
    })
    
    output$geomean_table <- renderTable({
      head(res$geomeans)
    })
    
    # Generate statistical summary
    summary_stats <- lapply(split(res$rawdata, res$rawdata$mastercode), function(df) {
      summary(df$result)
    })
    
    summary_table <- do.call(rbind, lapply(names(summary_stats), function(name) {
      data.frame(Parameter = name, Statistic = names(summary_stats[[name]]), Value = as.vector(summary_stats[[name]]))
    }))
    
    output$summary_table <- renderTable({
      summary_table
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Extracted_Data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      file.copy(output_file(), file)
    }
  )
  
  # New observe for handling tab-specific input updates
  observe({
    current_tab <- input$vizTabs
    param_choices <- params()  # Get the current params
    
    if (current_tab == "Customize Plot") {
      updateSelectInput(session, "data_type", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))
      updateSelectizeInput(session, "x_param", choices = param_choices)
      updateSelectizeInput(session, "y_param", choices = param_choices)
      updateSelectInput(session, "plot_type", choices = c("Scatter" = "scatter", "Line" = "line"))
      updateCheckboxInput(session, "add_trend_line", value = FALSE)
      updateTextInput(session, "point_color", value = "blue")  # Use textInput for color
      updateTextInput(session, "line_color", value = "red")  # Use textInput for color
      updateNumericInput(session, "point_size", value = 3, min = 1)
      updateNumericInput(session, "line_size", value = 1, min = 0.5)
      updateTextInput(session, "plot_title", value = "XY Plot")
      updateTextInput(session, "x_axis_label", value = "")
      updateTextInput(session, "y_axis_label", value = "")
    } else if (current_tab == "Nutrient Analysis") {
      updateSelectInput(session, "data_type_nutrients", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))
      updateCheckboxInput(session, "add_trend_line_nutrients", value = FALSE)
    } else if (current_tab == "Time Series Plot") {
      updateSelectizeInput(session, "ts_param", choices = param_choices)
      updateSelectInput(session, "data_type_ts", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))
      updateSelectInput(session, "plot_type_ts", choices = c("Line" = "line", "Scatter" = "scatter"))
      updateTextInput(session, "point_color_ts", value = "blue")  # Use textInput for color
      updateTextInput(session, "line_color_ts", value = "red")  # Use textInput for color
      updateNumericInput(session, "point_size_ts", value = 3, min = 1)
      updateNumericInput(session, "line_size_ts", value = 1, min = 0.5)
      updateTextInput(session, "plot_title_ts", value = "Time Series Plot")
      updateTextInput(session, "x_axis_label_ts", value = "")
      updateTextInput(session, "y_axis_label_ts", value = "")
    }
  })
  
  # Observe event for statistical summary
  observeEvent(input$runSummary, {
    res <- results()
    param <- input$summary_param
    data_type <- input$summary_data_type
    
    if (is.null(res)) {
      showNotification("No data available for summary", type = "warning")
      return(NULL)
    }
    
    data <- if (data_type == "raw") {
      res$rawdata
    } else {
      res$geomeans
    }
    
    if (!(param %in% data$mastercode)) {
      showNotification("Selected parameter is not available in the data", type = "warning")
      return(NULL)
    }
    
    summary_data <- data[data$mastercode == param, ]
    
    summary_stats <- summary(summary_data$result)
    
    output$summary_ui <- renderUI({
      tagList(
        h4("Statistical Summary"),
        tableOutput("summary_table")
      )
    })
    
    output$summary_table <- renderTable({
      data.frame(Statistic = names(summary_stats), Value = as.vector(summary_stats))
    })
  })
  
  observe({
    param_choices <- params()  # Get the current params
    updateSelectizeInput(session, "summary_param", choices = param_choices)
  })
  
  output$xyPlot <- renderPlotly({
    res <- results()
    x_param <- input$x_param
    y_param <- input$y_param
    data_type <- input$data_type
    plot_type <- input$plot_type
    point_color <- input$point_color
    plot_title <- input$plot_title
    add_trend_line <- input$add_trend_line
    
    if (is.null(res)) {
      showNotification("No data available for plotting", type = "warning")
      return(NULL)
    }
    
    if (data_type == "raw") {
      data <- res$rawdata
      data <- data[mastercode %in% c(x_param, y_param), ]
      data <- dcast(data, year + STA + month + day + time ~ mastercode, value.var = "result", fun.aggregate = mean)
    } else {
      data <- res$geomeans
    }
    
    if (!(x_param %in% names(data)) || !(y_param %in% names(data))) {
      showNotification("Selected parameters are not available in the data", type = "warning")
      return(NULL)
    }
    
    plot <- create_xy_plot(data, x_var = x_param, y_var = y_param, 
                           param_labels = param_labels, plot_type = plot_type, point_color = point_color, 
                           title = plot_title, add_trend_line = add_trend_line)
    
    ggplotly(plot)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("XY_Plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      res <- results()
      x_param <- input$x_param
      y_param <- input$y_param
      data_type <- input$data_type
      plot_type <- input$plot_type
      point_color <- input$point_color
      plot_title <- input$plot_title
      add_trend_line <- input$add_trend_line
      
      if (data_type == "raw") {
        data <- res$rawdata
        data <- data[mastercode %in% c(x_param, y_param), ]
        data <- dcast(data, year + STA + month + day + time ~ mastercode, value.var = "result", fun.aggregate = mean)
      } else {
        data <- res$geomeans
      }
      
      if (!is.null(res) && x_param %in% names(data) && y_param %in% names(data)) {
        plot <- create_xy_plot(data, x_var = x_param, y_var = y_param, 
                               param_labels = param_labels, plot_type = plot_type, point_color = point_color, 
                               title = plot_title, add_trend_line = add_trend_line)
        ggsave(file, plot = plot, device = "png")
      }
    }
  )
  
  observe({
    res <- results()
    data_type <- input$data_type_nutrients
    add_trend_line <- input$add_trend_line_nutrients
    nutrient_pairs <- list(
      c("TN", "CHLAC"),
      c("TP", "CHLAC"),
      c("TN", "TP"),
      c("COLOR", "CHLAC"),
      c("COLOR", "TN"),
      c("COLOR", "TP")
    )
    
    for (i in 1:length(nutrient_pairs)) {
      local({
        plot_id <- paste0("nutrientPlot", i)
        x_param <- nutrient_pairs[[i]][1]
        y_param <- nutrient_pairs[[i]][2]
        
        output[[plot_id]] <- renderPlotly({
          if (is.null(res)) {
            showNotification("No data available for plotting", type = "warning")
            return(NULL)
          }
          
          if (data_type == "raw") {
            data <- res$rawdata
            data <- data[mastercode %in% c(x_param, y_param), ]
            data <- dcast(data, year + STA + month + day + time ~ mastercode, value.var = "result", fun.aggregate = mean)
          } else {
            data <- res$geomeans
          }
          
          if (!(x_param %in% names(data)) || !(y_param %in% names(data))) {
            showNotification("Selected parameters are not available in the data", type = "warning")
            return(NULL)
          }
          
          plot <- create_nutrient_xy_plot(data, x_var = x_param, y_var = y_param, 
                                          param_labels = param_labels, data_type = data_type, add_trend_line = add_trend_line)
          
          ggplotly(plot)
        })
      })
    }
  })
  
  output$downloadNutrientPlots <- downloadHandler(
    filename = function() {
      paste("Nutrient_Plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      res <- results()
      data_type <- input$data_type_nutrients
      add_trend_line <- input$add_trend_line_nutrients
      nutrient_pairs <- list(
        c("COLOR", "CHLAC"),
        c("TN", "CHLAC"),
        c("TP", "CHLAC"),
        c("TN", "TP"),
        c("TN", "COLOR"),
        c("TP", "COLOR")
      )
      
      tmpdir <- tempdir()
      files <- c()
      
      for (i in 1:length(nutrient_pairs)) {
        plot <- create_nutrient_xy_plot(res, nutrient_pairs[[i]][1], nutrient_pairs[[i]][2], param_labels, data_type = data_type, add_trend_line = add_trend_line)
        file_path <- file.path(tmpdir, paste0(nutrient_pairs[[i]][1], "_vs_", nutrient_pairs[[i]][2], ".png"))
        ggsave(file_path, plot = plot, device = "png")
        files <- c(files, file_path)
      }
      
      zip(file, files)
    }
  )
  
  output$tsPlot <- renderPlotly({
    res <- results()
    ts_param <- input$ts_param
    data_type <- input$data_type_ts
    plot_type <- input$plot_type_ts
    point_color <- input$point_color_ts
    plot_title <- input$plot_title_ts
    
    if (is.null(res)) {
      showNotification("No data available for plotting", type = "warning")
      return(NULL)
    }
    
    if (data_type == "raw") {
      data <- res$rawdata
      data <- data[data$mastercode == ts_param, ]
      colnames(data)[colnames(data) == "result"] <- ts_param
    } else {
      data <- res$geomeans
    }
    
    if (nrow(data) == 0) {
      showNotification("Selected parameter is not available in the data", type = "warning")
      return(NULL)
    }
    
    plot <- create_timeseries_plot(data, time_var = "year", y_var = ts_param, 
                                   param_labels = param_labels, plot_type = plot_type, point_color = point_color, 
                                   title = plot_title)
    
    ggplotly(plot)
  })
  
  output$downloadTsPlot <- downloadHandler(
    filename = function() {
      paste("TimeSeries_Plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      res <- results()
      data_type <- input$data_type_ts
      ts_param <- input$ts_param
      plot_type <- input$plot_type_ts
      point_color <- input$point_color_ts
      plot_title <- input$plot_title_ts
      
      if (data_type == "raw") {
        data <- res$rawdata
        data <- data[mastercode == ts_param, ]
      } else {
        data <- res$geomeans
      }
      
      if (!is.null(res) && ts_param %in% names(data)) {
        plot <- create_timeseries_plot(data, time_var = "year", y_var = ts_param, 
                                       param_labels = param_labels, plot_type = plot_type, point_color = point_color, 
                                       title = plot_title)
        ggsave(file, plot = plot, device = "png")
      }
    }
  )
  
  observeEvent(results(), {
    res <- results()
    if (!is.null(res)) {
      output$summaryTable <- renderTable({
        res$summary
      })
    }
  })
  
  observeEvent(input$runRegression, {
    res <- results()
    WBID <- as.character(input$wbid)
    RESPONSE <- input$regression_response
    EXPLANATORY1 <- input$regression_explanatory1
    EXPLANATORY2 <- input$regression_explanatory2
    TYPE <- input$regression_type
    MINYEAR <- input$minyear
    IWR <- 'IWR65.sqlite'
    
    regression_results <- tryCatch({
      data <- if (TYPE == "raw") {
        rawdata_wide <- dcast(res$rawdata, year + STA ~ mastercode, value.var = "result", fun.aggregate = mean)
        rawdata_wide
      } else {
        res$geomeans
      }
      data <- data[data$year >= MINYEAR, ]
      perform_regression(data, RESPONSE, EXPLANATORY1, EXPLANATORY2)
    }, error = function(e) {
      showNotification(paste("Error in regression analysis:", e$message), type = "error")
      NULL
    })
    
    if (is.null(regression_results)) return(NULL)
    
    output$regressionSummary <- renderTable({
      summary_df <- as.data.frame(summary(regression_results)$coefficients)
      summary_df
    })
    
    output$regressionPlot <- renderPlotly({
      plot <- create_regression_plot(data, RESPONSE, EXPLANATORY1, EXPLANATORY2, param_labels)
      ggplotly(plot)
    })
  })
  
  output$completionMessage <- renderText("")
}
