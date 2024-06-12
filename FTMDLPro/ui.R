library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(plotly)

source('labels.R')

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      tags$img(src = "logo.png", height = "40px", style = "vertical-align: middle; margin-right: 10px;"),
      "Florida TMDL Data Analyzer"
    ),
    titleWidth = 350,
    .list = tagList(
      tags$li(class = "dropdown",
              tags$a(href = "https://floridadep.gov/TMDL", target = "_blank", "DEP Home"))
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Main Page", tabName = "main_page", icon = icon("home")),
      menuItem("Data Extraction", tabName = "data_extraction", icon = icon("database")),
      menuItem("Statistical Summary", tabName = "statistical_summary", icon = icon("table")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Regression Analysis", tabName = "regression_analysis", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f4f4f9;
          font-family: Arial, sans-serif;
        }
        .btn-primary {
          background-color: #0066cc;
          border-color: #005bb5;
        }
        .btn-primary:hover {
          background-color: #005bb5;
          border-color: #004c99;
        }
        .help-text {
          font-size: 12px;
          color: #666666;
        }
      "))
    ),
    tabItems(
      # Main Page tab content
      tabItem(tabName = "main_page",
              fluidRow(
                box(
                  title = "Welcome to the Florida TMDL Data Analyzer!",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  p("This tool assists TMDL developers in performing comprehensive water quality data analysis and developing Total Maximum Daily Loads (TMDLs) using advanced regression analysis.")
                ),
                box(
                  title = "Key Features",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  tags$ul(
                    tags$li(tags$strong("Data Extraction:"), " Efficiently retrieve water quality data from the IWR65 database, including information on nutrients, bacteria, and metals. Export data as Excel files for further analysis."),
                    tags$li(tags$strong("Customizable Parameters:"), " Select specific parameters and waterbody IDs (WBIDs) to generate tailored reports."),
                    tags$li(tags$strong("Advanced Visualization:"), " Create detailed plots and visual representations of data to enhance understanding and insights."),
                    tags$li(tags$strong("Regression Analysis:"), " Perform regression analysis to support advanced modeling and predictive analytics, helping to identify trends and make data-driven decisions.")
                  )
                ),
                box(
                  title = "Instructions",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  p(strong("Step-by-Step Guide:")),
                  tags$ol(
                    tags$li("Navigate to the 'Data Extraction' tab to retrieve water quality data. Select the desired parameters and waterbody IDs (WBIDs), then export the data as an Excel file."),
                    tags$li("Use the 'Visualization' tab to create visual representations of the extracted data. Customize the charts to fit your analysis needs."),
                    tags$li("In the 'Regression Analysis' tab, conduct regression analysis to support advanced modeling and predictive analytics. Identify trends and make data-driven decisions."),
                    tags$li("Refer to the 'About' tab for detailed information about the app and its development."),
                    tags$li("For support or feedback, visit the 'Contact' tab to reach out to the developer.")
                  ),
                  br(),
                  p("For detailed tutorials and user guides, please visit the official documentation or contact support.")
                ),
                box(
                  title = "Note",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  p("This is an early version of the app. We welcome your feedback and suggestions to help us improve and add more features, including additional modeling capabilities. Please visit the 'Contact' tab to share your thoughts.")
                )
              )
      ),
      # Data Extraction Tab
      tabItem(tabName = "data_extraction",
              fluidRow(
                column(3,
                       box(
                         title = "Data Extraction",
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         textInput("wbid", "Enter WBID:", value = "1476"),
                         selectInput("param_group", "Select Parameter Group:",
                                     choices = c("Nutrients", "Bacteria", "Metals")),
                         selectizeInput("parameters", "Select Parameters:", 
                                        choices = NULL, multiple = TRUE),
                         selectInput("lakewatch", "Include Lakewatch Data?", 
                                     choices = c("Yes", "No")),
                         sliderInput("year_range", "Select Year Range:",
                                     min = 1990, max = 2024, value = c(2000, 2024), step = 1, sep = ""),
                         textInput("station_id", "Enter Station ID (optional):", value = ""),
                         actionButton("runButton", "Run", class = "btn-primary"),
                         downloadButton("downloadData", "Download Data", class = "btn-primary"),
                         helpText("Click 'Run' to extract data and then 'Download Data' to save the file.", class = "help-text"),
                         textOutput("completionMessage")
                       )
                ),
                column(9,
                       uiOutput("data_overview")
                )
              ),
              fluidRow(
                box(
                  title = "Instructions for Data Extraction",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  "1. Enter the WBID of the water body you are interested in.",
                  br(),
                  "2. Select the parameter group you wish to analyze (e.g., Nutrients, Bacteria, Metals).",
                  br(),
                  "3. Choose specific parameters from the selected group.",
                  br(),
                  "4. Decide whether to include Lakewatch data.",
                  br(),
                  "5. Select the year range for the data extraction.",
                  br(),
                  "6. Optionally, enter a Station ID for more specific data extraction.",
                  br(),
                  "7. Click 'Run' to extract the data.",
                  br(),
                  "8. After the data is extracted, click 'Download Data' to save it as a file."
                )
              )
      ),
      # Add the Statistical Summary Tab
      tabItem(tabName = "statistical_summary",
              fluidRow(
                column(3,
                       box(
                         title = "Statistical Summary",
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         selectInput("summary_data_type", "Select Data Type:", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                         selectizeInput("summary_param", "Select Parameter:", choices = NULL),
                         actionButton("runSummary", "Generate Summary", class = "btn-primary")
                       )
                ),
                column(9,
                       uiOutput("summary_ui")
                )
              )
      ),
      # Visualization Tab
      tabItem(tabName = "visualization",
              fluidRow(
                column(3,
                       box(
                         title = "Data Visualization",
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         tabsetPanel(
                           id = "vizTabs",
                           tabPanel("Customize Plot",
                                    fluidRow(
                                      column(12, selectInput("data_type", "Select Data Type:", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))),
                                      column(12, selectizeInput("x_param", "Select X Parameter:", choices = NULL)),
                                      column(12, selectizeInput("y_param", "Select Y Parameter:", choices = NULL)),
                                      column(12, selectInput("plot_type", "Select Plot Type:", choices = c("Scatter" = "scatter", "Line" = "line"))),
                                      column(12, checkboxInput("add_trend_line", "Add Trend Line", value = FALSE)),
                                      column(12, textInput("point_color", "Select Point Color:", value = "blue")),
                                      column(12, textInput("line_color", "Select Line Color:", value = "red")),
                                      column(12, numericInput("point_size", "Point Size:", value = 3, min = 1)),
                                      column(12, numericInput("line_size", "Line Size:", value = 1, min = 0.5)),
                                      column(12, textInput("plot_title", "Plot Title:", value = "XY Plot")),
                                      column(12, textInput("x_axis_label", "X Axis Label:", value = "")),
                                      column(12, textInput("y_axis_label", "Y Axis Label:", value = "")),
                                      column(12, downloadButton("downloadPlot", "Download Plot"))
                                    )
                           ),
                           tabPanel("Nutrient Analysis",
                                    fluidRow(
                                      column(12, selectInput("data_type_nutrients", "Select Data Type:", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))),
                                      column(12, checkboxInput("add_trend_line_nutrients", "Add Trend Line", value = FALSE)),
                                      column(12, downloadButton("downloadNutrientPlots", "Download All Nutrient Plots"))
                                    )
                           ),
                           tabPanel("Time Series Plot",
                                    fluidRow(
                                      column(12, selectizeInput("ts_param", "Select Parameter:", choices = NULL)),
                                      column(12, selectInput("data_type_ts", "Select Data Type:", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm"))),
                                      column(12, selectInput("plot_type_ts", "Select Plot Type:", choices = c("Line" = "line", "Scatter" = "scatter"))),
                                      column(12, textInput("point_color_ts", "Select Point Color:", value = "blue")),
                                      column(12, textInput("line_color_ts", "Select Line Color:", value = "red")),
                                      column(12, numericInput("point_size_ts", "Point Size:", value = 3, min = 1)),
                                      column(12, numericInput("line_size_ts", "Line Size:", value = 1, min = 0.5)),
                                      column(12, textInput("plot_title_ts", "Plot Title:", value = "Time Series Plot")),
                                      column(12, textInput("x_axis_label_ts", "X Axis Label:", value = "")),
                                      column(12, textInput("y_axis_label_ts", "Y Axis Label:", value = "")),
                                      column(12, downloadButton("downloadTsPlot", "Download Time Series Plot"))
                                    )
                           )
                         )
                       )
                ),
                column(9,
                       tabsetPanel(
                         tabPanel("Customize Plot", plotlyOutput("xyPlot", height = "500px")),
                         tabPanel("Nutrient Analysis",
                                  plotlyOutput("nutrientPlot1", height = "400px"),
                                  plotlyOutput("nutrientPlot2", height = "400px"),
                                  plotlyOutput("nutrientPlot3", height = "400px"),
                                  plotlyOutput("nutrientPlot4", height = "400px"),
                                  plotlyOutput("nutrientPlot5", height = "400px"),
                                  plotlyOutput("nutrientPlot6", height = "400px")),
                         tabPanel("Time Series Plot", plotlyOutput("tsPlot", height = "500px"))
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Instructions for Data Visualization",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  "1. In the 'Customize Plot' tab, select the data type and parameters for the X and Y axes.",
                  br(),
                  "2. Choose the plot type (Scatter or Line) and optionally add a trend line.",
                  br(),
                  "3. Customize the appearance of the plot by selecting colors and sizes for points and lines.",
                  br(),
                  "4. Provide a title and axis labels for your plot.",
                  br(),
                  "5. Click 'Download Plot' to save the customized plot.",
                  br(), br(),
                  "In the 'Nutrient Analysis' tab, follow similar steps to customize and download nutrient-related plots.",
                  br(), br(),
                  "In the 'Time Series Plot' tab, select parameters and customize the appearance of time series plots. Click 'Download Time Series Plot' to save your work."
                )
              )
      ),
      # Regression Analysis Tab
      tabItem(tabName = "regression_analysis",
              fluidRow(
                column(3,
                       box(
                         title = "Regression Analysis",
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         selectInput("regression_type", "Select Data Type:", choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                         selectizeInput("regression_response", "Select Response Variable:", choices = NULL),
                         selectizeInput("regression_explanatory1", "Select Explanatory Variable 1:", choices = NULL),
                         checkboxInput("enable_explanatory2", "Enable Explanatory Variable 2", value = FALSE),
                         conditionalPanel(
                           condition = "input.enable_explanatory2 == true",
                           selectizeInput("regression_explanatory2", "Select Explanatory Variable 2 (optional):", choices = NULL)
                         ),
                         numericInput("minyear", "Enter Minimum Year:", value = 2000, min = 1990, max = 2024),
                         actionButton("runRegression", "Run Regression", class = "btn-primary")
                       )
                ),
                column(9,
                       tableOutput("regressionSummary"),
                       plotlyOutput("regressionPlot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Instructions for Regression Analysis",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  "1. Select the type of data to be used for the regression analysis (Raw Data or Annual Geometric Means).",
                  br(),
                  "2. Choose the response variable (dependent variable) for the regression model.",
                  br(),
                  "3. Select the first explanatory variable (independent variable).",
                  br(),
                  "4. If needed, enable and select a second explanatory variable.",
                  br(),
                  "5. Enter the minimum year for the data to be included in the analysis.",
                  br(),
                  "6. Click 'Run Regression' to perform the analysis.",
                  br(),
                  "7. Review the regression summary and visual plot of the regression results."
                )
              )
      ),
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About this App",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  p("Welcome to the Florida TMDL Data Analyzer, a comprehensive tool developed for the Florida Department of Environmental Protection. This app is designed to facilitate water quality analysis and support the development of Total Maximum Daily Loads (TMDLs) using advanced regression analysis."),
                  p(strong("Development Details:")),
                  p("The Florida TMDL Data Analyzer was developed using the R programming language and Shiny framework, providing an interactive and user-friendly interface. The app accesses data from the IWR (Impaired Waters Rule) database 65, which contains essential water quality information on nutrients, bacteria, and metals."),
                  p(strong("Data Extraction Process:")),
                  p("The data extraction process involves retrieving relevant water quality data from the IWR65 database based on user-specified parameters such as WBID (Waterbody IDs) and specific water quality parameters like nutrients, bacteria, and metals. The process includes several steps:"),
                  tags$ul(
                    tags$li(strong("1. Validation:"), " Ensures that required inputs such as WBID and parameters are provided and that the IWR database file exists."),
                    tags$li(strong("2. SQL Query Construction:"), " Constructs SQL queries based on the specified parameters and optional filters such as year range and station ID."),
                    tags$li(strong("3. Data Retrieval:"), " Connects to the SQLite database, executes the SQL queries, and retrieves the raw data."),
                    tags$li(strong("4. Data Processing:"), " Converts the raw data into a data.table format and processes it to calculate yearly geometric means, minimum, maximum, and other statistical summaries."),
                    tags$li(strong("5. Output Preparation:"), " Summarizes the processed data and prepares it for export as an Excel file.")
                  ),
                  p("This detailed extraction process ensures that users can efficiently obtain and analyze water quality data specific to their needs."),
                  p(strong("Key Features:")),
                  tags$ul(
                    tags$li(tags$strong("Data Extraction:"), " Efficiently retrieve water quality data and export it as an Excel file."),
                    tags$li(tags$strong("Customizable Parameters:"), " Select specific parameters and waterbody IDs (WBIDs) to generate tailored reports."),
                    tags$li(tags$strong("User-Friendly Interface:"), " Simplify the process of water quality data analysis and reporting."),
                    tags$li(tags$strong("Advanced Visualization:"), " Create detailed plots and visual representations of data for better insights."),
                    tags$li(tags$strong("Regression Analysis:"), " Perform regression analysis to support advanced modeling and predictive analytics.")
                  ),
                  p(strong("Future Updates:")),
                  p("Future updates will include additional modeling features to further enhance the app's capabilities. We are continuously improving based on user feedback."),
                  p(strong("Developer:")),
                  p("This app was developed by Zaim Ouazzani, an Environmental Specialist in the TMDL section at the Florida Department of Environmental Protection."),
                  p(strong("Feedback and Support:")),
                  p("We welcome your feedback to help us improve this tool. For more information or support, please visit the Contact section.")
                )
              )
      ),
      # Contact Tab
      tabItem(tabName = "contact",
              fluidRow(
                box(
                  title = "Contact Information",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  p("For support or feedback, please contact:"),
                  p("Zaim Ouazzani"),
                  p("Florida Department of Environmental Protection"),
                  p("zaim.ouazzani@FloridaDEP.gov")
                )
              )
      )
    ),
    # Footer
    tags$footer(
      class = "footer",
      p("Developed by Zaim Ouazzani | © 2024 Florida Department of Environmental Protection")
    )
  )
)