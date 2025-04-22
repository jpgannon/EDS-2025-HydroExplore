library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

sheds <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")


ui <- navbarPage(
  title = div(
    style = "color: white; font-size: 24px; font-weight: bold; text-align: center;",
    "Hubbard Brook Experimental Forest: Watershed Precipitation and Flow Trend Analysis"
  ),
  windowTitle = "Watershed Analysis",

  tags$head(
    tags$style(HTML("
      body { background-color: #A7C7E7; }
      .navbar-default { background-color: #4A7EBB; border-color: #4A7EBB; }
      .navbar-default .navbar-nav > li > a { color: white; font-weight: bold; }
      .navbar-default .navbar-brand { color: white !important; }
      .tab-content { background-color: #DCE6F1; padding: 20px; border-radius: 10px; }
      .sidebarPanel { background-color: #B0C4DE; padding: 15px; border-radius: 10px; }
      .btn { background-color: #4A7EBB; color: white; font-weight: bold; }
      .btn:hover { background-color: #365E96; }
      .well { background-color: #B0C4DE; border-radius: 10px; }
      h3, h4, strong { color: #2B4878; }
      input, select, textarea { border-radius: 5px; border: 1px solid #365E96; padding: 5px; }
    "))
  ),

  tabPanel("Trend Analysis",
           sidebarLayout(
             sidebarPanel(
               class = "sidebarPanel",
               h4("Choose Watersheds (1-9):"),
               checkboxGroupInput("Watershed", "Select Watersheds", sheds),

               h4("Add Baseflow Line"),
               actionButton("add_baseflow", "Baseflow"),

               h4("Selected Data Range:"),
               dateRangeInput("range", "Period of Record"),

               strong("Total Days:"),
               textOutput("total_days"),

               strong("Total Missing Days:"),
               textOutput("missing_days"),

               strong("Average Discharge:"),
               textOutput("avg_discharge"),

               strong("Median Discharge:"),
               textOutput("median_discharge")
             ),

             mainPanel(
               h3("Graph will be displayed here based on input selections."),
               plotOutput("trend_plot", height = "400px")
             )
           )
  ),

  tabPanel("Rolling Averages",
           sidebarLayout(
             sidebarPanel(
               class = "sidebarPanel",
               h4("Select Rolling Window (Days):"),
               textInput("30", label = NULL, value = "30"),

               actionButton("rolling", "Rolling Average"),


             ),
             mainPanel(
               h3("Rolling Averages Table"),
               tableOutput("rolling_avg_table")
             )
           )
  ),

  tabPanel("Add Data/See Tables",
           sidebarLayout(
             sidebarPanel(
               class = "sidebarPanel",
               h4("Enter New Dataset:"),
               fileInput("load_data", "Choose CSV File",
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
             ),
             mainPanel(
               h3("Data and Tables Coming Soon...")
             )
           )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)

  observeEvent(input$load_data, {
    req(input$load_data)
    dataset(read.csv(input$load_data$datapath))
  })
}

shinyApp(ui, server)

