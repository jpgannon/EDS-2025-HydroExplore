#install.packages("shiny")
library(shiny)
library(sqldf)
library(tidyverse)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("DT")
library(DT)
#install.packages("plotly")
library(plotly)
#install.packages("zoo")
library(zoo)

# UI: Navigation Bar with Multiple Pages
ui <- navbarPage("Hubbard Brook Watershed Data Analysis", theme = shinytheme("cerulean"),
                 
                 # 1. Main Page: Precipitation and Flow Trend Analysis
                 tabPanel("Trend Analysis",
                          fluidPage(
                            titlePanel(h3("Hubbard Brook Experimental Forest: Watershed Precipitation and Flow Trend Analysis", align = "center")),
                            
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file", "Enter New Dataset:", accept = c(".csv")),
                                checkboxGroupInput("watersheds", "Choose Watersheds (1-9):", choices = as.character(1:9), selected = c("1", "2", "7", "9")),
                                checkboxInput("addBaseflow", "Add Baseflow Line", value = FALSE),
                                hr(),
                                h4("Selected Data Range:"),
                                verbatimTextOutput("dateRangeText"),
                                h4("Period of Record:"),
                                verbatimTextOutput("recordPeriod"),
                                h5("Total Days:"), verbatimTextOutput("totalDays"),
                                h5("Total Flagged Days:"), verbatimTextOutput("missingDays"),
                                h5("Average Discharge:"), verbatimTextOutput("avgDischarge"),
                                h5("Median Discharge:"), verbatimTextOutput("medianDischarge")
                              ),
                              
                              mainPanel(
                                plotlyOutput("trendPlot")
                              )
                            )
                          )
                 ),
                 
                 # 2. Rolling Averages Tab
                 tabPanel("Rolling Averages",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("rollingWindow", "Rolling Average (Days):", value = 1, min = 1, max = 90),
                              actionButton("applyRolling", "Apply")
                            ),
                            mainPanel(
                              plotOutput("rollingPlot")
                            )
                          )
                 ),
                 
                 # 3. Add Data/See Tables Tab
                 tabPanel("See Tables",
                          sidebarLayout(
                            sidebarPanel(
                              #fileInput("uploadFile", "Upload Your Dataset (CSV)", accept = ".csv"),
                              downloadButton("downloadData", "Download Current Data")
                            ),
                            mainPanel(
                              DTOutput("dataTable")
                            )
                          )
                 )
)


# Define Server
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  dataset <- {
    watershed_precip <-read.csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/dailyWatershedPrecip1956-2024.csv"))
    watershed_flow <- read.csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/HBEF_DailyStreamflow_1956-2023.csv"))
    
    
    watershed_precip <- watershed_precip |> mutate_at("watershed", str_replace, "W", "")
    
    combined_data <- sqldf(
      "select a.DATE obs_date, a.watershed as watershed, 
  A.precip as precip,
  b.Streamflow as streamflow, b.Flag as flag
  from watershed_precip as a
  left outer join watershed_flow as b 
  on (
    a.watershed = B.WS
    and a.DATE = b.DATE
  )
  where b.streamflow is not Null
  and A.precip is not NULL
  order by a.DATE
  "
    )
    combined_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(combined_data$obs_date)),
                                                       month(as.Date(combined_data$obs_date)),
                                                       day(as.Date(combined_data$obs_date)),
                                                       week(as.Date(combined_data$obs_date)))
  
  
      
      
      return(combined_data)
    }
  
  dataset <- dataset()
  
  # Display Data Summary
  output$dateRangeText <- renderText({ "2020–2025" })
  output$recordPeriod <- renderText({ "2020-2025" })
  output$totalDays <- renderText({count(unique(dataset))})
  output$FlaggedDays <- renderText({count(unique(filter(dataset, flag == "1")))})
  output$avgDischarge <- renderText({mean(dataset$streamflow)})
  output$medianDischarge <- renderText({median(dataset$streamflow)})
  
  # Plot Trends
  output$trendPlot <- renderPlotly({
    df <- dataset()
    df$Date <- as.Date(df$Date)
    
    p <- ggplot(df, aes(x = Date)) +
      geom_bar(aes(y = Precipitation), stat = "identity", fill = "lightblue", alpha = 0.6) +
      scale_y_continuous(name = "Precipitation (mm/day)", sec.axis = sec_axis(~ ., name = "Streamflow (mm/day)")) +
      theme_minimal() +
      labs(title = "Precipitation & Flow Trend Analysis", x = "Date")
    
    if (input$addBaseflow) {
      p <- p + geom_line(aes(y = Baseflow, color = "Baseflow"), size = 1)
    }
    
    ggplotly(p)
  })
}

# Run App
shinyApp(ui, server)
