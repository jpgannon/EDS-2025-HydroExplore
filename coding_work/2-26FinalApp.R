library(shiny)
library(sqldf)
library(tidyverse)
library(shinythemes)
library(DT)
library(plotly)
library(zoo)

ui <- navbarPage("Hubbard Brook Watershed Data Analysis", theme = shinytheme("cerulean"),

                 tabPanel("Trend Analysis",
                          fluidPage(
                            titlePanel(h3("Hubbard Brook Experimental Forest: Watershed Precipitation and Flow Trend Analysis", align = "center")),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("watersheds", "Choose Watersheds (1-9):", choices = as.character(1:9), selected = c("1")),
                                dateRangeInput("dateRange", "Select Date Range:", start = "1956-01-01", end = "2025-12-31",
                                               min = "1956-01-01", max = "2025-12-31"),
                                checkboxInput("addBaseflow", "Add Baseflow Line", value = FALSE),
                                hr(),
                                h4("Selected Data Range:"), verbatimTextOutput("dateRangeText"),
                                h4("Period of Record:"), verbatimTextOutput("recordPeriod"),
                                h5("Total Days:"), verbatimTextOutput("totalDays"),
                                h5("Total Flagged Days:"), verbatimTextOutput("missingDays"),
                                h5("Average Discharge:"), verbatimTextOutput("avgDischarge"),
                                h5("Median Discharge:"), verbatimTextOutput("medianDischarge")
                              ),
                              mainPanel(plotlyOutput("trendPlot"))
                            )
                          )
                 ),

                 tabPanel("Rolling Averages",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("rollingWindow", "Rolling Average (Days):", value = 1, min = 1, max = 365),
                              actionButton("applyRolling", "Apply")
                            ),
                            mainPanel(
                              plotOutput("rollingPlot"),
                              plotOutput("monthlyGraph") # New graph added below rolling average graph
                            )
                          )
                 ),

                 tabPanel("See Tables",
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("downloadData", "Download Current Data")
                            ),
                            mainPanel(DTOutput("dataTable"))
                          )
                 )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  dataset <- reactive({
    watershed_precip <- read.csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/dailyWatershedPrecip1956-2024.csv"))
    watershed_flow <- read.csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/HBEF_DailyStreamflow_1956-2023.csv"))

    watershed_precip <- watershed_precip |> mutate(watershed = str_replace(watershed, "W", ""))

    combined_data <- sqldf(
      "SELECT a.DATE as obs_date, a.watershed,
              A.precip as precip,
              b.Streamflow as streamflow, b.Flag as flag
       FROM watershed_precip as a
       LEFT OUTER JOIN watershed_flow as b
       ON a.watershed = B.WS AND a.DATE = b.DATE
       WHERE b.streamflow IS NOT NULL
       AND A.precip IS NOT NULL
       ORDER BY a.DATE"
    ) %>%
      mutate(obs_date = as.Date(obs_date),
             yr = year(obs_date),
             mo = month(obs_date),
             da = day(obs_date),
             wk = week(obs_date))

    return(combined_data)
  })

  filtered_dataset <- reactive({
    dataset() %>%
      filter(obs_date >= input$dateRange[1] & obs_date <= input$dateRange[2] &
               watershed %in% input$watersheds)
  })

  aggregated_data <- reactive({
    filtered_dataset() %>%
      group_by(obs_date) %>%
      summarize(total_precip = sum(precip, na.rm = TRUE)) %>%
      left_join(filtered_dataset(), by = "obs_date")
  })

  output$dateRangeText <- renderText({
    paste(input$dateRange[1], "to", input$dateRange[2])
  })

  output$recordPeriod <- renderText({
    paste(year(input$dateRange[1]), "to", year(input$dateRange[2]))
  })

  output$totalDays <- renderText({ n_distinct(filtered_dataset()$obs_date) })
  output$missingDays <- renderText({ sum(filtered_dataset()$flag == 1, na.rm = TRUE) })
  output$avgDischarge <- renderText({ mean(filtered_dataset()$streamflow, na.rm = TRUE) })
  output$medianDischarge <- renderText({ median(filtered_dataset()$streamflow, na.rm = TRUE) })

  output$trendPlot <- renderPlotly({
    df <- aggregated_data()

    if (nrow(df) == 0) {
      return(NULL)
    }

    p <- ggplot(df, aes(x = obs_date)) +
      geom_col(aes(y = total_precip), fill = 'lightblue', alpha = 0.6) +
      geom_line(aes(y = streamflow, color = watershed, group = watershed), size = 1) +
      scale_y_continuous(name = "Precipitation (mm/day)", sec.axis = sec_axis(~ ., name = "Streamflow (mm/day)")) +
      theme_minimal() +
      labs(title = "Precipitation & Flow Trend Analysis", x = "Date") +
      scale_color_brewer(palette = "Set1")
    if (input$addBaseflow) {
      p <- p + geom_line(aes(y = streamflow, color = ""), size = 1)
    }

    ggplotly(p)
  })

  rolling_data <- eventReactive(input$applyRolling, {
    aggregated_data() %>%
      group_by(watershed) %>%
      mutate(rolling_avg = zoo::rollmean(streamflow, k = input$rollingWindow, fill = NA, align = "right"))
  })

  output$rollingPlot <- renderPlot({
    df <- rolling_data()

    ggplot(df, aes(x = obs_date, y = rolling_avg, color = watershed)) +
      geom_line(size = 1) +
      labs(title = paste(input$rollingWindow, "-Day Rolling Average"), x = "Date", y = "Streamflow (mm/day)") +
      theme_minimal()
  })

  output$monthlyGraph <- renderPlot({
    colors <- c("Average Precipitation" = "steelblue", "Average Streamflow" = "orangered")

    df <- filtered_dataset() %>%
      group_by(mo) %>%
      summarise(avgprecip = mean(precip, na.rm = TRUE), avgstreamflow = mean(streamflow, na.rm = TRUE))

    ggplot(df) +
      geom_col(aes(x = mo, y = avgprecip, fill = "Average Precipitation")) +
      geom_line(aes(x = mo, y = avgstreamflow, color = "Average Streamflow")) +
      theme_classic() +
      labs(title = "Average Precipitation and Average Streamflow by Month",
           y = "Average Flow and Precip (MM/Day)",
           x = "Month",
           color = "Legend") +
      theme(axis.title.y.left = element_text(hjust = 0),
            legend.position = "bottom",
            legend.justification = c(0.25, 0.5),
            legend.title = element_blank()) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  })

  output$dataTable <- renderDT({
    datatable(filtered_dataset(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
