suppressPackageStartupMessages(library('shiny'))
suppressPackageStartupMessages(library('sqldf'))
suppressPackageStartupMessages(library('tidyverse'))
suppressPackageStartupMessages(library('shinythemes'))
suppressPackageStartupMessages(library('DT'))
suppressPackageStartupMessages(library('plotly'))
suppressPackageStartupMessages(library('zoo'))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("grwat"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library('xts'))
suppressPackageStartupMessages(library('hrbrthemes')) 
suppressPackageStartupMessages(library('socviz'))
suppressPackageStartupMessages(library('geofacet'))
suppressPackageStartupMessages(library('usmap'))
suppressPackageStartupMessages(library('socviz'))
suppressPackageStartupMessages(library('ggmap'))
suppressPackageStartupMessages(library('cowplot'))
suppressPackageStartupMessages(library('gridExtra'))
suppressPackageStartupMessages(library('webshot2'))
suppressPackageStartupMessages(library('kableExtra'))
suppressPackageStartupMessages(library('RColorBrewer'))


BaseflowSeparation <- function(streamflow, filter_parameter=0.925, passes=3){
  suppressWarnings(Ends<-c(1,length(streamflow))*rep(1,(passes+1))) # Start and end values for the filter function
  suppressWarnings(AddToStart<-c(1,-1)*rep(1,passes))
  btP<-streamflow##Previous pass's baseflow approximation
  qft<-vector(length=length(streamflow))
  bt<-vector(length=length(streamflow))
  bt[1]<-if(streamflow[1]<quantile(streamflow,0.25)) streamflow[1] else mean(streamflow)/1.5
  ##Guess baseflow value in first time step.  
  for(j in 1:passes){
    for (i in (Ends[j]+AddToStart[j]):Ends[j+1]){
      if ((filter_parameter*bt[i-AddToStart[j]]+((1-filter_parameter)/2)*(btP[i]+btP[i-AddToStart[j]]))>btP[i]){
        bt[i]<-btP[i]
      } else bt[i]<-filter_parameter*bt[i-AddToStart[j]]+((1-filter_parameter)/2)*(btP[i]+btP[i-AddToStart[j]])
      qft[i]<-streamflow[i]-bt[i]
    }
    if (j<passes){
      btP<-bt
      bt[Ends[j+1]]<-if(streamflow[Ends[j+1]]<mean(btP))streamflow[Ends[j+1]]/1.2 else mean(btP)
      ##Refines the approximation of end values after the first pass
    }
  }
  f <- data.frame(bt,qft)
  return(f)
}

watershed_precip <- read_csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/dailyWatershedPrecip1956-2024.csv"))
watershed_flow <- read_csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/HBEF_DailyStreamflow_1956-2023.csv"))
snow_data <- read_csv(url("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/HBEF_snowcourse_1956-2024.csv"))
snow_info <- read_csv("https://raw.githubusercontent.com/jpgannon/EDS-2025-HydroExplore/refs/heads/main/snowcourse_info.csv")

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
)
combined_snow_data <- sqldf(
  "select a.DATE obs_date, b.watershed watershed, 
  a.winter winter,
  a.Site Site, a.snow_depth snow_depth,
  a.swe swe, a.frost_depth frost_depth, 
  a.frost_pct frost_pct
  from snow_data as a
  left outer join snow_info as b 
  on (
    a.Site = B.snowcourseID
  )
  order by a.DATE
  "
)

total_data <- sqldf(
  "SELECT a.obs_date as obs_date, a.watershed,
              a.precip as precip,
              a.Streamflow as streamflow, a.Flag as flag,
              b.winter winter, b.snow_depth snow_depth,
              b.swe swe, b.frost_depth frost_depth,
              b.frost_pct frost_pct, b.Site site
       FROM combined_data as a
       LEFT OUTER JOIN combined_snow_data as b
       ON (
       a.watershed = b.watershed AND a.obs_date = b.obs_date
       )
       WHERE a.streamflow IS NOT NULL
       AND a.precip IS NOT NULL
       ORDER BY a.obs_date"
)


total_data$baseflow <- BaseflowSeparation(total_data$streamflow)$bt


total_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(total_data$obs_date)),
                                                month(as.Date(total_data$obs_date)),
                                                day(as.Date(total_data$obs_date)),
                                                week(as.Date(total_data$obs_date)))
monthly_data <- total_data |> group_by(watershed,yr,mo) |> 
  summarise(total_precip = sum(precip,na.rm = TRUE), 
            total_streamflow = sum(streamflow,na.rm = TRUE), 
            precip_divided_streamflow = (sum(precip,na.rm = TRUE)/sum(streamflow,na.rm = TRUE)),
            obs_date = first(obs_date),
            avg_snow_depth = mean(snow_depth,, na.rm = TRUE))
weekly_data <- total_data |> group_by(watershed,yr,wk,mo) |> 
  summarise(total_precip = sum(precip,na.rm = TRUE), 
            total_streamflow = sum(streamflow,na.rm = TRUE), 
            precip_divided_streamflow = (sum(precip,na.rm = TRUE)/sum(streamflow,na.rm = TRUE)),
            obs_date = first(obs_date),
            avg_snow_depth = mean(snow_depth,, na.rm = TRUE))


ui <- navbarPage("Hubbard Brook Watershed Data Analysis", theme = shinytheme("cerulean"),
                 tabPanel("Trend Analysis",
                          fluidPage(
                            titlePanel(h3("Hubbard Brook Experimental Forest: Watershed Precipitation and Flow Trend Analysis", align = "center")),
                            sidebarLayout(
                              sidebarPanel(
                                verbatimTextOutput(
                                  "plotlyinfo2"
                                ),
                                tags$head(tags$style("#rolling_avg_info{color:black; font-size:10px; font-style:italic; 
overflow-y:scroll; background: ghostwhite;}")),
                                checkboxGroupInput("watersheds", "Choose Watersheds (1-9):", choices = as.character(1:9), selected = c("1")),
                                dateRangeInput("dateRange", "Select Date Range:", start = "1956-01-01", end = "2023-12-31",
                                               min = "1956-01-01", max = "2023-12-31"),
                                checkboxInput("addBaseflow", "Add Baseflow Line", value = FALSE),
                                hr(),
                                h4("Selected Data Range:"), verbatimTextOutput("dateRangeText"),
                                h4("Period of Record:"), verbatimTextOutput("recordPeriod"),
                                h5("Total Days:"), verbatimTextOutput("totalDays"),
                                h5("Total Flagged Days:"), verbatimTextOutput("missingDays"),
                                h5("Average Discharge:"), verbatimTextOutput("avgDischarge"),
                                h5("Median Discharge:"), verbatimTextOutput("medianDischarge")
                              ),
                              mainPanel(
                                plotOutput("precipplot", width = "100%", height = "200px"), 
                                plotOutput("trendPlot"),
                                sliderInput("zoom_trend", "Trend and Precip Plot::", 
                                            min = as.Date("1956-01-01"), max = as.Date("2023-12-31"), 
                                            value = c(as.Date("1956-01-01"), as.Date("2023-12-31")),
                                            timeFormat = "%Y-%m-%d", width = "100%")
                              )
                            )
                          )
                 ),
                 tabPanel("Monthly Analysis",
                          fluidPage( # Ensures full-page layout
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("time_period", "Select Time Period:",
                                             choices = list("Monthly" = "Monthly", "Weekly" = "Weekly"),
                                             selected = "Monthly", inline = TRUE),
                                checkboxInput("addprecip", "Add Precip Date", value = FALSE),
                                checkboxInput("addstreamflow", "Add Streamflow Data", value = FALSE),
                                checkboxInput("addsnow", "Add Snow Levels Data", value = FALSE),
                                checkboxInput("addprecipdischarge", "Add P/Q Data", value = FALSE),
                                checkboxInput("addtrendline", "Add Precip & Streamflow Trendline", value = FALSE),
                                numericInput("ci_level", "Confidence Interval Level (Between 0 and 1)", value = 0.95, min = 0, max = 1, step = 0.01),
                                selectInput("trend_var", "Select Trend Variable", choices = c("Precip" = "total_precip", 
                                                                                              "Streamflow" = "total_streamflow", 
                                                                                              "Snow Depth"= "avg_snow_depth", 
                                                                                              "Precipitation/Streamflow" = "precip_divided_streamflow")),
                                selectInput("single_watershed", "Select One Watershed:", choices = as.character(1:9), selected = "1"),  # NEW
                                sliderInput("zoom_monthly", "Select Date Range:",  # NEW
                                            min = as.Date("1956-01-01"), max = as.Date("2023-12-31"), 
                                            value = c(as.Date("1956-01-01"), as.Date("2023-12-31")),
                                            timeFormat = "%Y-%m-%d", width = "100%"),
                                verbatimTextOutput("model_stats")
                              ),
                              mainPanel(
                                fluidRow(
                                  column(12, plotOutput("monthly_summary", height = "90vh")) # Full width & large height
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Rolling Averages",
                          sidebarLayout(
                            sidebarPanel(
                              verbatimTextOutput(
                                "plotlyinfo"
                              ),
                              tags$head(tags$style("#plotlyinfo{color:black; font-size:8px; font-style:italic; 
overflow-y:scroll; 50px; background: ghostwhite;}")),
                              numericInput("rollingWindow", "Rolling Average (Days):", value = 1, min = 1, max = 365),
                              actionButton("applyRolling", "Apply"),
                              verbatimTextOutput(
                                "rolling_avg_info"
                              ),
                              tags$head(tags$style("#rolling_avg_info{color:black; font-size:10px; font-style:italic; 
overflow-y:scroll; background: ghostwhite;}")),
                            ),
                            mainPanel(
                              plotOutput("rollingPlot"),
                              sliderInput("zoom_rolling", "Zoom Rolling Average Plot:", 
                                          min = as.Date("1956-01-01"), max = as.Date("2023-12-31"), 
                                          value = c(as.Date("1956-01-01"), as.Date("2023-12-31")),
                                          timeFormat = "%Y-%m-%d", width = "100%"),
                              plotOutput("monthlyGraph") # New graph added below rolling average graph
                            )
                          )
                 ),
                 
                 tabPanel("See Tables",
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("downloadData", "Download Current Data")),
                            mainPanel(DTOutput("dataTable"))
                          )
                 ),
                 
                 tabPanel("User Guide",
                          fluidPage(
                            h1("Welcome to the Hubbard Brook Watershed Data Analysis App!"),
                            p("This tool provides interactive insights into streamflow data collected from the Hubbard Brook Experimental Forest. Use the different tabs to explore patterns, trends, and seasonal variations in streamflow. 
                              The functionality and purpose of each tab are explained below."),
                            
                            h2("Trend Analysis"),
                            p("This tab allows you to examine long-term streamflow trends and detect patterns over time.
                              You will notice two plots on this tab: one for precipitation per month for each year at the top, 
                              and one for precipitation and streamflow trend at the bottom.
                              On the sidebar on the left of the screen, you can select multiple watersheds, change the date range,
                              add a baseflow line to the streamflow plot, and view statistics for the selected date range.
                              The plots will automatically update to apply any changes you make to the number of watersheds or the date range.
                              Alternatively, you can use the slider underneath the streamflow trend plot to adjust the date range."),
                            
                            h2("Monthly Analysis"), 
                            p("The Monthly and Weekly Analysis tab explores seasonal variations in streamflow and compare month-to-month changes.
                              This tab is particularly useful for looking at specific months and better understand the trends involved,
                              allowing you to see how the values have changed over the years. On the sidebar, you can select whether you want to look at 
                              months or weeks, and add lines for precipitation, streamflow, snow levels, and P/Q. Please note that you must select
                              a line in order for the plots to appear. Also important to note is that unlike the Trend Analysis tab, 
                              you can only view the data for one watershed at a time. The date range can be adjusted using the slider at the bottom of the sidebar."),
                            
                            h2("Rolling Averages"),
                            p("In the Rolling Averages tab, you can see whether streamflow values have changed over time.
                              The Rolling Averages plot summarizes how much values have changed using different time periods.
                              You can view up to a 364-day rolling average, in which case each year would be a single point.
                              It is recommended to use smaller timescales, such as 30 or 90 days, as these work better for plotting rolling averages.
                              To enter the number of days, you can simply type in the number of days in the input box, or you can use the arrows on the right side of the input box.
                              The plot will automatically update as you change the number of days, but you can also use the Apply button to confirm your input. 
                              Please also note the message below the Apply button for selecting which watersheds you want to be plotted,
                              the watersheds must be selected in the Trend Analysis tab. 
                              Below the Rolling Averages plot, you will also notice an Average Precipitation and Streamflow by Month graph.
                              This graph is an easy way to see how each month differs in its average daily streamflow and precipitation and 
                              serves as a useful reference when comparing to the Rolling Averages plot."),
                            
                            h2("See Tables"), 
                            p("Here you can examine and download the data that is used in the app. 
                              Important to note is that the dataset matches the selections made in the other tabs, so the watersheds and date ranges that were selected
                              in the Trend Analysis tab will show up here. This allows you to use the data generated from this app for your own purposes, 
                              Hubbard Brook doesn’t offer a combined precipitation, snowfall, and streamflow dataset for example, so this is an easy way to get this data.")
                          ),
                          
                 )
)







server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2)
  rollingWindowVal <- reactiveVal(1)
  observe({
    req(input$rollingWindow)  # Ensure input exists
    rollingWindowVal(input$rollingWindow)
  })
  filtered_dataset <- reactive({
    total_data %>%
      filter(obs_date >= input$dateRange[1] & obs_date <= input$dateRange[2] &
               watershed %in% input$watersheds)
  })
  
  aggregated_data <- reactive({
    filtered_dataset() %>%
      group_by(obs_date) %>%
      summarize(total_precip = sum(precip, na.rm = TRUE), total_flow = sum(streamflow,na.rm=TRUE)) %>%
      left_join(filtered_dataset(), by = "obs_date")
  })
  
  output$dateRangeText <- renderText({
    paste(input$dateRange[1], "to", input$dateRange[2])
  })
  
  output$recordPeriod <- renderText({
    paste(year(input$dateRange[1]), "to", year(input$dateRange[2]))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Aggregated_data", ".csv", sep = "")
    },
    content = function(file){
      write.csv(aggregated_data(),file, row.names = FALSE)
    }
  )
  
  output$totalDays <- renderText({ n_distinct(filtered_dataset()$obs_date) })
  output$missingDays <- renderText({ sum(filtered_dataset()$flag == 1, na.rm = TRUE) })
  output$avgDischarge <- renderText({ mean(filtered_dataset()$streamflow, na.rm = TRUE) })
  output$medianDischarge <- renderText({ median(filtered_dataset()$streamflow, na.rm = TRUE) })
  output$precipplot <- renderPlot({
    df <- aggregated_data() %>%
      filter(obs_date >= input$zoom_trend[1] & obs_date <= input$zoom_trend[2])
    
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df) +
      geom_line(aes(x = as.Date(obs_date), y = precip, color = watershed, group = watershed)) +
      scale_y_reverse(position = "right", limits = c(300, 0), 
                      breaks = c(0, 25, 50, 100), labels = c(0, 25, 50, 100), expand = c(0, 0)) +
      labs(y = "Precipitation (mm/day)", x = "") +
      theme_minimal()
  })
  output$trendPlot <- renderPlot({
    df <- aggregated_data() %>%
      filter(obs_date >= input$zoom_trend[1] & obs_date <= input$zoom_trend[2]) 
    
    if (nrow(df) == 0) return(NULL)
    
    watersheds <- unique(df$watershed)
    streamflow_colors <- setNames(brewer.pal(length(watersheds), "Set1"), watersheds)
    baseflow_colors <- setNames(brewer.pal(length(watersheds), "Set2"), paste(watersheds, "baseflow"))
    all_colors <- c(streamflow_colors, baseflow_colors)
    
    p <- ggplot(df, aes(x = as.Date(obs_date))) +
      geom_line(aes(y = streamflow, color = watershed, group = watershed), linewidth = 1) +
      scale_y_continuous(name = "Streamflow (mm/day)") +
      scale_x_date(date_labels = "%Y", date_breaks = "10 years") +  # Adjusting x-axis
      theme_classic() +
      labs(title = "Precipitation & Flow Trend Analysis", x = "Year") +
      scale_color_brewer(palette = "Set1")
    
    if (input$addBaseflow) {
      p <- p + geom_line(aes(y = baseflow, color = paste(watershed, "baseflow")), linewidth = 1)
    }
    p
  })
  
  rolling_avg_data <- reactive({
    df <- aggregated_data()  # Ensure this function exists and returns data
    req(df)  # Ensure df is available
    
    df <- df %>%
      group_by(watershed) %>%
      mutate(rolling_avg = zoo::rollmean(streamflow, k = rollingWindowVal(), fill = NA, align = "right")) %>%
      ungroup()
    
    # Apply zoom filter based on date range
    df <- df %>% filter(obs_date >= input$zoom_rolling[1], obs_date <= input$zoom_rolling[2])
    
    df
  })
  
  # Render the plot
  output$rollingPlot <- renderPlot({
    df <- rolling_avg_data()
    req(nrow(df) > 0)  # Ensure there's data to plot
    
    ggplot(df, aes(x = obs_date, y = rolling_avg, color = watershed)) +
      geom_line() +
      labs(title = paste0(rollingWindowVal(), "-Day Rolling Average"),
           x = "Date", y = "Streamflow (mm/day)") +
      theme_minimal()
  })
  
  output$monthlyGraph <- renderPlot({
    colors <- c("Average Precipitation per day" = "steelblue", "Average Streamflow per day" = "orangered")
    
    df <- filtered_dataset() %>%
      group_by(mo) %>%
      summarise(avgprecip = mean(precip, na.rm = TRUE), avgstreamflow = mean(streamflow, na.rm = TRUE))
    
    ggplot(df) +
      geom_col(aes(x = mo, y = avgprecip, fill = "Average Precipitation per day")) +
      geom_line(aes(x = mo, y = avgstreamflow, color = "Average Streamflow per day")) +
      theme_classic() +
      labs(title = "Average Precipitation and Average Streamflow by Month",
           y = "Average Flow and Precip (mm/Day)",
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
  output$rolling_avg_info <- renderPrint({
    cat("These Graphs use the same
date range and watersheds as 
selected on the trend analysis panel")})
  output$plotlyinfo <- renderPrint({
    cat("TO ZOOM: On the streamflow plot, 
click and drag and then double click. 
Double click again to zoom to full extent.")}
  )
  output$plotlyinfo2 <- renderPrint({
    cat("TO ZOOM: On the streamflow plot, 
click and drag and then double click. 
Double click again to zoom to full extent.")}
  )
  output$monthly_summary <- renderPlot({
    if (input$time_period == "Monthly") {
      
      df <- monthly_data |> 
        filter(obs_date >= input$zoom_monthly[1] & obs_date <= input$zoom_monthly[2] &  # Date filter
                 watershed == input$single_watershed) |> filter(total_streamflow != 0)  # Watershed filter
      
      p <- ggplot(df, aes(x = as.Date(obs_date))) +
        scale_y_continuous(name = "Streamflow (mm/day)", sec.axis = sec_axis(~ ., name = "Streamflow (mm/day)")) +
        theme_minimal() +
        labs(title = "Monthly Trend Analysis", x = "Date") +
        scale_x_date(date_labels = "%Y", date_breaks = "10 years") + 
        scale_color_brewer(palette = "Set1") +
        facet_wrap(~mo)
      
      if (input$addprecip) {
        p <- p + geom_point(aes(y = total_precip, color = "Precip"))
      }
      if (input$addstreamflow) {
        p <- p + geom_point(aes(y = total_streamflow, color = "Streamflow"))
      }
      if (input$addsnow) {
        p <- p + geom_point(aes(y = avg_snow_depth, color = "Snow Depth"))
      }
      if (input$addprecipdischarge) {
        p <- p + geom_point(aes(y = precip_divided_streamflow, color = "Precip/Streamflow"))
      }
      
      if (input$addtrendline) {
        trend_var <- input$trend_var  # Get selected variable
        
        # Apply LOESS smoothing
        trend_data <- df[[trend_var]]  # Select the variable (e.g., total_precip or total_streamflow)
        
        # Fit a LOESS model (smooth curve)
        loess_model <- loess(trend_data ~ as.numeric(df$obs_date))  # LOESS model
        
        # Get fitted values and residuals
        fitted_values <- loess_model$fitted
        residuals <- trend_data - fitted_values
        
        # Compute R-squared from residuals
        rss <- sum(residuals^2)  # Residual sum of squares
        tss <- sum((trend_data - mean(trend_data))^2)  # Total sum of squares
        r_squared <- 1 - (rss / tss)
        
        # To get the p-value, fit a linear model and get its p-value
        lm_model <- lm(trend_data ~ as.numeric(df$obs_date))  # Linear model for p-value
        p_value <- summary(lm_model)$coefficients[2, 4]  # p-value for the slope
        
        # Plot the LOESS smooth line
        p <- p + geom_smooth(method = "loess", aes(y = trend_data), color = "black", size = 1,
                             level = input$ci_level) 
        
        # Store R-squared and p-value in reactive output
        output$model_stats <- renderText({
          paste0("R² = ", round(r_squared, 3), "\nP-value = ", signif(p_value, 3))
        })
      } else {
        # Reset output when trendline is not selected
        output$model_stats <- renderText({ "" })
      }
      
      return(p)
    }
    
    # Repeat the same for weekly data if necessary
    if (input$time_period == "Weekly") {
      
      df <- weekly_data |> 
        filter(obs_date >= input$zoom_monthly[1] & obs_date <= input$zoom_monthly[2] & 
                 watershed == input$single_watershed) |> filter(total_streamflow != 0)
      
      p <- ggplot(df, aes(x = as.Date(obs_date))) +
        scale_y_continuous(name = "Streamflow (mm/day)", sec.axis = sec_axis(~ ., name = "Streamflow (mm/day)")) +
        theme_minimal() +
        labs(title = "Weekly Trend Analysis", x = "Date") +
        scale_x_date(date_labels = "%Y", date_breaks = "10 years") + 
        scale_color_brewer(palette = "Set1") +
        facet_wrap(~mo)
      
      if (input$addprecip) {
        p <- p + geom_point(aes(y = total_precip, color = "Precip"))
      }
      if (input$addstreamflow) {
        p <- p + geom_point(aes(y = total_streamflow, color = "Streamflow"))
      }
      if (input$addsnow) {
        p <- p + geom_point(aes(y = avg_snow_depth, color = "Snow Depth"))
      }
      if (input$addprecipdischarge) {
        p <- p + geom_point(aes(y = precip_divided_streamflow, color = "Precip/Streamflow"))
      }
      
      if (input$addtrendline) {
        trend_var <- input$trend_var  # Get selected variable
        
        # Apply LOESS smoothing
        trend_data <- df[[trend_var]]  # Select the variable (e.g., total_precip or total_streamflow)
        
        # Fit a LOESS model (smooth curve)
        loess_model <- loess(trend_data ~ as.numeric(df$obs_date))  # LOESS model
        
        # Get fitted values and residuals
        fitted_values <- loess_model$fitted
        residuals <- trend_data - fitted_values
        
        # Compute R-squared from residuals
        rss <- sum(residuals^2)  # Residual sum of squares
        tss <- sum((trend_data - mean(trend_data))^2)  # Total sum of squares
        r_squared <- 1 - (rss / tss)
        
        # To get the p-value, fit a linear model and get its p-value
        lm_model <- lm(trend_data ~ as.numeric(df$obs_date))  # Linear model for p-value
        p_value <- summary(lm_model)$coefficients[2, 4]  # p-value for the slope
        
        # Plot the LOESS smooth line
        p <- p + geom_smooth(method = "loess", aes(y = trend_data), color = "black", size = 1,
                             level = input$ci_level) 
        
        # Store R-squared and p-value in reactive output
        output$model_stats <- renderText({
          paste0("R² = ", round(r_squared, 3), "\nP-value = ", signif(p_value, 3))
        })
      } else {
        # Reset output when trendline is not selected
        output$model_stats <- renderText({ "" })
      }
      
      return(p)
    }
  })
  
}

shinyApp(ui, server)
