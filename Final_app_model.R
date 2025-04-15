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
suppressPackageStartupMessages(library('forecast'))
suppressPackageStartupMessages(library('nlme'))
suppressPackageStartupMessages(library('mgcv'))





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

watershed_precip <- read_csv("dailyWatershedPrecip1956-2024.csv")
watershed_flow <- read_csv("HBEF_DailyStreamflow_1956-2023.csv")
snow_data <- read_csv("HBEF_snowcourse_1956-2024.csv")
snow_info <- read_csv("snowcourse_info.csv")

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
  summarise(total_precip = mean(precip,na.rm = TRUE), 
            total_streamflow = mean(streamflow,na.rm = TRUE), 
            precip_divided_streamflow = (sum(precip,na.rm = TRUE)/sum(streamflow,na.rm = TRUE)),
            obs_date = first(obs_date),
            avg_snow_depth = mean(snow_depth,, na.rm = TRUE))
weekly_data <- total_data |> group_by(watershed,yr,wk,mo) |> 
  summarise(total_precip = mean(precip,na.rm = TRUE), 
            total_streamflow = mean(streamflow,na.rm = TRUE), 
            precip_divided_streamflow = (sum(precip,na.rm = TRUE)/sum(streamflow,na.rm = TRUE)),
            obs_date = first(obs_date),
            avg_snow_depth = mean(snow_depth,, na.rm = TRUE))

heatmap <- total_data |> mutate(year = as.numeric(yr),
                                day_of_year = as.numeric(mo) * 30 + as.numeric(da))

water_year_days <- c(
  seq.Date(as.Date("2000-10-01"), as.Date("2000-12-31"), by = "day"),
  seq.Date(as.Date("2000-01-01"), as.Date("2000-09-30"), by = "day")
)
year_data <- addWaterYear(total_data)

year_data <- year_data %>%
  group_by(yr, da, mo, watershed) %>%
  mutate(precip_divided_by_discharge = sum(precip, na.rm = TRUE) / sum(streamflow, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    obs_date = as.Date(obs_date),      # ensure obs_date is Date
    water_doy = case_when(
      month(obs_date) >= 10 ~ yday(obs_date) - yday(as.Date(paste0(year(obs_date), "-10-01"))) + 1,
      TRUE ~ yday(obs_date) + (365 - yday(as.Date(paste0(year(obs_date) - 1, "-09-30"))))
    )
  )



ui <- navbarPage("Hubbard Brook Watershed Data Analysis", theme = shinytheme("cerulean"),
                 tabPanel("Trend Analysis",
                          fluidPage(
                            titlePanel(h3("Hubbard Brook Experimental Forest: Watershed Precipitation and Flow Trend Analysis", align = "center")),
                            sidebarLayout(
                              sidebarPanel(
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
                                sliderInput("zoom_trend", "Trend and Precip Plot:", 
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
                                checkboxInput("addprecip", "Add Precip Data", value = FALSE),
                                checkboxInput("addstreamflow", "Add Streamflow Data", value = FALSE),
                                checkboxInput("addsnow", "Add Snow Levels Data", value = FALSE),
                                checkboxInput("addprecipdischarge", "Add P/Q Data", value = FALSE),
                                checkboxInput("addtrendline", "Add Trendline", value = FALSE),
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
                 tabPanel("Heatmap Analysis",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("add_var", "Select Heatmap Variable", choices = c("Add Precip Data" = "precip", 
                                                                                              "Add Streamflow Data" = "streamflow")),
                                selectInput("single_watershed", "Select One Watershed:", choices = as.character(1:9), selected = "1"),
                                sliderInput("heatmap_rolling", "Select Data Range:",
                                            min = as.Date("1956-01-01"), max = as.Date("2023-12-31"),
                                            value = c(as.Date("1956-01-01"), as.Date("2023-12-31")),
                                            timeFormat = "%Y-%m-%d", width = "100%"),
                                verbatimTextOutput("model_stats")
                              ),
                              mainPanel(
                                fluidRow(
                                  column(12, plotOutput("heatmap", height = "90vh"))
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Yearly Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("zoom_wateryear", "Select Water‑Year Range:", min = min(year_data$waterYear), max = max(year_data$waterYear), value = c(min(year_data$waterYear), max(year_data$waterYear)), step = 1, sep = "", width = "100%"),
                              dateRangeInput(
                                inputId = "season_range",
                                label   = "Select Seasonal Range (Within Each Water‑Year):",
                                start   = as.Date("2000-10-01"),   # Oct 1 of dummy year
                                end     = as.Date("2001-09-30"),   # Sep 30 of next year
                                min     = as.Date("2000-10-01"),
                                max     = as.Date("2001-09-30"),
                                format  = "M dd",                  # show “Oct 01”, “Jan 15”, etc.
                                width   = "100%"
                              ),
                              selectInput("yearlysingle_watershed", "Select One Watershed:", choices = sort(unique(year_data$watershed))),
                              checkboxInput("addyearlyprecip", "Add Precip Data", FALSE),
                              checkboxInput("addyearlystreamflow", "Add Streamflow Data", FALSE),
                              checkboxInput("addyearlysnow", "Add Snow Levels Data", FALSE),
                              checkboxInput("addyearlyprecipdischarge", "Add P/Q Data", FALSE),
                              # checkboxInput("addyearlytrendline", "Add Trendline", FALSE),
                              # selectInput("yearlytrend_var", "Select Trend Variable:", choices = c("Precip" = "precip", "Streamflow" = "streamflow", "Snow Depth" = "snow_depth", "Precipitation/Streamflow" = "precip_divided_by_discharge")),
                              verbatimTextOutput("year_model_stats")
                            ),
                            mainPanel(
                              plotOutput(
                                "yearly_summary",height = "800px")
                            )
                          )
                 ),
#                  tabPanel("Rolling Averages",
#                           sidebarLayout(
#                             sidebarPanel(
#                               verbatimTextOutput(
#                                 "plotlyinfo"
#                               ),
#                               tags$head(tags$style("#plotlyinfo{color:black; font-size:8px; font-style:italic; 
# overflow-y:scroll; 50px; background: ghostwhite;}")),
#                               numericInput("rollingWindow", "Rolling Average (Days):", value = 1, min = 1, max = 365),
#                               actionButton("applyRolling", "Apply"),
#                               verbatimTextOutput(
#                                 "rolling_avg_info"
#                               ),
#                               tags$head(tags$style("#rolling_avg_info{color:black; font-size:10px; font-style:italic; 
# overflow-y:scroll; background: ghostwhite;}")),
#                             ),
                 #            mainPanel(
                 #              plotOutput("rollingPlot"),
                 #              sliderInput("zoom_rolling", "Zoom Rolling Average Plot:", 
                 #                          min = as.Date("1956-01-01"), max = as.Date("2023-12-31"), 
                 #                          value = c(as.Date("1956-01-01"), as.Date("2023-12-31")),
                 #                          timeFormat = "%Y-%m-%d", width = "100%"),
                 #              plotOutput("monthlyGraph") # New graph added below rolling average graph
                 #            )
                 #          )
                 # ),
                 tabPanel("See Tables",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("download_table", "Select the Table to Download", choices = c("Total Data" = "total_data", 
                                                                                                        "Filtered Data" = "filtered_dataset", 
                                                                                                        "Aggregated_data"= "aggregated_data", 
                                                                                                        "Weekly Data" = "weekly_data",
                                                                                                        "Monthly Data" = "monthly_data")),
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
                            
                            #h2("Rolling Averages"),
                            #p("In the Rolling Averages tab, you can see whether streamflow values have changed over time.
                              #The Rolling Averages plot summarizes how much values have changed using different time periods.
                              #You can view up to a 364-day rolling average, in which case each year would be a single point.
                              #It is recommended to use smaller timescales, such as 30 or 90 days, as these work better for plotting rolling averages.
                              #To enter the number of days, you can simply type in the number of days in the input box, or you can use the arrows on the right side of the input box.
                              #The plot will automatically update as you change the number of days, but you can also use the Apply button to confirm your input. 
                              #Please also note the message below the Apply button for selecting which watersheds you want to be plotted,
                              #the watersheds must be selected in the Trend Analysis tab. 
                              #Below the Rolling Averages plot, you will also notice an Average Precipitation and Streamflow by Month graph.
                              #This graph is an easy way to see how each month differs in its average daily streamflow and precipitation and 
                              #serves as a useful reference when comparing to the Rolling Averages plot."),
                            
                            h2("Heatmap Analysis"),
                            p("The heatmap shows by day of year when the highflow events happen. You can select by watershed and adjust the date range with the slider. 
                              Darker areas on the heatmap indicate a high flow for that day and lighter areas indicate lower flows."),
                            
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
      table <- input$download_table
      paste(table, ".csv", sep = "")
    },
    content = function(file) {
      table <- input$download_table
      if (table == "total_data") {
        write.csv(total_data, file, row.names = FALSE)
      } else if (table == "aggregated_data") {
        write.csv(aggregated_data(), file, row.names = FALSE)
      } else if (table == "weekly_data") {
        write.csv(weekly_data, file, row.names = FALSE)
      } else if (table == "filtered_dataset") {
        write.csv(filtered_dataset(), file, row.names = FALSE)
      } else if (table == "monthly_data") {
        write.csv(monthly_data, file, row.names = FALSE)
      } else {
        stop("Invalid table selection")
      }
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
      scale_y_reverse(position = "right", limits = c(200, 0), 
                      breaks = c(0, 25, 50, 100,150), labels = c(0, 25, 50, 100,150), expand = c(0, 0)) +
      labs(y = "Precipitation (mm/day)", x = "") +
      theme_minimal()
  })
  output$trendPlot <- renderPlot({
    df <- aggregated_data() %>%
      filter(obs_date >= input$zoom_trend[1] & obs_date <= input$zoom_trend[2]) 
    
    if (nrow(df) == 0) return(NULL)
    
    watersheds <- unique(df$watershed)
    streamflow_colors <- setNames(brewer.pal(length(watersheds), "Set1"), watersheds)
    baseflow_colors <- setNames(brewer.pal(length(watersheds), "Set3"), paste(watersheds, "baseflow"))
    all_colors <- c(streamflow_colors, baseflow_colors)
    
    p <- ggplot(df, aes(x = as.Date(obs_date))) +
      geom_line(aes(y = streamflow, color = watershed, group = watershed), linewidth = 1) +
      scale_y_continuous(name = "Streamflow (mm/day)") +
      scale_x_date(date_labels = "%Y", date_breaks = "10 years") +  # Adjusting x-axis
      theme_classic() +
      labs(title = "Precipitation & Flow Trend Analysis", x = "Year") +
      scale_color_manual(values = all_colors)
    
    if (input$addBaseflow) {
      p <- p + geom_line(aes(y = baseflow, color = paste(watershed, "baseflow")), linewidth = 1)
    }
    p
  })
  
  rolling_avg_data <- reactive({
    df <- aggregated_data()  # Ensure this function exists and returns data
    req(df)  # Ensure df is available
    # Apply zoom filter based on date range
    
    df <- df %>% filter(as.Date(obs_date) >= as.Date(input$zoom_rolling[1]), 
                        as.Date(obs_date) <= as.Date(input$zoom_rolling[2]))
    
    df <- df %>%
      group_by(watershed) %>%
      mutate(rolling_avg = zoo::rollmean(streamflow, k = rollingWindowVal(), fill = NA, align = "right")) %>%
      ungroup()
    
    df
  })
  
  # Render the plot
  output$rollingPlot <- renderPlot({
    df <- rolling_avg_data()
    req(nrow(df) > 0)  # Ensure there's data to plot
    df$obs_date <- as.Date(df$obs_date)
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
    table <- input$download_table
    if (table == "total_data") {
      datatable(total_data, options = list(pageLength = 10))
    }
    else if (table == "aggregated_data") {
      datatable(aggregated_data(), options = list(pageLength = 10))
    }
    else if (table == "weekly_data") {
      datatable(weekly_data, options = list(pageLength = 10))
    }
    else if (table == "filtered_dataset") {
      datatable(filtered_dataset(), options = list(pageLength = 10))
    }
    else if (table == "monthly_data") {
      datatable(monthly_data, options = list(pageLength = 10))
    }
  })
  
  output$heatmap <- renderPlot({
    df <- heatmap %>%
      filter(obs_date >= input$heatmap_rolling[1] & obs_date <= input$heatmap_rolling[2]) %>%
      filter(watershed == input$single_watershed)
    
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      mutate(year = as.numeric(yr),
             day_of_year = as.numeric(mo) * 30 + as.numeric(da))
    
    if (input$add_var == "precip") {
      data_to_plot <- df %>%
        mutate(value = precip) %>%
        group_by(year, day_of_year) %>%
        summarise(total_value = sum(value, na.rm = TRUE))
    } 
    else if (input$add_var == "streamflow") {
      data_to_plot <- df %>%
        mutate(value = streamflow) %>%
        group_by(year, day_of_year) %>%
        summarise(total_value = sum(value, na.rm = TRUE))
    } 
    else if (input$add_var == "snow") {
      data_to_plot <- df %>%
        mutate(value = snow_depth) %>%
        group_by(year, day_of_year) %>%
        summarise(total_value = sum(value, na.rm = TRUE))
    } else {
      return(NULL)
    }
    
    if (nrow(data_to_plot) == 0) return(NULL)
    
    # Rank flow events
    ranked_df <- data_to_plot %>%
      mutate(rank = rank(-total_value)) %>%
      mutate(category = case_when(
        rank <= 10 ~ "Top 10",
        rank <= 50 ~ "Top 50",
        TRUE ~ "Other"
      ))
    
    ggplot(ranked_df, aes(x = day_of_year, y = year, fill = category, text = paste("Date:", year, "- Day", day_of_year))) +
      geom_tile() +
      scale_fill_manual(values = c("Top 10" = "red", "Top 50" = "orange", "Other" = "lightblue")) +
      labs(title = paste("Watershed", input$single_watershed, "High Flow Events Heatmap"),
           x = "Day of Year", y = "Year", fill = "Event Rank") +
      theme_minimal()
  })
  
  rolling_avg_data <- reactive({
    df <- aggregated_data()  # Ensure this function exists and returns data
    req(df)  # Ensure df is available
    # Apply zoom filter based on date range
    
    df <- df %>% filter(as.Date(obs_date) >= as.Date(input$zoom_rolling[1]), 
                        as.Date(obs_date) <= as.Date(input$zoom_rolling[2]))
    
    df <- df %>%
      group_by(watershed) %>%
      mutate(rolling_avg = zoo::rollmean(streamflow, k = rollingWindowVal(), fill = NA, align = "right")) %>%
      ungroup()
    
    df
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
    
    df <- monthly_data |> 
      filter(obs_date >= input$zoom_monthly[1] & obs_date <= input$zoom_monthly[2] &
               watershed == input$single_watershed) |> filter(total_streamflow != 0)
    
    p <- ggplot(df, aes(x = as.Date(obs_date))) +
      scale_y_continuous(name = "(mm/day)", sec.axis = sec_axis(~ ., name = "(mm/day)")) +
      theme_classic() +
      labs(title = "Monthly Trend Analysis", x = "Date") +
      scale_x_date(date_labels = "%Y", date_breaks = "10 years") + 
      scale_color_brewer(palette = "Set1") +
      facet_wrap(~mo, scales = "free_y")  # Allow different y-axis scales for each facet
    
    # Add precipitation, streamflow, or snow data based on user inputs
    if (input$addprecip) {
      p <- p + geom_point(aes(y = total_precip, color = "Precip"))
    }
    if (input$addstreamflow) {
      p <- p + geom_point(aes(y = total_streamflow, color = "Streamflow"))
    }
    if (input$addsnow) {
      # Add snow depth data, but handle missing data gracefully
      p <- p + geom_point(aes(y = avg_snow_depth, color = "Snow Depth"), na.rm = TRUE)
    }
    if (input$addprecipdischarge) {
      p <- p + geom_point(aes(y = precip_divided_streamflow, color = "Precip/Streamflow"))
    }
    
    if (input$addtrendline) {
      trend_var <- input$trend_var
      # Ensure obs_date is ordered and convert it to a time series object
      df <- df[order(df$obs_date), ]
      
      ts_data <- ts(df[[trend_var]], frequency = 12)  # Assuming monthly data
      exog_data <- as.numeric(df$obs_date)  # Convert date to numeric for ARIMAX
      
      # Fit an ARIMAX model (ARIMA with exogenous variables)
      arimax_model <- auto.arima(ts_data, xreg = exog_data)
      
      # Get fitted values from the ARIMAX model
      df$fitted_values <- fitted(arimax_model)
      
      # Calculate R² for the fit
      residuals <- df[[trend_var]] - df$fitted_values
      r_squared <- 1 - (sum(residuals^2) / sum((df[[trend_var]] - mean(df[[trend_var]]))^2))
      
      # Plot the ARIMAX model trendline
      p <- p + geom_line(aes(y = fitted_values, color = "Trendline"), size = 1, data = df)
      
      # Output model statistics (R²)
      output$model_stats <- renderText({
        paste0("R² = ", round(r_squared, 3))
      })
    } else {
      output$model_stats <- renderText({ "" })
    }
    
    # Dynamically adjust y-axis limits for snow depth if included
    if (input$addsnow || input$trend_var == "avg_snow_depth") {
      max_snow_by_month <- df %>%
        group_by(mo) %>%
        summarize(max_snow = max(avg_snow_depth, na.rm = TRUE))
      
      p <- p + facet_wrap(~mo, scales = "free_y") +
        expand_limits(y = max_snow_by_month$max_snow)
    }
    p
  })
  doy_range <- reactive({
    req(input$season_range)
    start <- input$season_range[1]
    end   <- input$season_range[2]
    
    start_doy <- if (month(start) >= 10) yday(start) - yday(as.Date(paste0(year(start), "-10-01"))) + 1
    else yday(start) + (365 - yday(as.Date(paste0(year(start) - 1, "-09-30"))))
    
    end_doy <- if (month(end) >= 10) yday(end) - yday(as.Date(paste0(year(end), "-10-01"))) + 1
    else yday(end) + (365 - yday(as.Date(paste0(year(end) - 1, "-09-30"))))
    
    c(start_doy, end_doy)
  })
  
  # 2) Filter by water‑year and obs_yday, with wrap logic
  filtered_year_data <- reactive({
    req(input$zoom_wateryear, input$yearlysingle_watershed)
    days <- doy_range()
    
    df <- year_data %>%
      filter(
        waterYear >= input$zoom_wateryear[1],
        waterYear <= input$zoom_wateryear[2],
        watershed  == input$yearlysingle_watershed,
        streamflow != 0
      ) %>%
      {
        if (days[1] <= days[2]) {
          filter(., water_doy >= days[1], water_doy <= days[2])
        } else {
          filter(., water_doy >= days[1] | water_doy <= days[2])
        }
      }
    
    cat("▶ Rows after filtering:", nrow(df), "\n")
    df
  })
  
  # 3) ARIMAX fit reactive (only runs when trendline is requested)
  # trend_results <- reactive({
  #   req(input$addyearlytrendline)
  #   df <- filtered_year_data()
  #   df <- df[order(df$obs_date), ]
  #   
  #   ts_data   <- ts(df[[input$yearlytrend_var]], frequency = 12)
  #   exog_data <- as.numeric(df$obs_date)
  #   
  #   model <- auto.arima(ts_data, xreg = exog_data)
  #   df$fitted_values <- fitted(model)
  #   
  #   residuals  <- df[[input$yearlytrend_var]] - df$fitted_values
  #   r2         <- 1 - sum(residuals^2) /
  #     sum((df[[input$yearlytrend_var]] - mean(df[[input$yearlytrend_var]]))^2)
  #   
  #   list(df = df, r_squared = r2)
  # })
  
  # 4) Render R² text
  output$year_model_stats <- renderText({
    req(input$addyearlytrendline)
    res <- trend_results()
    paste0("R² = ", round(res$r_squared, 3))
  })
  
  # 5) Render the main plot
  output$yearly_summary <- renderPlot({
    df <- filtered_year_data()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = water_doy)) +
      theme_classic() +
      labs(
        title = "Yearly Trend Analysis (By Day of Water Year)",
        x     = "Day of Water Year (1 = Oct 1)",
        y     = "(mm/day)"
      ) +
      scale_y_continuous(sec.axis = sec_axis(~ ., name = "(mm/day)")) +
      scale_color_brewer(palette = "Set1") +
      facet_wrap(~ waterYear, scales = "free_y")  # keep y free, x fixed for comparison
    
    # conditional layers
    if (isTRUE(input$addyearlyprecip)) {
      p <- p + geom_point(aes(y = precip, color = "Precip"), data = df)
    }
    if (isTRUE(input$addyearlystreamflow)) {
      p <- p + geom_point(aes(y = streamflow, color = "Streamflow"), data = df)
    }
    if (isTRUE(input$addyearlysnow)) {
      p <- p + geom_point(aes(y = snow_depth, color = "Snow Depth"),
                          na.rm = TRUE, data = df)
    }
    if (isTRUE(input$addyearlyprecipdischarge)) {
      p <- p + geom_point(aes(y = precip_divided_by_discharge,
                              color = "Precip/Streamflow"),
                          data = df)
    }
    # # add trendline if selected
    # if (isTRUE(input$addyearlytrendline)) {
    #   res <- trend_results()
    #   p <- p + geom_line(aes(x = water_doy, y = fitted_values, color = "Trendline"),
    #                      size = 1, data = res$df)
    # }
    
    p
  })
  
}

shinyApp(ui, server)

