library(stringr)
library(sqldf)
library(dplyr)
library("lubridate")
library("zoo")
library("dataRetrieval")
watershed_precip <-read.csv("knb-lter-hbr-4/dailyWatershedPrecip1956-2024.csv")
watershed_flow <- read.csv("knb-lter-hbr-5/HBEF_DailyStreamflow_1956-2023.csv")


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
  order by a.DATE
  "
)
combined_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(combined_data$obs_date)),
                                                 month(as.Date(combined_data$obs_date)),
                                                 day(as.Date(combined_data$obs_date)),
                                                 week(as.Date(combined_data$obs_date)))