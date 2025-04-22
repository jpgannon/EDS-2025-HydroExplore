snow_data <-read.csv("knb-lter-hbr-4/HBEF_snowcourse_1956-2024.csv")
snow_info <- read.csv("knb-lter-hbr-4/snowcourse_info.csv")



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

combined_snow_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(combined_snow_data$obs_date)),
                                                   month(as.Date(combined_snow_data$obs_date)),
                                                   day(as.Date(combined_snow_data$obs_date)),
                                                   week(as.Date(combined_snow_data$obs_date)))

write.csv(combined_snow_data, "combined_snow_data.csv")
