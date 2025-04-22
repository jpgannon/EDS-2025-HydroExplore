library("stringr")
library("sqldf")
library("dplyr")
library("lubridate")
library("zoo")
library("dataRetrieval")
library("grwat")
library("ggplot2")
library('xts')
library('tidyverse')
library('hrbrthemes') 
library('socviz')
library('geofacet')
library('usmap')
library('socviz')
library('ggmap')
library('cowplot')
library('gridExtra')
library('webshot2')
library('kableExtra')

#Read in our combined data
combined_data <- read.csv("combined_precip_data.csv")
combined_data$baseflow <- BaseflowSeparation(combined_data$streamflow)

watershed_1 <- read.csv("1watershed.csv")
watershed_1$baseflow <- BaseflowSeparation(watershed_1$streamflow)

combined_data |> filter(watershed == 1) |> ggplot() +
  geom_area(aes(as.Date(obs_date), streamflow), fill = 'steelblue', color = 'black') +
  geom_area(aes(as.Date(obs_date), baseflow$qft), fill = 'orangered', color = 'black')

hdata = combined_data %>%
  mutate(lynehollick = gr_baseflow(streamflow, method = 'lynehollick', a = 0.9),
         boughton = gr_baseflow(streamflow, method = 'boughton', k = 0.9),
         jakeman = gr_baseflow(streamflow, method = 'jakeman', k = 0.9),
         maxwell = gr_baseflow(streamflow, method = 'maxwell', k = 0.9)) %>%
  pivot_longer(lynehollick:maxwell, names_to = 'Method', values_to = 'Qbase')
percentage_data <- hdata |>  mutate(percentage = ((Qbase/streamflow)*100))
percentage_data<- percentage_data |>
  mutate(season = case_when(mo %in% c(12, 1, 2) ~ "Winter",
                            mo %in% c(3, 4, 5) ~ "Spring",
                            mo %in% c(6, 7, 8) ~ "Summer",
                            TRUE ~ "Autumn"))


percentage_data |> group_by(Method) |> ggplot(aes(as.Date(x=obs_date), y = percentage, color = Method)) + geom_line() +theme_classic()



watershed_1 |> ggplot(aes(as.Date(x=obs_date), y = streamflow)) + geom_line() + theme_classic()

select_data <- percentage_data |> filter(Method == "lynehollick")

p1 <- select_data |> ggplot() +
  geom_area(aes(as.Date(obs_date), streamflow), fill = 'steelblue',color = 'black') +
  geom_area(aes(as.Date(obs_date), baseflow$bt), fill = 'orangered') + 
  scale_y_continuous(position = "left",
                     limits = c(0, 150),
                     expand = c(0,0)) + 
  labs(y = "Discharge [mm/d]",
       x = "High Flow LyneHollick Model") +
  theme_minimal() +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.25, 0.5),
        legend.title = element_blank())
p2 <- ggplot(select_data) +
  geom_line(aes(as.Date(obs_date), precip, color = "Precip")) +
  scale_y_reverse(position = "right",
                  limits = c(300,0),
                  breaks = c(0,25,50,100),
                  labels = c(0,25,50,100),
                  expand = c(0,0)) +
  scale_color_manual(values = c("sienna1")) +
  labs(y = "Precipitation [mm/day]", x = "") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank())
aligned_plots <- align_plots(p1, p2, align = "hv", axis = "tblr")
out <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
out





select_data |> ggplot(aes(x=as.Date(obs_date),y=precip)) +geom_line()


ggplot(select_data)+
  geom_line(aes(as.Date(obs_date), precip, color = "Precip")) + geom_line(aes(as.Date(obs_date),y=baseflow$bt, color = "Baseflow"))




select_data |> group_by(obs_date) |> summarise(precip = sum(precip), streamflow = sum(streamflow), baseflow = sum(baseflow$bt)) |> ggplot() + 
  geom_col(aes(x=as.Date(obs_date), y= precip, color ="Precip")) + 
  geom_line(aes(x=as.Date(obs_date), y =baseflow, color ="Baseflow")) + 
  theme_classic() +
  labs(title = "Precip Compared to Baseflow", x = "Date", y = "mm/day")

watersheds <- c(1)

filtered_data <- combined_data |> filter(watershed %in% watersheds)


flow_xts <- xts(combined_data$streamflow,
                       order.by=as.Date(combined_data$obs_date))
rollingavg <- rollmean(flow_xts, k = 7, aling = "right", fill = NA)


rolling_averages <- select_data |> 
  group_by(obs_date,watershed) |> 
  transform(avg = rollmeanr(cbind(streamflow, precip), 3, fill = NA)) %>%
  ungroup

#Month Graphs:
colors <- c("Average Precipitation" = "steelblue", "Average Streamflow" = "orangered")
select_data |> group_by(mo) |>  summarise(avgprecip = mean(precip), avgstreamflow = mean(streamflow)) |> 
  ggplot() + 
  geom_col(aes(x=mo,y = avgprecip, fill = "Average Precipitation")) +
  geom_line(aes(x = mo, y = avgstreamflow, color = "Average Streamflow")) +
  theme_classic()+
  labs(title = "Average Precipitation and Average Streamflow by Month", 
       y = "Average Flow and Precip (MM/Day)", 
       x = "Month",
       color = "legend") +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.25, 0.5),
        legend.title = element_blank())+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) 


#Rolling Averages:

rolling_averages |> ggplot() + 
  geom_col(aes(x=as.Date(obs_date), y = avg.precip, fill = "Average Precipitation"))+
  geom_line(aes(x=as.Date(obs_date), y = avg.streamflow, color = 'Average Streamflow'))+
  theme_classic()+
  labs(title = "Average Precipitation and Average Streamflow by Month", 
       y = "Rolling Average Flow and Precip (MM/Day)", 
       x = "Date",
       color = "legend") +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.25, 0.5),
        legend.title = element_blank())+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) 
  

select_data |> group_by(watershed) |> ggplot() +
  geom_line(aes(x=obs_date, y = precip, color = watershed))
  
  


