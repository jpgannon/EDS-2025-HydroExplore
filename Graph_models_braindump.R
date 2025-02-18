library("stringr")
library("sqldf")
library("dplyr")
library("lubridate")
library("zoo")
library("dataRetrieval")
library("grwat")

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
  geom_area(aes(as.Date(obs_date), Qbase), fill = 'orangered') + 
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

