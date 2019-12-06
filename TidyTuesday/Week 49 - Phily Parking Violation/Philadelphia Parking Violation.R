library(tidyverse)
library(lubridate)
library(readr)
library(reshape2)
library(gganimate)
library(gifski)
library(scales)


phillyTicket <- readr:: read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

averageHourly <- phillyTicket %>% select(issue_datetime, fine, issuing_agency) %>%
  mutate(hour = as.character(issue_datetime, "%I"),
         meridian = as.character(issue_datetime, "%p"),
         month = month(as.POSIXct(issue_datetime, "%v"))) %>%
  select(fine, hour, meridian, month) %>%
  group_by(month, hour, meridian,  month) %>%
  dplyr::summarise(fine = mean(fine)) %>%
  reshape2::dcast(month + hour ~ meridian)


#modeling
loessAM <- loess(AM ~ hour, averageHourly)
predictAM <- predict(loessAM)
predictAM <-  as.data.frame(predictAM)
averageHourly <- cbind(averageHourly, predictAM)

loessPM <- loess(PM ~ hour, averageHourly)
predictPM <- predict(loessPM)
predictPM <- as.data.frame(predictPM)
averageHourly <- cbind(averageHourly, predictPM)

averageHourly <- averageHourly %>% melt(id = c("month", 
              "hour", "predictPM",
              "predictAM"), value.name = "average fine",
       variable.name = "meridian") 


#visualisation

a <- averageHourly %>% ggplot() +
  geom_point(aes(x = hour, 
                 y = `average fine`,
                 color = meridian), size = 4, alpha = 1/2) +
  geom_line(aes(x = as.integer(hour),
                y = predictAM),
            linetype = "dashed", color = "#333366") +
  geom_line(aes(x = as.integer(hour),
                y = predictPM),
            linetype = "dashed", color = "#FF9933") +
  theme_light(base_family = 'Georgia') +
  theme(legend.title = element_blank(),
        legend.justification =  "top",
        plot.subtitle = element_text(face = 'italic', size = 10, color = "gray50"),
        plot.title = element_text(face = 'bold', size = 20),
        plot.caption = element_text(hjust = 1.12, color = "gray50", face = "bold")) +
  scale_y_continuous(labels = dollar, breaks = c(0, 50, 60)) +
  scale_colour_manual(values = c("#333366", "#FF9933")) +
  labs(x = "Time",
       y = "",
       title = "'Night and Day'",
       subtitle = "Average amount of Philadelphia's parking violation fines in  2017\nMonth :{closest_state}",
       caption = "Source: Philly Open Data | Phily Open Data | Graph by https://github.com/sbfikri") +
  transition_states(month, transition_length = 2, state_length = 0) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()


#save

b <- animate(a, 100, fps = 30,
        duration = 13,
        width = 982, 
        height = 571,
        renderer = gifski_renderer())

anim_save("Phily Parking Violation.gif", animation = b)
  
  
  
  



