library(tidyverse)
library(readr)
library(lubridate)
library(reshape2)
library(zoo)

studentLoan <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

#Change to quarterly data
studentLoan$year <-  paste("20", studentLoan$year, sep = "")
quarterlyLoan<- studentLoan %>% unite(Quarterly, year, quarter, sep = "-") 
quarterlyLoan$Quarterly <- as.yearqtr(quarterlyLoan$Quarterly) 
quarterlyLoan$Quarterly <- lubridate::yq(quarterlyLoan$Quarterly) 

#analysis
Grow <- quarterlyLoan %>% group_by(Quarterly) %>% 
  summarise(startingLoan = sum(starting, na.rm = TRUE), 
            totalPaid = sum(total, na.rm = TRUE)) %>% 
  mutate(remainingDebt = startingLoan - totalPaid,
         paidPrecentage = totalPaid/ startingLoan*100) %>%
  mutate(remainingLoan = 100 - paidPrecentage, 
         paidGrowth = (totalPaid/lead(totalPaid) - 1) * 100)

mean(Grow$paidPrecentage)

#preparation
loanGrowth <- quarterlyLoan %>% group_by(Quarterly) %>% 
  summarise(startingLoan = sum(starting, na.rm = TRUE), 
            totalPaid = sum(total, na.rm = TRUE))  %>%
  melt(id = "Quarterly")

  
#visualisation   
plot <- ggplot(loanGrowth, aes(x = Quarterly, y =value, fill = variable)) +
  geom_area(alpha = 0.80) + geom_label(
    label="Only 2.8% loan paid on average", hjust = 0.79, vjust = 0.8,
    x= as.Date("2018-06-01"),
    y= 12000000000,
    label.size = 0.35,
    color = "black",
    fill= "white") + scale_y_continuous(labels = paste0("$", c(0, 30, 60, 90, 120)),
                                        name = "(in Billion)") + 
labs(title = "'A Debt Crisis?'",                
       subtitle = "A growing student loan in US from 2016 to 2018",
     caption = "https://github.com/sbfikri                                                                                          Source: US Department of Education - Alex Albright - #Tidytuesday") +
  theme(text = element_text(size=12, family = "Georgia"), 
        panel.background = element_rect(fill = "#f9eee4"),
        plot.background = element_rect(fill = "#f9eee4"),
        legend.background = element_rect(fill = "#f9eee4"),
        panel.grid.major.y = element_line(color="grey80"),
        line = element_blank(),
        plot.title = element_text(face = "bold", size = 17,
                                  margin = margin(10, 0, 5, 0)),
        plot.subtitle = element_text(face = "italic",  size = 13,
                                     color = "black",
                                     margin = margin(0, 0, 45, 0)),
        plot.caption = element_text(face = "italic", 
                                    hjust = 0.2,
                                    vjust = 0,
                                    size = 8),
        axis.ticks.length = unit(0, "cm"),
        axis.title.x = element_blank(),
        legend.key = element_rect(colour = "#f9eee4", size = 0), 
        legend.text = element_text(size = 10),
        legend.justification = "top")  +
  scale_fill_discrete(name = "", labels = c("Outstanding Loan", "Loan Paid"))

ggsave("Student Loan.png", plot)

