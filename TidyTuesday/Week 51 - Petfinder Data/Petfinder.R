library(tidyverse)

dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

#data
label <- dog_descriptions %>% count(breed_primary) %>%
  top_n(n = 30) %>% mutate(id = c(1:30))

number_of_bar <- nrow(label)
angle <-  90 - (360 * (label$id-1)/ number_of_bar) 
label$hjust<-ifelse(angle < -90, 1.1, -0.1)
label$vjust <- ifelse(angle < -90, 2.3, -1.3)
label$angle<-ifelse(angle < -90, angle + 180, angle)
label$breed_primary <- paste(label$breed_primary, " ", "(",
                             label$n, ")",  sep = "")

dog_descriptions %>% count(breed_primary) %>%
  top_n(n = 30) %>% ggplot(aes(x = breed_primary, y = n)) + 
  geom_bar(stat = "identity", fill = alpha("brown", 0.3), width = 1.9) + 
  theme_minimal(base_family = "serif") + ggtitle("Top") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")      
  ) + coord_polar(start = 0) +
  ylim(-6500, 8000) +
  geom_text(label, mapping =
            aes(x= breed_primary, y= n + 30, label= breed_primary, hjust=hjust, vjust = vjust), 
            color="black", fontface="bold", 
            alpha=0.6, size=2.5, 
            angle= label$angle, inherit.aes = FALSE) +
  geom_text( aes(x=30, y=-6500, 
                 label="Top 30 adoptable dogs\nin Petfinder.com"), 
             color="black", fontface = "bold", inherit.aes = FALSE) +
  geom_text( aes(x = 30, y = -4200, 
                 label = "*based on primary\nbreed assumed by\nshelters and rescuers "), 
             color="gray", fontface = "bold", inherit.aes = FALSE, size = 2.1)

ggsave("Top 30 Dogs.png", last_plot(), height = 8.2, limitsize = FALSE)
