install.packages("googlesheets4")
install.packages("ggrepel")

library(googlesheets4)
library(tidyverse)
library(ggrepel)
library(viridis)

rbl <- read_sheet("https://docs.google.com/spreadsheets/d/1PAw_zTJpZ7FbQwEIKIw8xc4cst7dCGXvPT0SNnUfK-8/edit#gid=0", sheet = "Game Log")


mj_life
life_country

playersofinterest <- c("Adams", "Upamecano", "Werner")

rbl %>% 
  filter(`Player Name` %in% playersofinterest) %>%
  # filter(`Player Name` == "Adams") %>% 
  ggplot(aes(x = `Game Time`, y = `Event Score`, label = `Event Type`, fill = `Game Situtaiton`)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_segment(aes(y = `Event Score`, yend = 0, xend = `Game Time`)) +
  geom_label_repel() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
  coord_cartesian(xlim = c(0, 100)) +
  theme(axis.line.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  facet_grid(`Player Name` ~ .) +
  scale_fill_viridis(discrete = T, begin = .35) 

ggsave("plots/demo-timeline-adams-rbl.png", heigh = 8, width = 8)
  
  

  
 
