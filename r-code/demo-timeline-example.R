# install.packages("googlesheets4")
# install.packages("ggrepel")
# install.packages("directlabels")

library(googlesheets4)
library(tidyverse)
library(ggrepel)
library(viridis)


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
  
  
rbl %>% 
  filter(`Player Name` == "Adams") %>% 
  mutate(`Net Score` = cumsum(`Event Score`)) %>% 
  ggplot(aes(x = `Game Time`, y = `Net Score`)) + 
  geom_point() + 
  geom_step() +
  # geom_label_repel(aes(label = `Event Type`, fill = `Game Situtaiton`)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  coord_cartesian(xlim = c(0, 100)) +
  theme(axis.line.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  facet_grid(`Player Name` ~ .) +
  scale_fill_viridis(discrete = T, begin = .35)
  
 ggsave("plots/demo-timeline-cumulative-adams-rbl-nolab.png", height = 3.5, width = 8)

 
 
 rbl %>% 
   filter(`Player Name` == "Adams") %>% 
   mutate(`Net Score` = cumsum(`Event Score`)) %>% 
   ggplot(aes(x = `Game Time`, y = `Net Score`)) + 
   geom_point() + 
   geom_step() +
   geom_label_repel(aes(label = `Event Type`, fill = `Game Situtaiton`),
                    point.padding = .5, box.padding = .65, label.padding = .1) +
   theme_bw() +
   scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
   scale_y_continuous(breaks = seq(0, 12, by = 2)) +
   coord_cartesian(xlim = c(0, 100), ylim = c(-1, 10)) +
   theme(axis.line.y=element_blank(),
         axis.line.x =element_blank(),
         legend.position = "top",
         panel.grid.minor = element_blank()) +
   facet_grid(`Player Name` ~ .) +
   scale_fill_viridis(discrete = T, begin = .35)
 
 ggsave("plots/demo-timeline-cumulative-adams-rbl.png", height = 3.5, width = 8)
 
 
 
 
 rbl %>% 
   filter(`Player Name` %in% playersofinterest) %>% 
   group_by(`Player Name`) %>% 
   mutate(`Net Score` = cumsum(`Event Score`)) %>% 
   ungroup() %>% 
   ggplot(aes(x = `Game Time`, y = `Net Score`, color = `Player Name`)) + 
   geom_step() +
   # geom_label_repel(aes(label = `Player Name`),
   #                  nudge_x = 1, na.rm = T) +
   directlabels::geom_dl(aes(label = `Player Name`), 
                         method = list(directlabels::dl.combine("last.points"))) +
   theme_bw() +
   scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
   scale_y_continuous(breaks = seq(-10, 212, by = 2)) +
   # coord_cartesian(xlim = c(0, 100), ylim = c(-1, 10)) +
   theme(axis.line.y=element_blank(),
         axis.line.x =element_blank(),
         legend.position = "top",
         panel.grid.minor = element_blank()) +
   coord_cartesian(xlim = c(0, 110)) +
 scale_color_brewer(palette = "Set1", guide = "none")

 ggsave("plots/demo-timeline-cumulative-multiplayer.png", height = 3.5, width = 8)
 