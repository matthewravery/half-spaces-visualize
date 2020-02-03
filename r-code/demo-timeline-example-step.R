# install.packages("googlesheets4")
# install.packages("ggrepel")
# install.packages("directlabels")

library(googlesheets4)
library(tidyverse)
library(ggrepel)
library(viridis)
library(directlabels)

pad_time <- function(tb){
  
  pads <- tb %>% 
    group_by(`Player Name`) %>% 
    slice(1) %>% 
    mutate(`Game Time` = max(tb$`Game Time`)) 
    
  tb %>% 
    bind_rows(pads)
}


playersofinterest <- c("Adams", "Upamecano", "Werner")
# playersofinterest <- c("Upamecano")

rbl <- read_sheet("https://docs.google.com/spreadsheets/d/1PAw_zTJpZ7FbQwEIKIw8xc4cst7dCGXvPT0SNnUfK-8/edit#gid=0", sheet = "Game Log") %>% 
  pad_time() %>% 
  filter(`Player Name` %in% playersofinterest) %>% 
  arrange(`Player Name`, `Game Time`) %>%
  group_by(`Player Name`) %>% 
  mutate(`Net Score` = cumsum(`Event Score`)) %>% 
  ungroup() 


rblstep <- bind_rows(old = rbl, 
                     new = rbl %>% group_by(`Player Name`) %>%  mutate(`Net Score` = lag(`Net Score`)),
                     .id = "source") %>%
  filter(!is.na(`Net Score`)) %>% 
  arrange(`Game Time`, source)

lastpoints <- rbl %>% 
  group_by(`Player Name`) %>% 
  arrange(desc(`Game Time`)) %>% 
  slice(1)

rbl %>% 
  ggplot(aes(x = `Game Time`, y = `Net Score`, color = `Player Name`)) + 
  geom_step() +
  geom_ribbon(aes(x = `Game Time`, ymin = 0, ymax = `Net Score`, fill = `Player Name`), 
              data = rblstep, alpha = .1) +
  geom_label_repel(aes(label = `Player Name`),
                   nudge_x = 1, na.rm = T, data = lastpoints) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
  scale_y_continuous(breaks = seq(-10, 212, by = 2)) +
  # coord_cartesian(xlim = c(0, 100), ylim = c(-1, 10)) +
  theme(axis.line.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  # coord_cartesian(xlim = c(0, 110)) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_fill_brewer(palette = "Set1", guide = "none")


ggsave("plots/demo-timeline-cumulative-multiplayer-step.png", height = 3.5, width = 8)


## all players


rbl <- read_sheet("https://docs.google.com/spreadsheets/d/1PAw_zTJpZ7FbQwEIKIw8xc4cst7dCGXvPT0SNnUfK-8/edit#gid=0", sheet = "Game Log") %>% 
  pad_time() %>% 
  arrange(`Player Name`, `Game Time`) %>%
  group_by(`Player Name`) %>% 
  mutate(`Net Score` = cumsum(`Event Score`)) %>% 
  ungroup() 

lastpoints <- rbl %>% 
  group_by(`Player Name`) %>% 
  arrange(desc(`Game Time`)) %>% 
  slice(1)


rbl %>% 
  ggplot(aes(x = `Game Time`, y = `Net Score`, group = `Player Name`)) + 
  geom_step() +
  geom_label_repel(aes(label = `Player Name`),
                   nudge_x = 1, na.rm = T, data = lastpoints) +
    theme_bw() +
  scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
  scale_y_continuous(breaks = seq(-10, 212, by = 2)) +
  theme(axis.line.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0, 110))

ggsave("plots/demo-timeline-cumulative-allplayer-step.png", height = 6, width = 8)
