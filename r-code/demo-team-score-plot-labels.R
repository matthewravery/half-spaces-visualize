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

rbl <- read_sheet("https://docs.google.com/spreadsheets/d/17pyYilWp9xN3AxTpVLEug-0j9mWhWRNFCLac_BxEGO0/edit#gid=0",
                  sheet = "Game Log") %>% 
  pad_time() 

rbl <- rbl %>% 
  arrange(`Game Time`) %>%
  mutate(`Net Score` = cumsum(`Event Score`)) %>% 
  ungroup() 


rblstep <- bind_rows(old = rbl, 
                     new = rbl %>%  mutate(`Net Score` = lag(`Net Score`)),
                     .id = "source") %>%
  filter(!is.na(`Net Score`)) %>% 
  arrange(`Game Time`, source)

lastpoints <- rbl %>% 
  arrange(desc(`Game Time`)) %>% 
  slice(1)

label <- "Full Team (Cumulative)"

labtab <- rbl %>% 
  filter(abs(`Event Score`) > 1) %>% 
  mutate(newlab = map2_chr(`Player Name`, `Event Type`, stringr::str_c, sep = " - "))

# labtab <- slice(labtab, 0)

rbl %>% 
  ggplot(aes(x = `Game Time`, y = `Net Score`)) + 
  geom_step() +
  geom_ribbon(aes(x = `Game Time`, ymin = 0, ymax = `Net Score`), 
              data = rblstep, alpha = .1) +
  theme_bw() +
  geom_label_repel(aes(label = newlab, fill = `Game Situtaiton`),
                   point.padding = .15, data = labtab, nudge_y = 12.5) +
    scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
  scale_y_continuous(breaks = seq(-10, 212, by = 5)) +
  # coord_cartesian(xlim = c(0, 100), ylim = c(-1, 10)) +
  theme(axis.line.y=element_blank(),
        axis.line.x =element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  # coord_cartesian(xlim = c(0, 110)) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_fill_brewer(palette = "Set1")


ggsave("plots/usa-cr-20200201-total-team-score-labels.png", height = 6, width = 12)
# ggsave("plots/usa-cr-20200201-total-team-score-nolabels.png", height = 6, width = 12)
