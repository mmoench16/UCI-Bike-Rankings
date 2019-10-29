# Load libraries
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(gganimate)
library(Cairo)

filepath <- "C:/Users/martim09/Documents/R/UCI-Bike-Rankings/Data"
WTT <- c("ALM", "AST", "TBM", "BOH", "CCC", "DQT", "EF1", "GFC",
         "LTS", "MTS", "MOV", "TDD", "INS", "TJV", "TKA", "SUN", 
         "TFS", "UAD")

# Load data

raw_tibble <- tibble()
filelist <- list.files(path = filepath)

for (i in 1:length(filelist)) {
  temp_data <- read_excel(paste0(filepath, "/", filelist[i]), range = cell_cols("A:E"))
  temp_data <- temp_data %>% 
    mutate(Date = dmy(str_sub(filelist[i], -13, -6)))
  raw_tibble <- bind_rows(raw_tibble, temp_data)
}

# Manipulate data

teamrankings <- raw_tibble %>% 
  filter(`Team Code` %in% WTT) %>% 
  arrange(Date, Rank)

# Plot

teamColours <- c("#35E1EE","#009F7D", "#214699", "#16A5CF", "#951433", "#FEB729")
Top3Teams <- c("AST", "DQT", "INS", "BOH", "MOV", "TJV")

top3anim <- teamrankings %>%
  filter(`Team Code` %in% Top3Teams) %>%
  mutate(AlphaLine = ifelse(Rank > 3, 0.7, 0.9),
         AlphaPoint = ifelse(Rank > 3, 0.7, 1),
         AlphaText = ifelse(Rank > 3, 0.7, 1)) %>%
  ggplot(aes(x = Date, y = Points, col = Name, label = paste0(`Team Code`, ": #", Rank))) +
  geom_line(size = 2.5, aes(alpha = AlphaLine, group = Name)) +
  geom_point(size = 3, aes(alpha = AlphaPoint), show.legend = F) +
  geom_text(hjust = 0, nudge_x = 3, aes(alpha = AlphaText), show.legend = F) +
  scale_colour_manual(values = teamColours) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)) +
  guides(alpha = FALSE) +
  scale_x_date(limits = as.Date(c("2019-01-01","2019-11-15"))) +
  transition_reveal(Date) +
  labs(y = "Points",
       x = "Date",
       col = "Teams",
       title = "UCI Team Ranking Top 3 (Road) - Season 2019",
       subtitle = "Date: {frame_along}",
       caption = "Created by @JohnnyZeeGerman; www.marmoe.xyz")

top3anim <- animate(top3anim, nframes = 100, fps = 10, type = "cairo", res = 80, width = 800, height = 450)

anim_save("top3anim.gif", top3anim)

# Table

teamrankings %>% 
  filter(Rank <= 3) %>% 
  group_by(Date, Name, Rank) %>% 
  summarise() %>% 
  View()
