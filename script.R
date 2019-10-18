# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(gganimate)

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
    mutate(Month = i - 1) # We are doing the -1 here because first file (13 Jan) is baseline ranking
  raw_tibble <- bind_rows(raw_tibble, temp_data)
}

# Manipulate data

teamrankings <- raw_tibble %>% 
  filter(`Team Code` %in% WTT) %>% 
  mutate(Date = case_when(
    Month == 0 ~ "13/1/19",
    Month == 1 ~ "27/1/19",
    Month == 2 ~ "24/2/19",
    Month == 3 ~ "31/3/19",
    Month == 4 ~ "28/4/19",
    Month == 5 ~ "26/5/19",
    Month == 6 ~ "30/6/19",
    Month == 7 ~ "28/7/19",
    Month == 8 ~ "25/8/19",
    Month == 9 ~ "29/9/19",
    Month == 10 ~ "18/10/19",
  ),
  Date = dmy(Date)) 

baselinePoints <- teamrankings %>% 
  filter(Month == 0)

# Plot

teamrankings %>% 
  filter(Rank <= 5) %>% 
  ggplot(aes(x = Date, y = Points, col = Name)) +
  geom_point() +
  scale_color_viridis_d() +
  transition_time(Date) + 
  labs(title = "Date: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  shadow_mark(alpha = 0.3, size = 0.5)

# In-conding checks

teamrankings %>% 
  filter(`Team Code` == "INS")
