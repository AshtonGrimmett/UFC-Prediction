install.packages("sport")
library(sport)
library(tidyverse)
library(PlayerRatings)

event_details <- read.csv("event_details.csv")
fight_details <- read.csv("fight_details.csv")
fighter_details <- read.csv("fighter_details.csv")
UFC <- read.csv("UFC.csv")


UFC <- UFC %>%
  mutate(date = as.Date(date),
         red_wins = case_when(
           winner == r_name ~ 1,
           winner == b_name ~ 0,
           TRUE ~ 0.5),
         period = as.integer(format(as.Date(date), "%Y%m")))

train <- UFC %>% filter(date < as.Date("2023-01-01"))
test  <- UFC %>% filter(date >= as.Date("2023-01-01"))

ufc_glicko <- train %>%
  filter(!is.na(red_wins)) %>%
  select(period, r_name, b_name, red_wins)


result <- glicko2(ufc_glicko)


