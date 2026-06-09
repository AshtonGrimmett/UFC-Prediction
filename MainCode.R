library(sport)
library(tidyverse)
library(PlayerRatings)
library(ggplot2)
library(dplyr)


UFC <- read.csv("UFC.csv")
UFC <- UFC %>%
  mutate(date = as.Date(date),
         red_wins = case_when(
           winner == r_name ~ 1,
           winner == b_name ~ 0,
           TRUE ~ 0.5),
         period = as.integer(format(as.Date(date), "%Y%m")))


train <- UFC %>% filter(date < as.Date("2025-01-01"))
test  <- UFC %>% filter(date >= as.Date("2025-01-01"))


ufc_glicko <- train %>%
  filter(!is.na(red_wins)) %>%
  select(period, r_name, b_name, red_wins)


result <- glicko2(ufc_glicko)
results <- result$ratings

head(results)






periods <- sort(unique(ufc_glicko$period))

ratings <- NULL
history <- list()

for (p in periods) {
  
  current_period <- filter(ufc_glicko, period == p)
  
  ratings <- glicko2(
    current_period,
    status = ratings
  )$ratings
  
  history[[as.character(p)]] <-
    ratings %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Fighter") %>%
    mutate(period = p)
}

rating_history <- bind_rows(history)

fighters <- c(
  "Jon Jones",
  "Islam Makhachev",
  "Ilia Topuria"
)

rating_history %>%
  filter(Player %in% fighters) %>%
  ggplot(aes(period, Rating, colour = Player)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal()
