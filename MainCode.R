library(sport)
library(tidyverse)
library(PlayerRatings)
library(ggplot2)
library(dplyr)

#Basic data cleaning and formatting
UFC <- read.csv("UFC.csv")
UFC <- UFC |>
  mutate(date = as.Date(date),
         red_wins = case_when(
           winner == r_name ~ 1,
           winner == b_name ~ 0,
           TRUE ~ 0.5),
         period = as.integer(format(as.Date(date), "%Y%m")))


train <- UFC |> 
  filter(date < as.Date("2026-01-01"))

test  <- UFC |> 
  filter(date >= as.Date("2026-01-01"))


#Glicko
ufc_glicko <- train %>%
  filter(!is.na(red_wins)) %>%
  select(period, r_name, b_name, red_wins)


result <- glicko2(ufc_glicko)
results <- result$ratings



decay_rating <- function(mu, months_inactive, threshold = 14, decay_rate = 0.1) {
  if (months_inactive <= threshold) return(mu)
  excess_months <- months_inactive - threshold
  mu * exp(-decay_rate * (excess_months / 12))
}



#Glicko Time Series
periods <- sort(unique(ufc_glicko$period))
ratings <- NULL
history <- list()
last_active <- list()

for (p in periods) {
  current_period <- filter(ufc_glicko, period == p)
  active_fighters <- unique(c(current_period$r_name, current_period$b_name))
  
  ratings <- glicko2(current_period, status = ratings)$ratings
  
  for (fighter in active_fighters) {
    if (!is.null(last_active[[fighter]])) {
      months_inactive <- (floor(p / 100) - floor(last_active[[fighter]] / 100)) * 12 +
        (p %% 100 - last_active[[fighter]] %% 100)
      ratings$r[ratings$Player == fighter] <- decay_rating(
        mu = ratings$r[ratings$Player == fighter],
        months_inactive = months_inactive
      )
    }
    last_active[[fighter]] <- p
  }
  history[[as.character(p)]] <- ratings %>%
    as.data.frame() %>%
    mutate(period = p) %>%
    filter(Player %in% active_fighters)
}



rating_history <- bind_rows(history)

fighters <- c(
  "Jon Jones",
  "Islam Makhachev",
  "Ilia Topuria",
  "Georges St-Pierre",
  "Francis Ngannou"
)

rating_history %>%
  filter(Player %in% fighters) %>%
  group_by(Player) %>%
  filter(period >= min(period)) %>%
  ungroup() %>%
  ggplot(aes(period, Rating, colour = Player)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal()


     
