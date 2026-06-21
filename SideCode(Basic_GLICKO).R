library(sport)
library(tidyverse)
library(PlayerRatings)
library(ggplot2)
library(dplyr)
#Basic data cleaning and formatting
UFC_GOLD <- read.csv("ufc_gold_dataset_final.csv")
UFC <- UFC_GOLD %>%
  rename(
    date         = Event_Date,
    r_name       = Fighter_1,
    b_name       = Fighter_2,
    winner       = Winner,
    method       = Method,
    r_kd         = F1_KD,
    b_kd         = F2_KD,
    r_sig_str_landed  = F1_Sig_Landed,
    r_sig_str_atmpted = F1_Sig_Att,
    b_sig_str_landed  = F2_Sig_Landed,
    b_sig_str_atmpted = F2_Sig_Att,
    r_td_landed  = F1_TD_Landed,
    r_td_atmpted = F1_TD_Att,
    b_td_landed  = F2_TD_Landed,
    b_td_atmpted = F2_TD_Att,
    r_sub_att    = F1_Sub_Att,
    b_sub_att    = F2_Sub_Att,
    r_ctrl       = F1_Ctrl_Sec,
    b_ctrl       = F2_Ctrl_Sec,
    r_head_landed = F1_Head,
    b_head_landed = F2_Head,
    r_body_landed = F1_Body,
    b_body_landed = F2_Body,
    r_leg_landed  = F1_Leg,
    b_leg_landed  = F2_Leg,
    r_dist_landed   = F1_Distance,
    b_dist_landed   = F2_Distance,
    r_clinch_landed = F1_Clinch,
    b_clinch_landed = F2_Clinch,
    r_ground_landed = F1_Ground,
    b_ground_landed = F2_Ground,
    finish_round    = End_Round,
    match_time_sec  = Total_Fight_Time_Sec
  ) %>%
  mutate(
    date = as.Date(date),
    red_wins = case_when(
      winner == r_name ~ 1,
      winner == b_name ~ 0,
      TRUE ~ 0.5
    ),
    period = as.integer(format(date, "%Y%m"))
  )
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
decay_rating <- function(mu, months_inactive, threshold = 8, decay_rate = 0.1) {
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

results <- rating_history %>%
  group_by(Player) %>%
  slice_max(Rating, n = 1) %>%
  ungroup()

fighters <- c(
  "Jon Jones",
  "Georges St-Pierre",
  "Anderson Silva",
  "Demetrious Johnson"
)

rating_history %>%
  filter(Player %in% fighters) %>%
  group_by(Player) %>%
  filter(period >= min(period)) %>%
  ungroup() %>%
  ggplot(aes(period, Rating, colour = Player)) +
  geom_line(linewidth = 0.8, alpha = 0.5) +
  geom_point(alpha = 0.9) +
  theme_minimal()+
  labs(title = "Popular 'GOATs' Glicko-2 Ratings Over Time", subtitle="Without Method Bonus", x = "Period", y = "Rating", colour = "Fighter")



active_fighters <- rating_history %>%
  filter(period >= 202500) %>%
  pull(Player) %>%
  unique()

top3 <- rating_history %>%
  filter(Player %in% active_fighters) %>%
  group_by(Player) %>%
  slice_max(period, n = 1) %>%
  ungroup() %>%
  slice_max(Rating, n = 3) %>%
  pull(Player)

rating_history %>%
  filter(Player %in% top3) %>%
  group_by(Player) %>%
  filter(period >= min(period)) %>%
  ungroup() %>%
  ggplot(aes(period, Rating, colour = Player)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(alpha = 0.9) +
  theme_minimal() +
  labs(title = "Top 3 Active UFC Fighters by Glicko-2 Rating",
       x = "Period", y = "Rating", colour = "Fighter", subtitle="Without Method Bonus")

library(gridExtra)

top10_table <- results %>%
  arrange(desc(Rating)) %>%
  head(10) %>%
  select(Player, Rating, Deviation, Volatility, period) %>%
  mutate(Rating = round(Rating, 1),
         Deviation = round(Deviation, 1),
         Volatility = round(Volatility, 4),
         period = as.character(period)) %>%
  rename("Fighter" = Player,
         "Peak Rating" = Rating,
         "RD" = Deviation,
         "Volatility" = Volatility,
         "Peak Period" = period)
grid.newpage()
grid.table(top10_table, rows = NULL)
