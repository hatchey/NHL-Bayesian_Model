library(hockeyR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)


nhl_pbp <- hockeyR::load_pbp(season = 2015:2023)

nhl_game_stats <- nhl_pbp %>%
  dplyr::filter(season_type == 'R', strength_code %in% c('EV', 'EVEN'), season %in% c('20212022', '20202021', '20222023')) %>%
  dplyr::group_by(season, game_id, event_player_1_id, event_player_1_name) %>%
  dplyr::summarize(
    goals = sum(event_type == "GOAL" & period %in% c(1:3), na.rm = TRUE),
    shots = sum(event_type == "SHOT" & period %in% c(1:3), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(player_id = event_player_1_id,
                player_name = event_player_1_name) %>%
  dplyr::filter(!is.na(player_id), shots > 0)

players <- unique(nhl_pbp$event_player_1_id)
player_ids <- setNames(seq_along(players), players)


nhl_stats <- nhl_game_stats %>%
  dplyr::filter(season %in% c('20212022', '20202021', '20222023')) %>%
  dplyr::group_by(player_id, player_name) %>%
  dplyr::summarize(goals_for = sum(goals, na.rm = TRUE),
                   shots_for = sum(shots, na.rm = TRUE)) %>%
  dplyr::filter(shots_for > 100) %>%
  dplyr::mutate(shooting_percentage = goals_for / shots_for) %>%
  dplyr::ungroup()

shooting_mean <- mean(nhl_stats$shooting_percentage, na.rm = TRUE)
shooting_var <- var(nhl_stats$shooting_percentage, na.rm = TRUE)
  
nhl_top_20_shooting_percentage <- nhl_stats %>%
  dplyr::arrange(desc(shooting_percentage)) %>%
  utils::head(20)

nb_fit <- fitdistr(nhl_stats$goals_for, "Negative Binomial")


r <- nb_fit$estimate["size"]


list(r = r)

ggplot(nhl_top_20_shooting_percentage, aes(x = player_name, y = shooting_percentage, fill = shooting_percentage)) +
  geom_bar(stat = 'identity') + 
  scale_fill_viridis_c(option = 'G', direction = -1) +
  coord_flip() + 
  labs(title = 'NHL Player Shooting Percentage',
       x = 'Player',
       y = 'Shooting Percentage',
       fill = 'Shooting Percentage') +
  theme_minimal() + 
  theme(legend.position = 'none')



