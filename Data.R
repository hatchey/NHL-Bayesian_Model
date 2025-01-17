library(hockeyR)
library(dplyr)
library(tidyr)


nhl_pbp <- hockeyR::load_pbp(season = 2015:2023)

nhl_game_stats <- nhl_pbp %>%
  dplyr::filter(season_type == 'R') %>%
  dplyr::group_by(season, game_id, home_abbreviation, away_abbreviation) %>%
  dplyr::summarize(
    home_es_xg = sum(xg[strength_code %in% c('EV', 'EVEN') & period %in% c(1:3) & home_abbreviation == event_team_abbr], na.rm = TRUE),
    home_pp_xg = sum(xg[strength_code %in% c('PPG', 'PP') & period %in% c(1:3) & home_abbreviation == event_team_abbr], na.rm = TRUE),
    home_sh_xg = sum(xg[strength_code %in% c('SHG', 'SH') & period %in% c(1:3) & home_abbreviation == event_team_abbr], na.rm = TRUE),
    home_ot_xg = sum(xg[!(period %in% c(1:3)) & home_abbreviation == event_team_abbr], na.rm = TRUE),
    away_es_xg = sum(xg[strength_code %in% c('EV', 'EVEN') & period %in% c(1:3) & away_abbreviation == event_team_abbr], na.rm = TRUE),
    away_pp_xg = sum(xg[strength_code %in% c('PPG', 'PP') & period %in% c(1:3) & away_abbreviation == event_team_abbr], na.rm = TRUE),
    away_sh_xg = sum(xg[strength_code %in% c('SHG', 'SH') & period %in% c(1:3) & away_abbreviation == event_team_abbr], na.rm = TRUE),
    away_ot_xg = sum(xg[!(period %in% c(1:3)) & away_abbreviation == event_team_abbr], na.rm = TRUE),
    home_es_goals = sum(event_type == "GOAL" & strength_code %in% c('EV', 'EVEN') & period %in% c(1:3) & home_abbreviation == event_team_abbr, na.rm = TRUE),
    home_pp_goals = sum(event_type == "GOAL" & strength_code %in% c('PPG', 'PP') & period %in% c(1:3) & home_abbreviation == event_team_abbr, na.rm = TRUE),
    home_sh_goals = sum(event_type == "GOAL" & strength_code %in% c('SHG', 'SH') & period %in% c(1:3) & home_abbreviation == event_team_abbr, na.rm = TRUE),
    home_ot_goals = sum(event_type == "GOAL" & !(period %in% c(1:3)) & home_abbreviation == event_team_abbr, na.rm = TRUE),
    away_es_goals = sum(event_type == "GOAL" & strength_code %in% c('EV', 'EVEN') & period %in% c(1:3) & away_abbreviation == event_team_abbr, na.rm = TRUE),
    away_pp_goals = sum(event_type == "GOAL" & strength_code %in% c('PPG', 'PP') & period %in% c(1:3) & away_abbreviation == event_team_abbr, na.rm = TRUE),
    away_sh_goals = sum(event_type == "GOAL" & strength_code %in% c('SHG', 'SH') & period %in% c(1:3) & away_abbreviation == event_team_abbr, na.rm = TRUE),
    away_ot_goals = sum(event_type == "GOAL" & !(period %in% c(1:3)) & away_abbreviation == event_team_abbr, na.rm = TRUE),
    .groups = "drop"
  )

teams <- unique(c(nhl_game_stats$home_abbreviation, nhl_game_stats$away_abbreviation))
team_ids <- setNames(seq_along(teams), teams)


nhl_stats <- nhl_game_stats %>%
  dplyr::group_by(season, home_abbreviation) %>%
  dplyr::summarize(xg_for = sum(home_es_xg + home_pp_xg + home_sh_xg + home_ot_xg, na.rm = TRUE),
                   xg_against = sum(away_es_xg + away_pp_xg + away_sh_xg + home_ot_xg, na.rm = TRUE),
                   goals_for = sum(home_es_goals + home_pp_goals + home_sh_goals + home_ot_goals, na.rm = TRUE),
                   goals_against = sum(away_es_goals + away_pp_goals + away_sh_goals + home_ot_goals, na.rm = TRUE)) %>%
  dplyr::rename(team = home_abbreviation) %>%
  base::rbind(nhl_game_stats %>%
                dplyr::group_by(season, away_abbreviation) %>%
                dplyr::summarize(xg_for = sum(away_es_xg + home_pp_xg + home_sh_xg + home_ot_xg, na.rm = TRUE),
                                 xg_against = sum(home_es_xg + away_pp_xg + away_sh_xg + home_ot_xg, na.rm = TRUE),
                                 goals_for = sum(away_es_goals + home_pp_goals + home_sh_goals + home_ot_goals, na.rm = TRUE),
                                 goals_against = sum(home_es_goals + away_pp_goals + away_sh_goals + home_ot_goals, na.rm = TRUE)) %>%
                dplyr::rename(team = away_abbreviation)) %>%
  dplyr::group_by(team, season) %>%
  dplyr::summarize(xg_for = sum(xg_for, na.rm = TRUE), 
                   xg_against = sum(xg_against, na.rm = TRUE),
                   goals_for = sum(goals_for, na.rm = TRUE), 
                   goals_against = sum(goals_against, na.rm = TRUE)) %>%
  dplyr::mutate(xg_diff = xg_for - xg_against,
                xg_diff_g = xg_diff / 82,
                goals_diff = goals_for - goals_against,
                goals_diff_g = goals_diff / 82) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(league_avg_xg_for = mean(xg_for) / 82, 
                league_avg_xg_against = mean(xg_against) / 82,
                league_avg_goals_for = mean(goals_for) / 82,
                league_avg_goals_against = mean(goals_against) / 82) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(exp_xg_for = league_avg_xg_for + (xg_diff_g / 2),
                exp_xg_against = league_avg_xg_for + (xg_diff_g / 2),
                exp_goals_for = league_avg_goals_for + (goals_diff_g / 2),
                exp_goals_against = league_avg_goals_against + (goals_diff_g / 2))

nhl_stats_2022 <- nhl_stats %>% 
  dplyr::filter(season == '20222023')

ggplot(nhl_stats_2022, aes(x = reorder(team, goals_diff), y = goals_diff, fill = goals_diff)) +
  geom_bar(stat = 'identity') + 
  scale_fill_viridis_c(option = 'G', direction = -1) +
  coord_flip() + 
  labs(title = 'NHL Team Goal Difference',
       x = 'Team',
       y = 'Goal Difference Per Game',
       fill = 'Goal Difference') +
  theme_minimal() + 
  theme(legend.position = 'none')



