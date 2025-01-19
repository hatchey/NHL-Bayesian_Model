library(ggplot2)
library(viridis)
library(tidyverse)
library(tidyr)
library(reshape)

player_names_df <- nhl_game_stats %>%
  dplyr::select(player_id, player_name) %>%
  dplyr::distinct(player_id)

posterior <- rstan::extract(fit)

shooting_percent <- posterior$p

shooting_percent_df <- melt(shooting_percent)
colnames(shooting_percent_df) <- c('Iteration', 'ID', 'Shooting_Percentage')

player_names <- names(player_ids)
ordered_player_names <- sort(player_names)

player_mapping <- match(player_names, ordered_player_names)

shooting_percent_df$ID <- factor(player_mapping[shooting_percent_df$ID], levels = 1:length(ordered_player_names), labels = ordered_player_names)

top_20_shooting_percent_df <- shooting_percent_df %>%
  dplyr::left_join(player_names_df, by = c('ID' = 'player_id')) %>%
  dplyr::rename(Player = 'player_name') %>%
  dplyr::head(20)

ggplot(stop_20_shooting_percent_df, aes(x = Shooting_Percentage, fill = Player)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0.065, color = 'red',  linetype = 'dashed') +
  facet_wrap(~ player_name, scale = 'free', labeller = labeller(player_name = label_value)) + 
  ggtitle('Distribution of Player Shooting Percentage') +
  xlab('Shooting Percentage') +
  ylab('Density') + scale_fill_viridis_d(option = 'C', direction = 1) +
  theme_minimal() + 
  theme(legend.position = 'none')
  