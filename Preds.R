library(ggplot2)
library(viridis)
library(tidyverse)
library(tidyr)
library(reshape)

posterior <- rstan::extract(fit)

agg_strength <- posterior$att[, 9, ] - posterior$def[, 9, ]

agg_strength_df <- melt(agg_strength)
colnames(agg_strength_df) <- c('Iteration', 'Team', 'Strength')

team_names <- names(team_ids)
ordered_team_names <- sort(team_names)

team_mapping <- match(team_names, ordered_team_names)

agg_strength_df$Team <- factor(team_mapping[agg_strength_df$Team], levels = 1:length(ordered_team_names), labels = ordered_team_names)

ggplot(agg_strength_df, aes(x = Strength, fill = Team)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0, color = 'red',  linetype = 'dashed') +
  facet_wrap(~ Team, scale = 'free', labeller = labeller(Team = label_value)) + 
  ggtitle('Distribution of Aggregate Team Strengths') +
  xlab('Aggregate Strength') +
  ylab('Density') + scale_fill_viridis_d(option = 'C', direction = 1) +
  theme_minimal() + 
  theme(legend.position = 'none')
  