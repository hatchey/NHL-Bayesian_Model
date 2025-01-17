library(rstan)

team_levels <- unique(c(nhl_game_stats$home_abbreviation, nhl_game_stats$away_abbreviation))

nhl_stan_data <- list(
  N = nrow(nhl_game_stats),
  T = length(unique(c(nhl_game_stats$home_abbreviation, nhl_game_stats$away_abbreviation))),
  S = length(unique(nhl_game_stats$season)),
  home_team = as.numeric(factor(nhl_game_stats$home_abbreviation, levels = team_levels)),
  away_team = as.numeric(factor(nhl_game_stats$away_abbreviation, levels = team_levels)),
  home_goals = nhl_game_stats$home_es_goals + nhl_game_stats$home_pp_goals + nhl_game_stats$home_sh_goals + nhl_game_stats$home_ot_goals,
  away_goals = nhl_game_stats$away_es_goals + nhl_game_stats$away_pp_goals + nhl_game_stats$away_sh_goals + nhl_game_stats$away_ot_goals,
  season = as.numeric(as.factor(nhl_game_stats$season))
)

nhl_stan_model <- "
data {
  int<lower=1> N; // Number of games
  int<lower=1> T; // Number of teams
  int<lower=1> S; // Number of seasons
  int home_team[N]; // Home team index
  int away_team[N]; // Away team index
  int home_goals[N]; // Actual goals scored by home team
  int away_goals[N]; // Actual goals scored by away team
  int season[N]; // Season index for each game
}

parameters {
  real home_advantage[S]; // Home advantage per season
  real goal_mean[S]; // Goal mean per season
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  matrix[S, T] att_raw;
  matrix[S, T] def_raw;
}

transformed parameters {
  matrix[S, T] att;
  matrix[S, T] def;

  // Centering xG to remove team effects
  for (s in 1:S) {
    att[s] = att_raw[s] - mean(att_raw[s]);
    def[s] = def_raw[s] - mean(def_raw[s]);
  }
}

model {
  // Priors
  for (s in 1:S) {
    home_advantage[s] ~ normal(0, 1);
    goal_mean[s] ~ normal(0, 1);
    sigma_att ~ normal(0, 1);
    sigma_def ~ normal(0, 1);
    att_raw[s] ~ normal(0, sigma_att);
    def_raw[s] ~ normal(0, sigma_def);
  }

  // Likelihood: Model actual goals using Poisson distribution with log link
  for (i in 1:N) {
    // Home team's goals
    home_goals[i] ~ poisson_log(
      home_advantage[season[i]] + 
      att[season[i], home_team[i]] + 
      def[season[i], away_team[i]] + 
      goal_mean[season[i]]
    );
    
    // Away team's goals
    away_goals[i] ~ poisson_log(
      att[season[i], away_team[i]] + 
      def[season[i], home_team[i]] + 
      goal_mean[season[i]]
    );
  }
}
"

fit <- stan(
  model_code = nhl_stan_model,
  data = nhl_stan_data,
  iter = 4000,
  warmup = 1000,
  chains = 2,
  cores = 6,
  seed = 1967,
  init = "random",
  control = list(max_treedepth = 10))

