library(rstan)

player_levels <- unique(nhl_game_stats$player_id)

nhl_stan_data <- list(
  N = nrow(nhl_game_stats),
  P = length(unique(nhl_game_stats$player_id)),
  player = as.numeric(factor(nhl_game_stats$player_id, levels = player_levels)),
  goals = nhl_game_stats$goals,
  shots = nhl_game_stats$shots,
  alpha = 3.973,
  beta = 39.032
)

nhl_stan_model <- "
data {
  int<lower=1> N; // Number of games
  int<lower=1> P; // Number of players
  int player[N]; // Player Id
  int goals[N]; // Goals scored by player
  int shots[N]; // Shots taken by player
  real<lower=0> alpha; // Alpha parameter for Beta prior
  real<lower=0> beta; // Beta parameter for Beta prior
}

parameters {
  real<lower=0, upper=1> p[N];  // Player-specific success probabilities
  real<lower=0> r;              // Shape parameter for Negative Binomial
}

model {
  // Priors
  p ~ beta(alpha, beta);
  r ~ exponential(0.4);

  // Likelihood: Model actual goals using Poisson distribution with log link
  for (i in 1:N) {
    goals[i] ~ neg_binomial_2(shots[i] * p[i], r);
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

