data {
  int<lower=1> N; // Number of observations
  int<lower=1> P; // Covariate number
  int<lower=1> nyears;
  int<lower=1> nstations;

  array[N] int<lower=1> year;
  array[N] int<lower=1> stations;

  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nyears] xi; // Random effects for years
  vector[nstations] eta;//For the stations
  real<lower = 0> sigma_eta;
  real<lower = 0> sigma_beta;
  real<lower = 0> sigma_xi;
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;
  vector[N] intercept;

  fix_eff = X * beta;
  intercept = xi[year] + eta[stations];

  lambda = exp(fix_eff + intercept);
}

model {
  beta ~ normal(0, sigma_beta);
  y[1:N] ~ poisson(lambda[1:N]);

  xi ~ normal(0, sigma_xi);
  eta ~ normal(0, sigma_eta);
  sigma_eta ~ inv_gamma(2, 2);
  sigma_beta ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_pred;
 
  for(i in 1:N){
    log_lik[i] = poisson_lpmf(y[i]|lambda[i]);
    y_pred[i] = poisson_rng(lambda[i]);
  }
}

