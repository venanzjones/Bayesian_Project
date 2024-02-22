data {
  int<lower=0> N; // Number of observations
  int<lower=0> N_miss; // Number of missing observations
  int<lower=0> P; // Covariate number
  int<lower=0> nyears;
  int<lower=0> nstations;

  array[N] int<lower=0> station;
  array[N] int<lower=0> year;
  array[N] int<lower=0> max_month;

  array[N_miss] int<lower=0> station_miss;
  array[N_miss] int<lower=0> year_miss;
  array[N_miss] int<lower=0> max_month_miss;
  
  array[N] int<lower=0> y; // Count data

  matrix[N, P] X; // Predictor matrix
  matrix[N_miss, P] X_miss; // Predictor matrix

  real phi;
  matrix[nstations, nstations] distances;

  vector[N] dummy_obs_jul;
  vector[N_miss] dummy_miss_jul;
  vector[N] dummy_obs_apr;
  vector[N_miss] dummy_miss_apr;

}

transformed data {
  matrix[nstations,nstations] H = exp(-(1/phi) * distances);
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nstations] eta; // Random effects for comuni
  vector[nyears] xi; // Random effects for years
  real<lower = 0> sigma;
  real<lower = 0> sigma_xi;
  real<lower = 0> sigma_dummy;
  vector[nstations] gamma_jul;
  vector[nstations] gamma_apr;

}

transformed parameters {
  vector[N] alpha;
  vector[N] fix_eff;
  vector[N] intercept;
  vector[N] dummy_eff_jul;
  vector[N] dummy_eff_apr;

  vector[N_miss] alpha_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] intercept_miss;
  vector[N_miss] dummy_eff_miss_jul;
  vector[N_miss] dummy_eff_miss_apr;

  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; //To be added the variance
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;
  for (i in 1:N) {
    dummy_eff_jul[i] = dummy_obs_jul[i] * gamma_jul[station[i]];
    dummy_eff_apr[i] = dummy_obs_apr[i] * gamma_apr[station[i]];
  }
  intercept = xi[year] + eta[station] + dummy_eff_jul + dummy_eff_apr;
  alpha = fix_eff + intercept;

  fix_eff_miss = X_miss * beta;
  for (i in 1:N_miss) {
    dummy_eff_miss_jul[i] = dummy_miss_jul[i] * gamma_jul[station_miss[i]];
    dummy_eff_miss_apr[i] = dummy_miss_apr[i] * gamma_apr[station_miss[i]];
  }
  intercept_miss = xi[year_miss] + eta[station_miss] + dummy_eff_miss_jul + dummy_eff_miss_apr;
  alpha_miss = fix_eff_miss + intercept_miss;
}

model {
  beta ~ normal(0, 1);
  for (i in 1:N) {
    y[i] ~ binomial_logit(max_month[i], alpha[i]);
  };

  xi ~ normal(0, sigma_xi);
  eta ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  gamma_jul ~ normal(0, sigma_dummy);
  gamma_apr ~ normal(0, sigma_dummy);

  sigma ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);
  sigma_dummy ~ inv_gamma(4, 2);

}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;
  vector[N] theta;
  vector[N_miss] theta_miss;

  for(i in 1:N) {
    log_lik [ i ] = binomial_logit_lpmf (y[i] | max_month[i], alpha[i]);
  }
  
  for(i in 1:N){
  theta[i] = inv_logit(alpha[i]);
  y_pred[i] = binomial_rng(max_month[i], theta[i]);
  }

  for(i in 1:N_miss){
  theta_miss[i] = inv_logit(alpha_miss[i]);
  y_pred_miss[i] = binomial_rng(max_month[i], theta_miss[i]);
  }

}