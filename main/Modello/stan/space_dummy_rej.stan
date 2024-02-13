
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

  vector[N] dummy_obs;
  vector[N_miss] dummy_miss;
}

transformed data {
  matrix[nstations,nstations] H = exp(-(1/phi^2) * distances^2);
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nstations] eta; // Random effects for comuni
  vector[nyears] xi; // Random effects for years
  vector[nstations] w; //Random zero-mean effect for the space model
  real<lower = 0> sigma;
  real<lower = 0> sigma_eta;
  real<lower = 0> sigma_beta;
  real<lower = 0> sigma_xi;

  vector[nstations] dummy_beta;
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;
  vector[N] dummy_eff;
  vector[N] intercept;

  vector[N_miss] lambda_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] dummy_eff_miss;
  vector[N_miss] intercept_miss;

  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; //To be added the variance
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;
  for (i in 1:N) {
    dummy_eff[i] = dummy_obs[i] * dummy_beta[station[i]];
  }
  intercept = xi[year] + eta[station] + w[station] + dummy_eff;
  lambda = exp(fix_eff + intercept);

  fix_eff_miss = X_miss * beta;
  for (i in 1:N_miss) {
    dummy_eff_miss[i] = dummy_miss[i] * dummy_beta[station_miss[i]];
  }
  intercept_miss = xi[year_miss] + eta[station_miss] + w[station_miss] + dummy_eff_miss;
  lambda_miss = exp(fix_eff_miss + intercept_miss);
}

model {
  beta ~ normal(0, 2);
  for (i in 1:N) {
    y[i] ~ poisson(lambda[i]);
  };

  xi ~ normal(0, sigma_xi);
  eta ~ normal(0, sigma_eta);
  w ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(2, 2);
  sigma_eta ~ inv_gamma(2, 2);
  sigma_beta ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);

  dummy_beta ~ normal(-1, 2);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;

  for(i in 1:N) {
    log_lik [ i ] = poisson_lpmf (y[i] | lambda[i]);
  }
  
  for(i in 1:N){
  real y_temp = max_month[i] + 1;
    while (y_temp > max_month[i]) {
      y_temp = poisson_rng(lambda[i]);
    };
    y_pred[i] = y_temp;
  }

  for(i in 1:N_miss){
    real y_temp = max_month_miss[i] + 1;
    while (y_temp > max_month_miss[i]){
      y_temp = poisson_rng(lambda_miss[i]);
    };
    y_pred_miss[i] = y_temp;
  } 
}

