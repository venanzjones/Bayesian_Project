functions {
  real my_tan(real x) {
    return(sin(x) / cos(x));
  }
}

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

}

transformed data {
  matrix[nstations,nstations] H = exp(-(1/phi) * distances);
  array[N] real theta;
  for (i in 1:N)
  {
	theta[i] = my_tan((y[i]%/%max_month[i])*((y[i]%/%max_month[i])-0.5));
  }
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nstations] eta; // Random effects for comuni
  vector[nyears] xi; // Random effects for years
  vector[nstations] w;
  real<lower = 0> sigma;
  real<lower = 0> sigma_obs;
  real<lower = 0> sigma_xi;
  real<lower = 0> sigma_eta;

}

transformed parameters {
  vector[N] alpha;
  vector[N] fix_eff;
  vector[N] intercept;

  vector[N_miss] alpha_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] intercept_miss;

  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; //To be added the variance
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;

  intercept = xi[year] + eta[station] + w[station];
  alpha = fix_eff + intercept;

  fix_eff_miss = X_miss * beta;

  intercept_miss = xi[year_miss] + eta[station_miss]+ w[station_miss];
  alpha_miss = fix_eff_miss + intercept_miss;
}

model {
  beta ~ normal(0, 1);
  for (i in 1:N) {
    theta[i] ~ normal(alpha[i], sigma_obs);
  };

  xi ~ normal(0, sigma_xi);
  eta ~ normal(0, sigma_eta);
  w ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(4, 2);
  sigma_obs ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);
  sigma_eta ~ inv_gamma(4, 2);

}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;
  vector[N] theta_pred;
  vector[N_miss] theta_pred_miss;

  for(i in 1:N) {
    log_lik [ i ] = normal_lpdf (theta[i] | alpha[i], sigma_obs);
  }
  
  for(i in 1:N){
  theta_pred[i] = normal_rng(alpha[i], sigma_obs);
  y_pred[i] = inv_logit(theta_pred[i])*max_month[i];
  }

  for(i in 1:N_miss){
  theta_pred_miss[i] = normal_rng(alpha_miss[i], sigma_obs);
  y_pred_miss[i] = inv_logit(theta_pred_miss[i])*max_month_miss[i];
  }

}