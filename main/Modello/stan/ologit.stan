
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
  vector[31] c_31 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]';
  vector[30] c_30 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]';
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nstations] eta; // Random effects for comuni
  vector[nyears] xi; // Random effects for years
  vector[nstations] w; //Random zero-mean effect for the space model
  real<lower = 0> sigma;
  real<lower = 0> sigma0;
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;
  vector[N] intercept;

  vector[N_miss] lambda_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] intercept_miss;

  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; //To be added the variance
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;
  intercept = xi[year] + eta[station] + w[station];

  lambda = exp(fix_eff + intercept);

  fix_eff_miss = X_miss * beta;
  intercept_miss = xi[year_miss] + eta[station_miss] + w[station_miss];

  lambda_miss = exp(fix_eff_miss + intercept_miss);
}

model {
  beta ~ normal(0, 2);
  for (i in 1:N) {
    if (max_month[i] == 30) {
      y[i] ~ ordered_logistic(lambda[i], c_30);
    }
    if (max_month[i] == 31) {
      y[i] ~ ordered_logistic(lambda[i], c_31);
    }
  }

  xi ~ normal(0, 2);
  eta ~ normal(0, sigma0);
  w ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(2, 2);
  sigma0 ~ inv_gamma(2, 2);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;

  for (i in 1:N) {
    if (max_month[i] == 30) {
      log_lik[i] = ordered_logistic_lpmf(y[i] | lambda[i], c_30);
      y_pred[i] = ordered_logistic_rng(lambda[i], c_30);
    }
    if (max_month[i] == 31) {
      log_lik[i] = ordered_logistic_lpmf(y[i] | lambda[i], c_31);
      y_pred[i] = ordered_logistic_rng(lambda[i], c_31);
    }
  }

  for (i in 1:N_miss) {
    if (max_month_miss[i] == 30) {
      y_pred_miss[i] = ordered_logistic_rng(lambda_miss[i], c_30);
    }
    if (max_month_miss[i] == 31) {
      y_pred_miss[i] = ordered_logistic_rng(lambda_miss[i], c_31);
    }
  }
}