
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

  /*
  for (i in 1:N) {
    if (lambda[i] > max_month[i]) {
      lambda[i] = max_month[i];
    }
  }

  for (i in 1:N_miss) {
    if (lambda_miss[i] > max_month_miss[i]) {
      lambda_miss[i] = max_month_miss[i];
    }
  }
  */
}

model {
  beta ~ normal(0, 2);
  y[1:N] ~ poisson(lambda[1:N]);

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

  /*
  for (i in 1:N) {
    real sum_p = 0;
    real u = uniform_rng(0, 1);
    for (b in 0:max_month[i]) {
      sum_p = sum_p + exp(poisson_lpmf(b | lambda[i]) - poisson_lcdf(max_month[i] | lambda[i]));
      if (sum_p >= u) {
        y_pred[i] = b;
        break;
      }
    }
  }
  */

  for(i in 1:N){
    log_lik [ i ] = poisson_lpmf (y[i] | lambda[i]);
    y_pred[i] = poisson_rng(lambda[i]);
  }

  for(i in 1:N_miss){
    y_pred_miss[i] = poisson_rng(lambda_miss[i]);
  }
}

