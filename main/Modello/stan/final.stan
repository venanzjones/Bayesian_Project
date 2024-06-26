data {
  int<lower=0> N; // Number of observations
  int<lower=0> N_miss; // Number of missing observations
  int<lower=0> P; // Covariate number
  int<lower=0> nyears;
  int<lower=0> nstations;
  int <lower=0> nmonths;

  array[N] int<lower=0> station;
  array[N] int<lower=0> year;
  array[N] int<lower=0> max_month;
  array[N] int<lower=0> month;

  array[N_miss] int<lower=0> station_miss;
  array[N_miss] int<lower=0> year_miss;
  array[N_miss] int<lower=0> max_month_miss;
  array[N_miss] int<lower=0> month_miss;
  
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
  vector[nstations] w; // Random effects for comuni
  vector[nyears] xi; // Random effects for years
  real mu_xi;
  vector[nmonths] gamma;
  real mu_gamma;
  real<lower = 0> sigma2;
  real<lower = 0> sigma2_xi;
  real<lower = 0> sigma2_gamma;
  vector<lower = 0>[nstations] sigma2_eta;
}

transformed parameters {
  vector[N] alpha;
  vector[N] fix_eff;
  vector[N] intercept;

  vector[N_miss] alpha_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] intercept_miss;

  matrix[nstations,nstations] Sigma_s = sigma2 * H + diag_matrix(sigma2_eta); 
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;

  intercept = gamma[month] + xi[year] + w[station];
  alpha = fix_eff + intercept; //It is the logit of theta actual parameter

  fix_eff_miss = X_miss * beta;

  intercept_miss = gamma[month_miss] + xi[year_miss] + w[station_miss];
  alpha_miss = fix_eff_miss + intercept_miss;
}

model {
  beta ~ normal(0, 1);
  for (i in 1:N) {
    y[i] ~ binomial_logit(max_month[i], alpha[i]);
  }

  mu_xi ~ normal(0, 1);
  for (i in 1:nyears) {
    xi[i] ~ normal(mu_xi, sqrt(sigma2_xi));
  }
  mu_gamma ~ normal(0, 1);
  for (i in 1:nmonths) {
    gamma[i] ~ normal(mu_gamma, sqrt(sigma2_gamma));
  }

  w ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  
  sigma2 ~ inv_gamma(4,2);
  sigma2_xi ~inv_gamma(4,2);
  sigma2_gamma ~ inv_gamma(4,2);
  sigma2_eta ~ inv_gamma(4,2);
}

generated quantities {
  vector[N] log_lik;

  vector[N] y_pred;
  vector[N_miss] y_pred_miss;

  vector[N] theta;
  vector[N_miss] theta_miss;

  for(i in 1:N) {
    log_lik [i] = binomial_lpmf(y[i]|max_month[i], inv_logit(alpha[i]));
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
