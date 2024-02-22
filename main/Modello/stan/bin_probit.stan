data {
  int<lower=1> N; // Number of observations
  int<lower=1> P; // Covariate number
  int<lower=1> nyears;
  int<lower=1> nstations;

  array[N] int<lower=1> year;
  array[N] int<lower=1> stations;

  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
  real phi;
  matrix[nstations, nstations] distances;

  array[N] int max_month;
}

transformed data {
  matrix[nstations,nstations] H = exp(-(1/phi) * distances);
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nyears] xi; // Random effects for years
  vector[nstations] eta;//For the stations
  real<lower = 0> sigma;
  real<lower = 0> sigma_xi;
}

transformed parameters {
    vector[N] fix_eff;
    vector[N] rand_eff;
    matrix[nstations,nstations] Sigma_s = sigma * H; //To be added the variance
    matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);

    fix_eff = X * beta;
    rand_eff = xi[year] + eta[stations];
}

model {
  beta ~ normal(0, 1);
  for (i in 1:N) {
    y[i] ~ binomial(max_month[i], Phi(fix_eff[i] + rand_eff[i]));
  }

  xi ~ normal(0, sqrt(sigma_xi));
  eta ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
 
  for(i in 1:N){
    log_lik[i] = binomial_lpmf(y[i]|max_month[i], Phi(fix_eff[i] + rand_eff[i]));
    y_pred[i] = binomial_rng(max_month[i], Phi(fix_eff[i] + rand_eff[i]));
  }
}