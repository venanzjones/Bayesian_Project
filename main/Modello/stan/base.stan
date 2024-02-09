data {
  int<lower=1> N; // Number of observations
  int<lower=1> P; // Covariate number

  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
}

parameters {
  vector[P] beta; // Coefficients for predictors
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;

  fix_eff = X * beta;

  lambda = exp(fix_eff);
}

model {
  beta ~ normal(0, 2);
  y[1:N] ~ poisson(lambda[1:N]);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_pred;
 
  for(i in 1:N){
    log_lik[i] = poisson_lpmf(y[i]|lambda[i]);
    y_pred[i] = poisson_rng(lambda[i]);
  }
}


