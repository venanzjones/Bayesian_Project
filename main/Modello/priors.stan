

data {
  int<lower=1> N; // Number of observations
  int<lower=1> P; // Covariate number
  int<lower=1> nyears;
  int<lower=1> nstations;


  array[N] int<lower=1> station;
  array[N] int<lower=1> year;
  // array[N] int<lower=4, upper=10> month;

  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector[nstations] eta; // Random effects for comuni
  vector[nyears] xi; // Random effects for years

  //real<lower=0> sigma0; // Standard deviation for beta
  //real<lower=0> sigma1; // Standard deviation for xi
  //real<lower=0> sigma2; // Standard deviation for eta
}

transformed parameters {
    vector[N] lambda;
    vector[N] intercept;
    vector[N] fix_eff;

    intercept = xi[year] + eta[station];
    fix_eff = X * beta;

    lambda = exp(intercept + fix_eff);
}

model {

  beta ~ normal(0, 2);
  xi ~ normal(0, 2);
  eta ~ normal(0, 2);
  y[1:N] ~ poisson(lambda[1:N]);


}


