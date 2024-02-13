
functions {
    #include "./spt_tools.stan"
}

data {
  int<lower=1> N; // Number of observations
  int<lower=1> P; // Covariate number
  int<lower=1> nyears;
  int<lower=1> nstations;


  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
  int N1;
  matrix[N1, P] X1; // Non-zero Predictor matrix
}

transformed data {
  int<lower = 0> N_zero = num_zeros(y);
  array[N1] int<lower = 1> y_nonzero;
  int N_nonzero = 0;
  for (n in 1:N) {
    if (y[n] == 0) continue;
    N_nonzero += 1;
    y_nonzero[N_nonzero] = y[n];
  }
}

parameters {
  vector[P] beta; // Coefficients for predictors
  real<lower=0, upper=1> theta; // Probability of excess zeros
}

transformed parameters {
  vector[N1] lambda;
  vector[N1] fix_eff;

  fix_eff = X1 * beta;
  lambda = exp(fix_eff);
}

model {
  beta ~ normal(0, 2);
  theta ~ beta(0.5, 0.5); // Prior for excess zeros probability

target
     += N_zero
          * log_sum_exp(bernoulli_lpmf(1 | theta),
                        bernoulli_lpmf(0 | theta)
                          + poisson_lpmf(0 | lambda));
   target += N_nonzero * bernoulli_lpmf(0 | theta);
   target += poisson_lpmf(y_nonzero | lambda);
}

generated quantities{
 vector[N] log_lik;
 for(i in 1:N){
   log_lik [ i ] = N_zero
          * log_sum_exp(bernoulli_lpmf(1 | theta),
                        bernoulli_lpmf(0 | theta)
                          + poisson_lpmf(0 | lambda));
    log_lik [ i ] = N_nonzero * bernoulli_lpmf(0 | theta);
    log_lik [ i ] = poisson_lpmf(y_nonzero | lambda);

}
}


