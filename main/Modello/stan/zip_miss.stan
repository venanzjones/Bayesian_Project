data {
  int<lower=1> N; // Number of observations
  int<lower=0> N_miss; // Number of missing observations
  int<lower=1> P; // Covariate number
  int<lower=1> nyears;
  int<lower=1> nstations;

  array[N] int<lower=1> year;
  array[N] int<lower=1> stations;
  array[N] int<lower=0> max_month;

  array[N_miss] int<lower=0> station_miss;
  array[N_miss] int<lower=0> year_miss;
  array[N_miss] int<lower=0> max_month_miss;

  array[N] int<lower=0> y; // Count data
  matrix[N, P] X; // Predictor matrix
  matrix[N_miss,P] X_miss;
  
  real phi;
  matrix[nstations, nstations] distances;
  
  int nmonths;
  array[N] int month;
  array[N_miss] int month_miss;

}

transformed data {
  matrix[nstations,nstations] H = exp(-phi * distances);
}

parameters {
  vector[P] beta; // Coefficients for predictors
  vector<lower=0, upper=1>[nmonths] theta; // Probability of excess zeros
  
  vector[nyears] xi; // Random effects for years
  vector[nstations] eta;//For the stations
  
  real<lower = 0> sigma;
  real<lower = 0> sigma_beta;
  real<lower = 0> sigma_xi;
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;
  vector[N] intercept;
  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; 
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);

  fix_eff = X * beta;
  intercept = eta[stations] + xi[year];

  lambda = exp(fix_eff + intercept);  

    vector[N_miss] lambda_miss;
    vector[N_miss] fix_eff_miss;
    vector[N_miss] intercept_miss;

    fix_eff_miss = X_miss * beta;
    intercept_miss = xi[year_miss] + eta[station_miss];

    lambda_miss = exp(fix_eff_miss + intercept_miss);
}

model {
  beta ~ normal(0, sigma_beta);

  xi ~ normal(0, sigma_xi);
  eta ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(2, 2);
  sigma_beta ~ inv_gamma(4, 2);
  sigma_xi ~ inv_gamma(4, 2);

  theta ~ beta(0.5, 0.5); // Prior for excess zeros probability

  for (i in 1:N) {
    if (y[i] == 0) {
      target += log_sum_exp(log(theta[month[i]]),
                            log1m(theta[month[i]])
                              + poisson_lpmf(y[i] | lambda[i]));
    } else {
      target += log1m(theta[month[i]])
                  + poisson_lpmf(y[i] | lambda[i]);
    }
  }
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;
 
  for(i in 1:N){
    if (y[i] == 0) {
      log_lik[i] = log_sum_exp(log(theta[month[i]]), log1m(theta[month[i]]) + poisson_lpmf(y[i] | lambda[i]));
    } else {
      log_lik[i] = log1m(theta[month[i]]) + poisson_lpmf(y[i] | lambda[i]);
    }
  }
  
  for(i in 1:N) {
    real is_zero = bernoulli_rng(theta[month[i]]);
    if(is_zero == 1) {
      y_pred[i] = 0;
    } else {
      y_pred[i] = poisson_rng(lambda[i]);
    }
  }

  for(i in 1:N_miss) {
    real is_zero = bernoulli_rng(theta[month_miss[i]]);
    if(is_zero == 1) {
      y_pred_miss[i] = 0;
    } else {
      y_pred_miss[i] = poisson_rng(lambda_miss[i]);
    }
  }
}