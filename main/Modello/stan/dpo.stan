functions {
  real dpo_lpmf(int X, real mu, real phi) {
    real ans;
    real A = inv(2) * log(phi) - phi * mu;
    if (X == 0)
      ans = A;
    else
      ans = A + X * (phi * (1 + log(mu)) - 1) - lgamma(X + 1) + (1 - phi) * X * log(X);
    return ans;
  }

  vector dpo_cdf_(real mu, real phi, int maxval) {
    real d = exp(phi * (1 + log(mu)) - 1);
    real prob;
    int n = maxval + 1;
    vector[n] cdf;
    cdf[1] = sqrt(phi) * exp(-mu * phi);
    prob = cdf[1] * d;
    cdf[2] = cdf[1] + prob;
    for (i in 2:maxval) {
      prob = prob * d * exp((1 - phi) * (i - 1) * (log(i) - log(i - 1))) / (i^phi);
      cdf[i + 1] = cdf[i] + prob;
      if (prob / cdf[i + 1] < 1e-8) {
        n = i + 1;
        break;
      }
    }
    return cdf / cdf[n];
  }

  array[] int dpo_quantiles(vector p, real mu, real phi, int maxval) {
    int N = rows(p);
    array[N] int qs;
    array[N] int indices = sort_indices_asc(p);
    vector[maxval + 1] cdf_vec = dpo_cdf_(mu, phi, maxval);
    int j = 0;
    for (i in indices) {
      while (cdf_vec[j + 1] < p[i]) {
        j += 1;
      }
      qs[i] = j;
    }
    return qs;
  }

  array[] int dpo_sample_rng(int n, real mu, real phi, int maxval) {
    vector[n] p;
    for (i in 1:n) {
      p[i] = uniform_rng(0,1);
    }
    array[n] int x = dpo_quantiles(p, mu, phi, maxval);
    return x;
  }

  int dpo_quantile(real p, real mu, real phi, int maxval) {
    vector[maxval + 1] cdf_vec = dpo_cdf_(mu, phi, maxval);
    int q = 0;
    while (cdf_vec[q + 1] < p) {
        q += 1;
      }
    return q;
  }

  int dpo_rng(real mu, real phi, int maxval) {
    real p = uniform_rng(0,1);
    int x = dpo_quantile(p, mu, phi, maxval);
    return x;
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

  int <lower = 0> nmonths;

  array[N] int<lower = 0> month;
  array[N_miss] int<lower = 0> month_miss;
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

  array[nmonths] real<lower = 0> phi_dpo;
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
  phi_dpo ~ exponential(1);
  for (i in 1:N) {
    target += dpo_lpmf(y[i]| lambda[i], phi_dpo[month[i]]);
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
    log_lik [ i ] = dpo_lpmf (y[i] | lambda[i], phi_dpo[month[i]]);
    y_pred[i] = dpo_rng(lambda[i], phi_dpo[month[i]], 100);
  }

  for(i in 1:N_miss){
    y_pred_miss[i] = dpo_rng(lambda_miss[i], phi_dpo[month_miss[i]], 100);
  }
}