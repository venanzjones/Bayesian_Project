import os
from cmdstanpy import CmdStanModel

code_120 = """
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
"""
code_120_dummy = """
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

  vector[N] dummy_obs;
  vector[N_miss] dummy_miss;

  int<lower=0,upper=3> sampling_type;
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

  vector[nstations] dummy_beta;
}

transformed parameters {
  vector[N] lambda;
  vector[N] fix_eff;
  vector[N] dummy_eff;
  vector[N] intercept;

  vector[N_miss] lambda_miss;
  vector[N_miss] fix_eff_miss;
  vector[N_miss] dummy_eff_miss;
  vector[N_miss] intercept_miss;

  matrix[nstations,nstations] Sigma_s = sigma * sigma * H; //To be added the variance
  matrix[nstations,nstations] Lw = cholesky_decompose(Sigma_s);


  fix_eff = X * beta;
  for (i in 1:N) {
    dummy_eff[i] = dummy_obs[i] * dummy_beta[station[i]];
  }
  intercept = xi[year] + eta[station] + w[station] + dummy_eff;
  lambda = exp(fix_eff + intercept);

  fix_eff_miss = X_miss * beta;
  for (i in 1:N_miss) {
    dummy_eff_miss[i] = dummy_miss[i] * dummy_beta[station_miss[i]];
  }
  intercept_miss = xi[year_miss] + eta[station_miss] + w[station_miss] + dummy_eff_miss;
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
  for (i in 1:N) {
    y[i] ~ poisson(lambda[i]);
  };

  xi ~ normal(0, 2);
  eta ~ normal(0, sigma0);
  w ~ multi_normal_cholesky(rep_vector(0, nstations), Lw);
  sigma ~ inv_gamma(2, 2);
  sigma0 ~ inv_gamma(2, 2);

  dummy_beta ~ normal(-1, 2);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  vector[N_miss] y_pred_miss;

  for(i in 1:N) {
    log_lik [ i ] = poisson_lpmf (y[i] | lambda[i]);
  }

  if (sampling_type == 0) {
    for (i in 1:N) {
     y_pred[i] = poisson_rng(lambda[i]);
    }

    for (i in 1:N_miss) {
      y_pred_miss[i] = poisson_rng(lambda_miss[i]);
    }
  }
  
  if (sampling_type == 1) {
    for(i in 1:N){
    real y_temp = max_month[i] + 1;
      while (y_temp > max_month[i]) {
        y_temp = poisson_rng(lambda[i]);
      };
      y_pred[i] = y_temp;
    }

    for(i in 1:N_miss){
      real y_temp = max_month_miss[i] + 1;
      while (y_temp > max_month_miss[i]){
        y_temp = poisson_rng(lambda_miss[i]);
      };
      y_pred_miss[i] = y_temp;
    }
  }  

  if (sampling_type == 2) {
    for (i in 1:N) {
      if (y_pred[i] > max_month[i]) {
        y_pred[i] = max_month[i];
      }
    }

    for (i in 1:N_miss) {
      if (y_pred_miss[i] > max_month_miss[i]) {
        y_pred_miss[i] = max_month_miss[i];
      }
    }
  }
  
  if (sampling_type == 3) {
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

    for (i in 1:N_miss) {
      real sum_p = 0;
      real u = uniform_rng(0, 1);
      for (b in 0:max_month[i]) {
        sum_p = sum_p + exp(poisson_lpmf(b | lambda_miss[i]) - poisson_lcdf(max_month[i] | lambda_miss[i]));
        if (sum_p >= u) {
          y_pred_miss[i] = b;
          break;
        }
      }
    }
  }
  
}
"""

hardcode_dict = {
    'model_120': code_120,
    'model_120_dummy': code_120_dummy
}

class FetchModel:
    """
    This class takes as input the name of the model and fetches it from the folder.
    The list of available models is stored in the code_dict attribute. If the requested model is not available, the class raises an error.
    The method compile() returns the compiled model.
    There hardcoded models that can be found before the class declaration are:
    - model_120
    - model_120_dummy
    It is also possible to add new models by adding the stan file directly in the folder ./stan.
    Alternatively it can be done by adding the add_model = True argument when initializing and providing the model_code string.
    To remove a model, it is enough to delete the corresponding file in the folder. (hardcoded models cannot be removed)
    It is possible to retrieve the code of the model by calling the get_code() method.
    It is also possible to update the model by calling the update_model() method and providing the new model_code string.
    """
    def __init__(self, model_name = None, add_model = False, model_code = None):
        if not os.path.exists('./stan'):
            os.makedirs('./stan')
        self.model_list = hardcode_dict.keys()
        self.update_list()
        if add_model:
            if model_name in self.model_list:
                raise ValueError('\nThe model you want to add is {}, which is already available.\nPlease choose another name.\nThe list of names already in use is:\n{}'.format(model_name, self.model_list))
            self.model_list.append(model_name)
            self.stan_file = "./stan/{}.stan".format(model_name)
            with open(self.stan_file, 'w') as f:
                print(model_code, file=f)
            self.update_list()
            print('The model {} has been added.\nThe updated model list is the following:\n{}'.format(model_name, self.model_list))
        else:
            if model_name in self.model_list:
                self.stan_file = "./stan/{}.stan".format(model_name)
                if not os.path.exists(self.stan_file): # this implies that the model is hardcoded
                    with open(self.stan_file, "w") as f:
                        print(hardcode_dict[model_name], file=f)
            else:
                raise ValueError('\nThe requested model is {}, which is not available.\nPlease choose one among the following:\n{}'.format(model_name, self.model_list))
        return self
    
    def update_list(self):
        for file in os.listdir('./stan'):
            if file.endswith('.stan'):
                self.model_list.append(file.split('.')[0])
        return self

    def compile(self):
        return CmdStanModel(stan_file=self.stan_file)
    
    def get_code(self):
        with open(self.stan_file, 'r') as f:
            return f.read()
        
    def update_model(self, model_code):
        with open(self.stan_file, 'w') as f:
            print(model_code, file=f)
        print('The model {} has been updated.\nThe updated model list is the following:\n{}'.format(self.stan_file.split('/')[-1].split('.')[0], self.model_list))
        return self
    