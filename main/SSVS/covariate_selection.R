library(rjags)
library(coda)

# for plots
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr) 

Y <- read.csv("./Datasets/Dataset_180.csv", header = T)
X <- read.csv("./Datasets/variables_to_select.csv", header = T)
# continuous covariates have been standardized
# Type is turned into dummies
# Year, Station, Month are dropped in X

# drop NAs in X and Y 
index_NA <- which(is.na(Y[,'Count_180']))
Y <- Y[-index_NA,]
X <- X[-index_NA,]

X <- as.matrix(X)
Y <- as.vector(Y[,'Count_180'])

N <- dim(X)[1]
p <- dim(X)[2]

# We consider the SSVS prior, the spike and slab where the spike is a 
# continuous distribution (Guassian) with variance very small
c_ss <- 100
intersect <- 0.05
tau_ss <- intersect / sqrt(2 * log(c_ss) * c_ss^2/(c_ss^2 - 1))

# Data to pass to JAGS (see the code in SSVS_probit.bug):
data_JAGS_1 <- list(N = N, p = p, Y = Y, X = as.matrix(X), 
                    tau_ss = tau_ss, c_ss = c_ss)

inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), g = rep(0,p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("SSVS_poi.bug",
                 data = data_JAGS_1,
                 n.adapt = 1000,
                 inits = inits,
                 n.chains = 1)
# burn-in = 1000
update(model,n.iter=1000)
param <- c("beta0", "beta", "g", "mdl") # posterior parameters
nit <- 10000 # number of iterations
thin <- 10 #thinning
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin) # ci mette un po' a runnare questo

save(output, file='ssvs_1.dat') 
load('ssvs_1.dat')
str(output)
summary(output)
output <- as.matrix(output)

### VARIABLE SELECTION 
## criterion 1: MPM
head(output)

## we save the posterior chain of the inclusion variable in post_g
post_g <-as.matrix(output[,13:23]) 
post_mean_g <- apply(post_g, 2, "mean")

p2 <- data.frame(value = post_mean_g, var = colnames(X)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat = "identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("")
p2

# covariates selected 
mp_SSV1 <- as.vector(which(post_mean_g > 0.5))
post_mean_g[mp_SSV1]
colnames(X)
# "mean_temperature", "mean_precipitation_sum", "mean_precipitation_hours",
# "mean_windspeed_10m_max", "mean_radiation_sum", "Quota", "type_rural"     

## criterion 2: HPD
plot(output[,"mdl"], pch = 20)

# number of models visited by the posterior chain
length(unique(output[,"mdl"]))

# posterior frequency of the visited models
visited_models <- table(output[,"mdl"])
# getting the unique profiles and sort the results
unique_model <- unique(post_g, MARGIN  = 1)
freq <- apply(unique_model, 1, function(b) sum(apply(post_g, MARGIN = 1, function(a) all(a == b))))
cbind(unique_model[order(freq,decreasing = T),], sort(freq,decreasing = T))

# the HPD model is
colnames(X)[as.logical(unique_model[which.max(freq),])]

# covariates selected
HDP_SSV1 <- c(1:p)[as.logical(unique_model[which.max(freq),])]
HDP_SSV1

## criterion 3: HS
beta = as.matrix(output[,1:p])
# compute the 95% posterior credible interval for beta
CI_beta = apply(beta, 2, quantile, c(0.025, 0.975)) 

# if the credibility interval does not contain 0 then I keep the variable 
idx_cov_BL = NULL
for(l in 1:p){
  if(CI_beta[1,l]<0 && CI_beta[2,l]>0)
  {
    cat("*** variable ", colnames(X)[l], " excluded \n")
  }
  else
  {
    cat("*** variable ", colnames(X)[l], " included \n")
    idx_cov_BL = c(idx_cov_BL, l)
  }
}

mean_beta_post <- apply(beta, 2, "mean")
mean_beta_post

# all the 3 criteria give the same result