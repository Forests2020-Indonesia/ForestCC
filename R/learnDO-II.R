# A Bayesian approach to obtaining uncertainty estimates from neural networks
# https://blogs.rstudio.com/tensorflow/posts/2018-11-12-uncertainty_estimates_dropout/
# Bagian II

source("R/learnDO.R")


# simulated data ----

gen_data_1d <- function(n) {
  sigma <- 1
  X <- matrix(rnorm(n))
  w <- 2
  b <- 8
  Y <- matrix(X %*% w + b + sigma * rnorm(n))
  list(X, Y)
}

c(X, Y) %<-% gen_data_1d(n_train + n_val)

c(X_train, Y_train) %<-% list(X[1:n_train], Y[1:n_train])
c(X_val, Y_val) %<-% list(X[(n_train + 1):(n_train + n_val)], 
                          Y[(n_train + 1):(n_train + n_val)])

model %>% compile(
  optimizer = "adam",
  loss = heteroscedastic_loss,
  metrics = c(custom_metric("heteroscedastic_loss", heteroscedastic_loss))
)

history <- model %>% fit(
  X_train,
  Y_train,
  epochs = 30,
  batch_size = 10
)

num_MC_samples <- 20

MC_samples <- array(0, dim = c(num_MC_samples, n_val, 2 * output_dim))
for (k in 1:num_MC_samples) {
  MC_samples[k, , ] <- (model %>% predict(X_val))
}

# the means are in the first output column
means <- MC_samples[, , 1:output_dim]  
# average over the MC samples
predictive_mean <- apply(means, 2, mean)

epistemic_uncertainty <- apply(means, 2, var)

logvar <- MC_samples[, , (output_dim + 1):(output_dim * 2)]
aleatoric_uncertainty <- exp(colMeans(logvar))

df <- data.frame(
  x = X_val,
  y_pred = predictive_mean,
  e_u_lower = predictive_mean - sqrt(epistemic_uncertainty),
  e_u_upper = predictive_mean + sqrt(epistemic_uncertainty),
  a_u_lower = predictive_mean - sqrt(aleatoric_uncertainty),
  a_u_upper = predictive_mean + sqrt(aleatoric_uncertainty),
  u_overall_lower = predictive_mean - 
    sqrt(epistemic_uncertainty) - 
    sqrt(aleatoric_uncertainty),
  u_overall_upper = predictive_mean + 
    sqrt(epistemic_uncertainty) + 
    sqrt(aleatoric_uncertainty)
)
ggplot(df, aes(x, y_pred)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = e_u_lower, ymax = e_u_upper), alpha = 0.3)

# dataset_frci <- readRDS("PROCESSED_DATA/SET_SAMPLES/dataset_frci.RDS")
