
clustered_se <- function(model, clustervar){
   # constructs clustered standard errors variance-covariance matrix

   M <- length(unique(clustervar))        # number of clusters
   N <- length(clustervar)                # number of observations
   K <- model$rank                        # number of parameters in the model
   dfc <- (M/(M - 1)) * ((N - 1)/(N - K)) # degrees of freedom adjustment
   uj <- as_tibble(cbind(clustervar, sandwich::estfun(model))) %>%
      group_by(clustervar) %>%
      summarise_all(sum) %>%
      select(-clustervar) %>%
      as.matrix()
   rcse.cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N) # clustered standard errors variance-covariance matrix
}
