
clustered_se <- function(model, clustervar){
   # constructs clustered standard errors variance-covariance matrix

   require(sandwich)
   require(lmtest)

   M <- length(unique(clustervar))        # number of clusters
   N <- length(clustervar)                # number of observations
   K <- model$rank                        # number of parameters in the model
   dfc <- (M/(M - 1)) * ((N - 1)/(N - K)) # degrees of freedom adjustment
   uj <- apply(estfun(model), 2, function(x) tapply(x, clustervar, sum))
   rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N) # clustered standard errors variance-covariance matrix
}
