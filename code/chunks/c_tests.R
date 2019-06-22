#### test weights ####

df <- data.frame(
    sex = rep(1:2, each = 2),
    weight = 1:4)

df %>%
    group_by(sex) %>%
    summarise(weight = sum(weight)) %>%
    mutate(shr =  weight / sum(weight))

df %>%
    count(sex, wt = weight) %>%
    mutate(weight = prop.table(n))



#### test clustered errors ####

clustered_se <- function(model, cluster){
    require(sandwich)
    require(lmtest)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
    model$rcse.vcov <- dfc * sandwich(model, meat = crossprod(uj)/N)
    model$rcse.ttest <- coeftest(model, model$rcse.vcov)
    return(model)
}

mylm <- lm(mpg ~ wt, data = mtcars)
coeftest(mylm)

mylm <- clustered_se(mylm, mtcars$cyl)
mylm$rcse.ttest

stargazer(mylm, type = "text")
stargazer(mylm, type = "text",
          t  = list(mylm$rcse.ttest[, "t value"]),
          se = list(mylm$rcse.ttest[,"Std. Error"]),
          p  = list(mylm$rcse.ttest[,"Pr(>|t|)"]))



#### test stargazer fix for omitted variables ####

remotes::install_github("jduras/stargazer")
library(stargazer)

stargazer(df.lm.results.ttl$lm.model[c(1,2,7)], type = "text",
          omit = c("^factor\\(tudiaryday","^demo.", "^factor\\(tuyear","^factor\\(gestfips"),
          omit.labels = c("day of week dummies", "demographic controls", "year dummies", "state dummies"))
