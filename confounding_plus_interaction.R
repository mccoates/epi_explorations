
library(regclass)
library(data.table)
set.seed(94710)


## SCENARIO 1: CONFOUNDER FOR EXPOSURE 1

## set up objects to store results in
out <- list()
out2 <- list()
out3 <- list()
out4 <- list()

for (i in 1:1000) {
  
  cat(paste0("iteration ",i,"\n")); flush.console()
  
  ## define confounder first so that it can be used to define exposure and outcome
  confounder <- rnorm(n=1000,mean=10,sd=1)
  
  ## define exposure 1 as a function of the confounder plus some error
  exposure1 <-  confounder*1.5 + rnorm(n=1000,mean=1,sd=.1)
  
  ## make exposure2 independent for now
  exposure2 <- rnorm(n=1000,mean=10,sd=1)
  
  all_correlations(as.data.frame(cbind(confounder,exposure1,exposure2)))
  
  beta0 <- 10
  beta1 <- 4
  beta2 <- 0 ## make exposure unrelated to outcome except through confounder
  beta3 <- 2
  beta4 <- 3 ## make exposure unrelated to outcome except through confounder
  
  outcome <- beta0 + confounder*beta1 + exposure1*beta2 + exposure2*beta3 + exposure1*exposure2*beta4
  
  mod <- lm(outcome~exposure1 + exposure2 + exposure1*exposure2)
  out[[i]] <- data.table(t(as.data.frame(summary(mod)$coefficients[,1])))
  
  mod2 <- lm(outcome~ confounder + exposure1 + exposure2 + exposure1*exposure2)
  out2[[i]] <- data.table(t(as.data.frame(summary(mod2)$coefficients[,1])))
  
  mod3 <- lm(outcome~ exposure2 + exposure1:exposure2)
  out3[[i]] <- data.table(t(as.data.frame(summary(mod3)$coefficients[,1])))

  mod4 <- lm(outcome~ confounder + exposure2 + exposure1:exposure2)
  out4[[i]] <- data.table(t(as.data.frame(summary(mod4)$coefficients[,1])))  
}
out <- rbindlist(out)
out2 <- rbindlist(out2)
out3 <- rbindlist(out3)
out4 <- rbindlist(out4)

summary(out$exposure1)
summary(out$exposure2)
summary(out$`exposure1:exposure2`)
summary(out2$`exposure1:exposure2`)
summary(out3$`exposure2:exposure1`)
summary(out4$`exposure2:exposure1`)





## SCENARIO 2: CONFOUNDER FOR BOTH EXPOSURES

out <- list()
out2 <- list()
for (i in 1:1000) {
  
  cat(paste0("iteration ",i,"\n")); flush.console()
  
  ## define confounder first so that it can be used to define exposure and outcome
  confounder <- rnorm(n=1000,mean=10,sd=1)
  
  ## define exposure 1 as a function of the confounder plus some error
  exposure1 <-  confounder*1.5 + rnorm(n=1000,mean=0,sd=2)
  
  ## make exposure2 independent for now
  exposure2 <- confounder*2 + rnorm(n=1000,mean=0,sd=.1)
  
  beta0 <- 10
  beta1 <- 4
  beta2 <- 0 ## make exposure unrelated to outcome except through confounder
  beta3 <- 2
  beta4 <- 1.5
  
  outcome <- beta0 + confounder*beta1 + exposure1*beta2 + exposure2*beta3 + exposure1*exposure2*beta4
  
  all_correlations(as.data.frame(cbind(confounder,exposure1,exposure2,outcome)))
  
  
  mod <- lm(outcome~exposure1 + exposure2 + exposure1*exposure2)
  out[[i]] <- data.table(t(as.data.frame(summary(mod)$coefficients[,1])))
  
  mod2 <- lm(outcome~ confounder + exposure1 + exposure2 + exposure1*exposure2)
  out2[[i]] <- data.table(t(as.data.frame(summary(mod2)$coefficients[,1])))
  
  
}
out <- rbindlist(out)
out2 <- rbindlist(out2)

summary(out$exposure1)
summary(out$exposure2)
summary(out$`exposure1:exposure2`)

