
rm(list=ls())
library(data.table)


##################
## model functions
##################

## strategy 1
strat1 <- function(C,E,D) {
  
  ## regress C on E
  mod1 <- lm(C ~ E)
  C_resids <- mod1$residuals
  
  ## regress D on E and residuals of C
  mod2 <- lm(D ~ E + C_resids)
  
  ## store coefficient for comparison
  return(coefficients(mod2)["E"])
  
}

## strategy 2
strat2 <- function(C,E,D) {
  
  ## regress C on E
  mod1 <- lm(C ~ 1)
  C_resids <- mod1$residuals
  
  ## regress D on E and residuals of C
  mod2 <- lm(D ~ E + C_resids)
  
  ## store coefficient for comparison
  return(coefficients(mod2)["E"])
  
}

## unconditional on C
uncond <- function(C,E,D) {
  mod1 <- lm(D ~ E)
  return(coefficients(mod1)["E"])
}

## conditional on C
cond <- function(C,E,D) {
  mod1 <- lm(D ~ E + C)
  return(coefficients(mod1)["E"])
}

## wrapping to run both models and format return to 
## keep simulation loop cleaner
runmods <- function(C,E,D,scenario) {
  
  res <- data.table(data.frame(strat1=strat1(C,E,D),strat2=strat2(C,E,D),
                                true_est=ifelse(scenario==1,cond(C,E,D),uncond(C,E,D)),
                                param_specified=ifelse(scenario==1,alpha_d1,alpha_c2*beta_d2)))
  return(res)
  
}


N <- 10000

## scenario 1 inputs
U_C1 <- 2
U_E1 <- 3
U_D1 <- 4

# paths from C
beta_e1 <- 2
beta_d1 <- 3
# paths from E
alpha_d1 <- 1

## scenario 2 inputs
U_E2 <- 3
U_C2 <- 2
U_D2 <- 4

# paths from C
beta_d2 <- 3
# paths from e 
alpha_c2 <- 1

##################################
## simulate 
##################################

## Scenario 1 (confounder) #######

## generate data
C <- rnorm(n=N,mean=U_C1,sd=.5) ## C ~ N(0,1)
E <- rnorm(n=N,mean=beta_e1,sd=0.1)*C + rnorm(n=N,mean=U_E1,sd=1) 
D <- rnorm(n=N,mean=beta_d1,sd=0.1)*C + rnorm(n=N,mean=alpha_d1,sd=0.1)*E + rnorm(n=N,mean=U_D1,sd=1)

## run model
out <- runmods(C,E,D,scenario=1)
out

## Scenario 2 (mediator)

## generate data
E <- rnorm(n=N,mean=U_E2,sd=1) 
C <- rnorm(n=N,mean=alpha_c2,sd=0.1)*E + rnorm(n=N,mean=U_C2,sd=1)
D <- rnorm(n=N,mean=beta_d2,sd=0.1)*C + rnorm(n=N,mean=U_D2,sd=1)

out <- runmods(C,E,D,scenario=2)
out





