## try to replicate simulation from
## Murray et al. (2020)
## Matthew Coates


rm(list=ls())
library(data.table)

## define useful functions
expit <- function(x) {exp(x)/(1+exp(x))}

N <- 10000
niter <- 1:1000


## parameters from Appendix Table 1

## Exponential parameter for T_0bar, lambda_0(t)
lambda0 <- 0.010

## Treatment effect parameter
psi_null <- 0
psi_beneficial <- -1

## Conditional probability distribution for L_k
A_k_m1 <- 0.675
gamma1 <- 25
kappa <- -11.2
tau <- 0.38

## Conditional probability distribution for A_k, logistic regression model parameters
intercept <- 0.1
L_k <- -0.3
L_km1 <- -0.25
L_km2 <- -0.10



## first, example from DAG without the more complicated AFT model
## to think about issues
U <- rbinom(n=N,size=1,prob=0.5)
A0 <- rbinom(n=N,size=1,prob=0.2) 
Lt <- rbinom(n=N,size=1,prob=expit(-1 + .5*U-.6*A0))
At <- Lt ## there was no arrow from A0 to At...
Y <- rbinom(n=N,size=1,prob=expit(-2 + .2*U - 0.5*At + .2*Lt - 0.2*A0))

## now, we can estimate effect of At on Y
## and total effect of A0 on Y
## and can get effect of A0 on Y that is not through At



