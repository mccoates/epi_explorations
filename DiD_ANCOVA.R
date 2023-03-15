## Matthew Coates
## Simulation for DiD/Longitudinal ANCOVA

rm(list=ls())
library(data.table)
library(ggplot2)

## define useful functions
expit <- function(x) {exp(x)/(1+exp(x))}

## small number to have more likelihood for imbalance
N <- 20
niter <- 1:1000


############################
## Y0 and E cause Y1
############################

## DAG structure
# dag {
#   E [exposure,pos="0.167,0.126"]
#   Y0 [pos="0.180,0.780"]
#   Y1 [outcome,pos="0.752,0.464"]
#   E -> Y1
#   Y0 -> Y1
# }

bd <- c()
m1 <- c()
m2 <- c()
for (i in niter) {
  cat(paste0(i)); flush.console()
  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  ## continuous first because it seems easier
  Y0 <- rnorm(n=N,mean=10,sd=1.5)
  E <- as.numeric(c(1:N) %in% tx)
  Y1 <- (1*Y0 + E*(-2) + rnorm(n=N,mean=0,sd=.2))
  id=1:N
  d <- data.table(as.data.frame(cbind(Y0,E,Y1,id)))
  
  ## data formatted for DiD
  d2 <- melt(copy(d),id.vars=c("id","E"),variable.name="time",value.name="Y")
  d2[,time:=as.numeric(substr(time,2,3))]
  d2[,id:=factor(id)]
  
  ## find imbalance in baseline values
  bd[i] <- mean(d[E==1]$Y0)-mean(d[E==0]$Y0)
  
  ## regression models
  mod1 <- lm(data=d2,formula="Y ~ E + time + E*time")
  # did2 <- lmer(data=d2,formula="Y ~ E + time + E*time + (1 | id)") in this case exactly the same
  mod2 <- lm(data=d,formula="Y1 ~ Y0 + E")
  m1[i] <- summary(mod1)$coefficients["E:time","Estimate"]
  m2[i] <- summary(mod2)$coefficients["E","Estimate"]
    
}

m1diff <- abs(m1+2)
m2diff <- abs(m2+2)
mean(m1)
mean(m2)
summary(lm(m1diff ~ bd))
summary(lm(m2diff ~ bd))
mean(bd)



############################
## Y0 interacts with E
############################

## DAG structure
# dag {
#   "E*Y0" [pos="0.206,0.357"]
#   E [exposure,pos="-0.239,-0.459"]
#   Y0 [pos="-0.129,1.076"]
#   Y1 [outcome,pos="0.752,0.464"]
#   "E*Y0" -> Y1
#   E -> "E*Y0"
#   E -> Y1
#   Y0 -> "E*Y0"
#   Y0 -> Y1
# }


bd <- c()
m1 <- c()
m2 <- c()
marg <- c()
for (i in niter) {
  cat(paste0(i)); flush.console()
  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  ## continuous first because it seems easier
  Y0 <- rnorm(n=N,mean=10,sd=1.5)
  E <- as.numeric(c(1:N) %in% tx)
  Y1 <- (1*Y0 + E*(-2) + -0.1*Y0*E + rnorm(n=N,mean=0,sd=.2))
  id=1:N
  d <- data.table(as.data.frame(cbind(Y0,E,Y1,id)))
  
  ## since we have interaction here, find true marginal effect in total sample
  marg[i] <- mean(1*-2 + -0.1*d$Y0*1)
  
  ## data formatted for DiD
  d2 <- melt(copy(d),id.vars=c("id","E"),variable.name="time",value.name="Y")
  d2[,time:=as.numeric(substr(time,2,3))]
  d2[,id:=factor(id)]
  
  ## find imbalance in baseline values
  bd[i] <- mean(d[E==1]$Y0)-mean(d[E==0]$Y0)
  
  ## regression models
  mod1 <- lm(data=d2,formula="Y ~ E + time + E*time")
  # did2 <- lmer(data=d2,formula="Y ~ E + time + E*time + (1 | id)") in this case exactly the same
  mod2 <- lm(data=d,formula="Y1 ~ Y0 + E")
  m1[i] <- summary(mod1)$coefficients["E:time","Estimate"]
  m2[i] <- summary(mod2)$coefficients["E","Estimate"]
  
}

m1absdiff <- abs(m1-marg)
m2absdiff <- abs(m2-marg)
m1m2absdiff <- abs(m1-m2)

m1diff <- m1-marg
m2diff <- m2-marg
m1m2diff <- m1-m2

mean(m1)
mean(m2)
mean(m1absdiff)
mean(m2absdiff)
## both have similar error though?
summary(lm(m1diff ~ bd)) ## the amount off does appear to be related to the baseline difference
summary(lm(m2diff ~ bd)) ## the amount off doesn't appear to be related to the baseline difference

summary(lm(m1absdiff ~ bd)) ## is amount off related to the baseline difference?
summary(lm(m2absdiff ~ bd)) ## the amount off doesn't appear to be related to the baseline difference

## the difference between the two models depends on the baseline difference
summary(lm(m1m2absdiff ~ bd))
summary(lm(m1m2diff ~ bd))

mean(bd)

res <- data.table(data.frame(baseline_difference=bd,DiD_error_abs=m1absdiff,ANCOVA_error_abs=m2absdiff,
                             DiD_ANCOVA_diff=m1m2diff,abs_DiD_ANCOVA_diff=abs(m1m2absdiff),
                             DiD_error=m1diff,ANCOVA_error=m2diff),
                             diff_errors=m1diff-m2diff, diff_abs_errors=m1absdiff-m2absdiff)
res <- melt(res,id.vars="baseline_difference")
res[variable=="DiD_error_abs",variable:="DiD Absolute Error"]
res[variable=="ANCOVA_error_abs",variable:="ANCOVA Absolute Error"]
res[variable=="DiD_ANCOVA_diff",variable:="DiD minus ANCOVA Estimate"]
res[variable=="abs_DiD_ANCOVA_diff",variable:="Absolute value of DiD Minus ANCOVA Estimate"]
res[variable=="DiD_error",variable:="DiD Error"]
res[variable=="ANCOVA_error",variable:="ANCOVA Error"]
res[variable=="diff_errors",variable:="DiD Error minus ANCOVA Error"]
res[variable=="diff_abs_errors",variable:="Difference in Absolute Values of Errors"]


gg <- ggplot(data=res[variable %in% c("DiD Error","ANCOVA Error")],aes(x=baseline_difference,y=value)) + geom_point() + theme_bw() +
  facet_wrap(~variable) + geom_smooth(method='lm',se = FALSE) + ylab("Error from True Marginal Effect in Sample") + 
  xlab("Difference in Mean Exposure at Baseline")
print(gg)



gg <- ggplot(data=res,aes(x=bd,y=errordiff)) + geom_point()
print(gg)
gg <- ggplot(data=res,aes(x=bd,y=abserrordiff)) + geom_point()
print(gg)

## does data generating process matter?? which is "correct" data generating process for whether
## DiD or ancova better?

## so they have basically same mean error, but one seems to have error more correlated with baseline difference (DiD)
## when does the other have more error to balance it out?
## how much error can this lead to--does it make a real difference?
## Question: in cases where you have large baseline differences, are you actually better off with ANCOVA?
## identify cases with large baseline differences and see which method results in smaller error

##AUGMENT THE DAG TO SEE DiD???
## VERSUS OTHER APPROACH? CAN WE SEE BOTH ON THE SAME DAG?
## Equations with edges labeled


## visual of DiD?
# dag {
#   "E*Time" [pos="0.264,0.394"]
#   E [exposure,pos="0.203,0.163"]
#   Time [pos="0.127,0.620"]
#   Y [outcome,pos="0.430,0.412"]
#   "E*Time" -> Y
#   E -> "E*Time"
#   E -> Y
#   Time -> "E*Time"
#   Time -> E
#   Time -> Y
# }






# ## simulate (try many different types)
# Y0 <- rbinom(n=N,size=1,prob=.3)
# E <- rbinom(n=N,size=1,prob=.5)
# Y1 <- rbinom(n=N,size=1,expit(-1.2+log(2)*E+log(2)*Y0+log(1.5)*Y0*E)) ## think about intercept basically equaling Y0 with some noise?
# 
# ## true marginal effect?
# 
# ## ANCOVA approach
# mod <- glm(Y1 ~ E + Y0 + ,family=binomial(link="logit"))




# ## Y0 and U both interact with E, U is an unmeasured cause of Y0 and Y1 (but not E)
# dag {
#   "E*U" [pos="-0.099,0.398"]
#   "E*Y0" [pos="0.253,0.198"]
#   E [exposure,pos="-0.239,-0.459"]
#   U [pos="-0.704,0.938"]
#   Y0 [pos="-0.129,1.076"]
#   Y1 [outcome,pos="0.752,0.464"]
#   "E*U" -> Y1
#   "E*Y0" -> Y1
#   E -> "E*U"
#   E -> "E*Y0"
#   E -> Y1
#   U -> "E*U"
#   U -> Y0
#   U -> Y1
#   Y0 -> "E*Y0"
#   Y0 -> Y1
# }
# 
# 
# 
# ## Baseline as proxy for EMM
# dag {
#   "E*U" [pos="-0.138,0.352"]
#   E [exposure,pos="-0.594,-0.516"]
#   U [pos="-0.704,0.938"]
#   Y0 [pos="-0.086,1.492"]
#   Y1 [outcome,pos="0.752,0.464"]
#   "E*U" -> Y1
#   E -> "E*U"
#   E -> Y1
#   U -> "E*U"
#   U -> Y0
#   U -> Y1
# }
# 
# ## Baseline as proxy for EMM
# dag {
#   "E*U" [pos="-0.138,0.352"]
#   E [exposure,pos="-0.594,-0.516"]
#   U [pos="-0.704,0.938"]
#   Y0 [pos="-0.086,1.492"]
#   Y1 [outcome,pos="0.752,0.464"]
#   "E*U" -> Y1
#   E -> "E*U"
#   E -> Y1
#   U -> "E*U"
#   U -> Y0
#   U -> Y1
# }
# 
# ## Baseline EMM (with some common cause of Y0 and Y1)
# dag {
#   "E*Y0" [pos="-0.536,0.432"]
#   E [exposure,pos="-0.594,-0.516"]
#   U [pos="0.344,1.324"]
#   Y0 [pos="-0.060,0.737"]
#   Y1 [outcome,pos="0.752,0.464"]
#   "E*Y0" -> Y1
#   E -> "E*Y0"
#   E -> Y1
#   U -> Y0
#   U -> Y1
#   Y0 -> "E*Y0"
#   Y0 -> Y1
# }
# 
# ## uncontrolled confounder of Y0, Y1, and E
# dag {
#   E [exposure,pos="-0.138,0.400"]
#   U [pos="0.187,1.476"]
#   Y0 [pos="-0.704,0.432"]
#   Y1 [outcome,pos="0.752,0.464"]
#   E -> Y1
#   U -> E
#   U -> Y0
#   U -> Y1
# }


