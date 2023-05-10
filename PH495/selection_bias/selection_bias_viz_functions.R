## Selection bias illustration code


## PH 495 Teaching Example

## Somehow create scenario where X -> Y
## People on left versus right side of room? 
## Give e.g. half of people on left some object, 1/4 of people on the right some object
## And X -> Z
## and Y -> Z


library(data.table)
library(ggplot2)
library(ggExtra)
library(MASS)

logit <- function(x){log(x/(1-x))}
invlogit <- function(x){exp(x)/(1+exp(x))}
cor2cov_1 <- function(R,S){
  diag(S) %*% R %*% diag(S)
}

# dag {
#   "In the NBA" [adjusted,pos="-0.155,0.863"]
#   "Love Basketball" [outcome,pos="-1.413,0.807"]
#   "Super Talented" [exposure,pos="-0.697,-0.681"]
#   "Love Basketball" -> "In the NBA"
#   "Super Talented" -> "In the NBA"
#   "Super Talented" -> "Love Basketball"
# }


sim_data <- function(N=100000,eff_skilled_love=0.6){
  
  ## simulate talent
  skilled <- rbinom(n=N,size=1,prob=0.05)
  
  ## simulate love of basketball
  # love <- rbinom(n=N,size=1,prob=invlogit(-2+log(2)*skilled))
  love <- rbinom(n=N,size=1,prob=.04+eff_skilled_love*skilled)
  
  ## simulate being in the NBA
  # nba <- rbinom(n=N,size=1,prob=invlogit(-7+log(5)*love+log(3)*skilled))
  nba <- rbinom(n=N,size=1,prob=0.001+0.01*love+0.05*skilled)
  
  # summary(skilled)
  # summary(love)
  # summary(nba)
  # summary(nba[skilled==1])
  # summary(nba[skilled==0])
  # sum(nba)
  
  ## conditioning on collider
  # mod <- glm(love ~ skilled + nba,family=binomial(link="logit"))
  mod <- glm(love ~ skilled + nba,family=binomial(link="identity"))
  # summary(mod)
  
  ## not conditioning on collider
  # mod2 <- glm(love ~ skilled,family=binomial(link="logit"))
  mod2 <- glm(love ~ skilled,family=binomial(link="identity"))
  # summary(mod2)
  
  ## stratifying on NBA
  # mod3 <- glm(love[nba==1] ~ skilled[nba==1],family=binomial(link="logit"))
  mod3 <- glm(love[nba==1] ~ skilled[nba==1],family=binomial(link="identity"))
  # summary(mod3)
  
  ## stratifying on not NBA
  # mod4 <- glm(love[nba==0] ~ skilled[nba==0],family=binomial(link="logit"))
  mod4 <- glm(love[nba==0] ~ skilled[nba==0],family=binomial(link="identity"))
  # summary(mod4)
  
  # out <- data.table(data.frame(true=2,
  #                              conditioned=exp(coefficients(mod)["skilled"]),
  #                              unconditioned=exp(coefficients(mod2)["skilled"]),
  #                              strat1=exp(coefficients(mod3)["skilled[nba == 1]"]),
  #                              strat0=exp(coefficients(mod4)["skilled[nba == 0]"])))
  
  out <- data.table(data.frame(true=eff_skilled_love,
                               conditioned=coefficients(mod)["skilled"],
                               unconditioned=coefficients(mod2)["skilled"],
                               strat1=coefficients(mod3)["skilled[nba == 1]"],
                               strat0=coefficients(mod4)["skilled[nba == 0]"]))
  
  return(out)
  
}

res <- list()
for (i in 1:10) {
  cat(paste0(i,"\n")); flush.console()
  res[[i]] <- sim_data(eff_skilled_love=0)
}
res <- rbindlist(res)
resmean <- copy(res[,lapply(.SD,mean),.SDcols=c(names(res))])






## start with: 
## (1) Correlation between A and B
## (2) Effect of A on Y
## (3) Effect of B on Y
## (4) Threshold for selecting subset of Y
sim_data2 <- function(N=10000,corrAB=.8,effAY=1,effBY=-1,sthresh=0.8) {
  
  cor_mat <- matrix(c(1,corrAB,corrAB,1),byrow=T,nrow=2)
  covariance_matrix <- cor2cov_1(R=cor_mat,S=c(1,1))
  out <- data.table(as.data.frame(mvrnorm(n = N,
                                          mu = c(1000,3.4), 
                                          Sigma = covariance_matrix)))
  setnames(out,c("A","B"))
  out[,Y:=rnorm(n=N,mean=A*effAY+B*effBY,sd=1)]
  out[,Ycat:=as.numeric(Y < quantile(out$Y,probs=0.5))]
  out[,Ycat:=factor(Ycat,levels=c(1,0),labels=c("Low","High"))]
  
  test <- lm(data=out,formula="B~A")
  test1 <- lm(data=out[Ycat=="Low"],formula="B~A")
  test2 <- lm(data=out[Ycat=="High"],formula="B~A")
  
  tmp <- data.table(data.frame(
    overall=test$coefficients["A"],
    low=test1$coefficients["A"],
    high=test2$coefficients["A"]
  ))
  
  return(tmp)
  # gg <- ggplot(data=out,aes(x=A,y=B)) + theme_bw() + geom_point() + 
  #   geom_smooth(method="lm")
  # print(gg)
  # 
  # gg <- ggplot(data=out,aes(x=A,y=B,group=Ycat,color=Ycat)) + theme_bw() + geom_point() + 
  #   geom_smooth(method="lm")
  # print(gg)
}


d <- data.table(expand.grid(corrAB=c(-0.9,-0.4,0,0.4,0.9),effAY=c(-4,0,4),
                       effBY=c(-4,0,4),sthresh=seq(from=0.1,to=0.9,by=0.4)))

res <- list()
for (i in 1:nrow(d)) {
  cat(paste0(i,"\n")); flush.console()
  res[[i]] <- sim_data2(N=10000,corrAB=d$corrAB[i],effAY=d$effAY[i],
                        effBY=d$effBY[i],sthresh=d$sthresh[i])
}
res <- rbindlist(res)

res[,diff_low_high:=low-high]
res[diff_low_high > 0.02]




sim_data <- function(N=10000){
  
  # latent <- rnorm(n=N,mean=0,sd=1)
  ## scores range basically 600 to 1600 with a median of about 1050
  ## making normal in logit space, then back-transforming to that range
  #SAT <- invlogit(rnorm(n=N,mean=-5+5*latent,sd=1))
  #SAT <- (SAT - min(SAT))*((1600-600)/(max(SAT)-min(SAT))) + 600
  # SAT <- rnorm(n=N,mean=1050+200*latent,sd=(1600-1050)/(2))
  # SAT[SAT > 1600] <- 1600
  # SAT[SAT < 600] <- 600
  # hist(SAT2)
  #GPA <- invlogit(rnorm(n=N,mean=-5+5*latent,sd=1))
  #GPA <- (GPA - min(GPA))*((4.5-2.0)/(max(GPA)-min(GPA))) + 2.0
  # GPA <- rnorm(n=N,mean=,sd=(4.5-3.4)/2)
  
  cor_mat <- matrix(c(1,0,0,1),byrow=T,nrow=2)
  covariance_matrix <- cor2cov_1(R=cor_mat,S=c((1600-1000)/(3),(4.5-3.4)/3))
  
  # create bivariate normal distribution
  out <- data.table(as.data.frame(mvrnorm(n = N,
                                           mu = c(1000,3.4), 
                                           Sigma = covariance_matrix)))
  setnames(out,c("SAT","GPA"))
  out[SAT > 1600,SAT:=1600]
  out[SAT < 500,SAT:=500]
  out[GPA > 4.5,GPA:=4.5]
  out[GPA < 2,GPA:=2]

  
  ## maybe a 100 point increase in SAT should be ~2% increase in chance of admission, so
  ## 1-point is .0002
  ## maybe a 0.1 point increase in GPA should be ~2% increase in chance of admission, so
  ## 1-point is 20%
  invlogit(-16+log(1.01)*SAT+log(3)*GPA)
  out[,admitted:=rbinom(n=N,size=1,prob=invlogit(-16+log(1.01)*SAT+log(3)*GPA))]
  out[,admitted:=factor(admitted,levels=c(0,1),labels=c("Not Admitted","Admitted"))]
  
  # out <- data.table(data.frame(latent=latent,SAT=SAT,GPA=GPA,admitted=admitted))
  return(out)
  
}

# mean(invlogit(7+log(0.99)*SAT+log(3)*GPA))

d <- sim_data(N=10000)


lm(data=d[admitted=="Admitted"],formula="GPA~SAT")
lm(data=d[admitted=="Not Admitted"],formula="GPA~SAT")

gg <- ggplot(data=d,aes(x=SAT,y=GPA)) + theme_bw() + geom_point() + 
  geom_smooth(method="lm")
print(gg)

gg <- ggplot(data=d,aes(x=SAT,y=GPA,group=admitted,color=admitted)) + theme_bw() + geom_point() + 
  geom_smooth(method="lm")
print(gg)
# gg2 <- ggMarginal(gg,type="histogram")
# print(gg2)
