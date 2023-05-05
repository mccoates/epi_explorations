
## Some ecological analysis exploration

rm(list=ls())
library(data.table)
library(haven)
library(lme4)

## understand data
## provider data
d <- data.table(read_dta("C:/Users/MattC/Downloads/providers.dta"))
table(d$cid)
d[,id:=1:nrow(d)]

## client data
d2 <- data.table(read_dta("C:/Users/MattC/Downloads/clients.dta"))


## first simulation
ind_effect <- -1
tx_effect <- 0.2
mean_effect <- 0

res <- c()
for (i in 1:1000) {
  cat(paste0(i,"\n")); flush.console()
  providerN <- nrow(d)
  simd <- data.table(data.frame(id=sample(1:nrow(d),size=providerN,replace=T)))
  simd <- merge(simd,d,by=c("id"),all.x=T)
  simd[,mean_attitudes:=mean(attitudes_all),by=c("cid")]
  simd[,pall_me_mean_st:=ind_effect*attitudes_all + treatment*tx_effect + mean_effect*mean_attitudes]
  
  ## find what we'd estimate for the mean
  groupeff <- glm(data=simd,formula="pall_me_mean_st ~ mean_attitudes + treatment",family=gaussian(link="identity"))
  res <- c(res,coefficients(groupeff)[2])
}

summary(res)



test <- copy(d[treatment == 0])
test[,pall_me_mean_st:=-1*attitudes_all+rnorm(n=nrow(test),mean=0,sd=0.2)]
test[,mean_attitudes:=mean(attitudes_all),by=c("cid")]

lm(data=test,formula="pall_me_mean_st~mean_attitudes")
lm(data=test,formula="pall_me_mean_st~attitudes_all")

gg <- ggplot() + geom_point(data=test,aes(x=mean_attitudes,y=pall_me_mean_st),color="blue") + 
  geom_point(data=test,aes(x=attitudes_all,y=pall_me_mean_st),color="red")
print(gg)

gg <- ggplot(data=test,aes(x=attitudes_all,y=pall_me_mean_st)) + geom_point()
print(gg)



## second simulation
ind_effect <- -1
tx_effect <- 0.5
mean_effect <- 0

res <- c()
for (i in 1:1000) {
  cat(paste0(i,"\n")); flush.console()
  simd <- copy(d)
  ## reassign provider attitudes randomly with respect to facility, conditional on treatment
  
  ## draw some facility-level latent variable that will have to do with 
  
  simd[,attitudes_all:=0.2*treatment+3+rnorm(n=nrow(simd),mean=0,sd=0.5)] ## this won't actually have the right distribution, but gets at what we're interested in
  simd[,mean_attitudes:=mean(attitudes_all),by=c("cid")]
  simd[,pall_me_mean_st:=ind_effect*attitudes_all + treatment*tx_effect + mean_effect*mean_attitudes + rnorm(n=nrow(simd),mean=0,sd=0.5)]
  
  ## find what we'd estimate for the mean
  groupeff <- glm(data=simd,formula="pall_me_mean_st ~ mean_attitudes + treatment",family=gaussian(link="identity"))
  res <- c(res,coefficients(groupeff)[2])
}

summary(res)

attitudes_mod <- glmer(data=simd,formula="attitudes_all ~ treatment + (1 | cid)",family=gaussian(link="identity"))
summary(attitudes_mod)



eff_xy <- 2
N <- 1000

res <- c()
res2 <- c()

for (i in 1:1000) {
  cat(paste0(i,"\n")); flush.console()
  
  test <- data.table(data.frame(x=rnorm(n=N,mean=0,sd=1),group=rep(seq(from=1,to=20),N/20)))
  test[,meanx:=mean(x),by=c("group")]
  test[,y:=2+x*eff_xy+rnorm(n=N,mean=0,sd=1)]
  
  mod <- lm(data=test,formula="y~meanx")
  mod2 <- lm(data=test,formula="y~x")
  
  res <- c(res,coefficients(mod)[2])  
  res2 <- c(res2,coefficients(mod2)[2])  
  
}
summary(res)
sd(res)
summary(res2)
sd(res2)



eff_xy <- 2
eff_zx <- -3
eff_zy <- 5
N <- 1000

res <- c()
res2 <- c()
res3 <- c()

for (i in 1:1000) {
  cat(paste0(i,"\n")); flush.console()
  
  z <- rnorm(n=N,mean=0,sd=1)
  test <- data.table(data.frame(z=z,x=rnorm(n=N,mean=z*eff_zx,sd=1),group=rep(seq(from=1,to=20),N/20)))
  test[,meanx:=mean(x),by=c("group")]
  test[,y:=2+x*eff_xy+z*eff_zy+rnorm(n=N,mean=0,sd=1)]
  test[,meanz:=mean(z),by=c("group")]
  test[,meany:=mean(y),by=c("group")]
  
  mod <- lm(data=test,formula="y~meanx+meanz")
  mod2 <- lm(data=test,formula="y~x+z")
  mod3 <- lm(data=unique(test[,c("group","meanx","meanz","meany"),with=F]),formula="meany~meanx+meanz")
  
  res <- c(res,coefficients(mod)[2])  
  res2 <- c(res2,coefficients(mod2)[2])  
  res3 <- c(res3,coefficients(mod3)[2])  
  
}
summary(res)
sd(res)
summary(res2)
sd(res2)
summary(res3)
sd(res3)




## regress attitudes on facility and treatment to get model of data generating process for exposure
# attitudes_mod <- glmer(data=d,formula="attitudes_all ~ treatment + (1 | cid)",family=gaussian(link="identity"))
# attitudes_mod_res <- summary(attitudes_mod)
# ## save an estimate of treatment effect
# treatment_eff <- attitudes_mod_res$coefficients["treatment","Estimate"]


# attitudes_mod <- glmer(data=d,formula="attitudes_all ~ treatment + (1 | cid)",family=gaussian(link="identity"))
# attitudes_mod <- glm(data=simd,formula="attitudes_all ~ treatment",family=gaussian(link="identity"))
# summary(attitudes_mod)






