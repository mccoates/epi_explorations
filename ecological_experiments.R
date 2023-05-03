
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




## regress attitudes on facility and treatment to get model of data generating process for exposure
# attitudes_mod <- glmer(data=d,formula="attitudes_all ~ treatment + (1 | cid)",family=gaussian(link="identity"))
# attitudes_mod_res <- summary(attitudes_mod)
# ## save an estimate of treatment effect
# treatment_eff <- attitudes_mod_res$coefficients["treatment","Estimate"]


# attitudes_mod <- glmer(data=d,formula="attitudes_all ~ treatment + (1 | cid)",family=gaussian(link="identity"))
# attitudes_mod <- glm(data=simd,formula="attitudes_all ~ treatment",family=gaussian(link="identity"))
# summary(attitudes_mod)






