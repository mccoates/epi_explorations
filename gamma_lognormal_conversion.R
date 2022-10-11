
library(EnvStats)

gam <- rgamma(n=10000,shape=5,rate=1)
hist(gam)

loggam <- log(gam)
hist(loggam)

lnorm <- rlnorm(n=10000, meanlog = mean(loggam), sdlog = sd(loggam))

summary(gam)
summary(lnorm)


elres <- elnorm(gam, method = "mvue", ci = FALSE)
lnorm2 <- rlnorm(n=10000, meanlog = elres$parameters["meanlog"], 
                 sdlog = elres$parameters["sdlog"])

plot(density(gam),col="red")
lines(density(lnorm),col="blue")
lines(density(lnorm2),col="green")



