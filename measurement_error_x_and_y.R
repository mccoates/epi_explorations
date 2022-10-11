
res <- c()

for (i in 1:1000) {
  A <- rnorm(n=1000,mean=0,sd=1)
  Y <- A*2+rnorm(n=1000,mean=0,sd=1)
  Ua <- rnorm(n=1000,mean=0,sd=1)
  Astar <- A + Ua
  
  mod1 <- lm(Y~A)
  mod2 <- lm(Y~Astar)
  res[i] <- mod1$coefficients[2]-mod2$coefficients[2]
}

res


plot(A,Astar)
plot(A,Y)
plot(Astar,Y)


A <- rnorm(n=1000,mean=0,sd=1)
Y <- A*2+rnorm(n=1000,mean=1,sd=1)
Ua <- rnorm(n=1000,mean=0,sd=1)
Astar <- A + Ua
mod1 <- lm(Y~A)
mod2 <- lm(Y~Astar)

mod1
mod2

plot(A,Y,xlim=c(-5,5),ylim=c(-5,10))
plot(Astar,Y,xlim=c(-5,5),ylim=c(-5,10))


for (i in 1:1000) {
  
A <- rnorm(n=1000,mean=0,sd=1)
Y <- A*2+rnorm(n=1000,mean=1,sd=1)
Uy <- rnorm(n=1000,mean=0,sd=10)
Ystar <- Y + Uy
mod1 <- lm(Y~A)
mod2 <- lm(Ystar~A)
res[i] <- mod1$coefficients[2]-mod2$coefficients[2]
}

res
summary(res)

mod1
mod2

plot(A,Y,xlim=c(-5,5),ylim=c(-5,10))
plot(Astar,Y,xlim=c(-5,5),ylim=c(-5,10))


x <- rnorm(10000,mean=1,sd=1)
y1 <- x*2+rnorm(10000,mean=0,sd=1)
y2 <- x*2+rnorm(10000,mean=0,sd=3)
y3 <- x*2+rnorm(10000,mean=0,sd=5)

mod1 <- lm(y1~x)
mod2 <- lm(y2~x)
mod3 <- lm(y3~x)

summary(mod1)
summary(mod2)
summary(mod3)
