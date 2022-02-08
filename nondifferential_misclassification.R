## Explore implications of misclassification 
## Matthew Coates

rm(list=ls())
library(data.table)


a <- 100
b <- 20
c <- 50
d <- 80

two <- matrix(data=c(a,b,c,d),nrow=2,byrow=T)

mis1 <- seq(from=0,to=0.85,by=0.05) ## percent of cases that become non-cases
mis2 <- seq(from=0, to=0.85,by=0.05) ## percent of non-cases that become cases
allmis <- data.table(expand.grid(mis1=mis1,mis2=mis2))
rm(mis1); rm(mis2)

## RR calculation function
rr <- function(d=NULL) {
  (d[1,1]/(d[1,1]+d[1,2]))/(d[2,1]/(d[2,1]+d[2,2]))
}

## OR calculation function
or <- function(d=NULL) {
  (d[1,1]/(d[1,2]))/(d[2,1]/(d[2,2]))
}

## misclassification function that can be applied to a 2x2 table to create a misclassified table
misclass <- function(d=two,m1,m2,exposed) {
  ## exposed is the argument that changes whether the cases/non-cases or exposures/non-exposures are misclassified (exposed=T means exposures)
  ## m1 is the % of cases or exposed that get reclassified
  ## m2 is the % of non-cases or non-exposed that get reclassified
  if (exposed == F) {
    d2 <- copy(d)
    d2[,1] <- d[,1]*(1-m1) + d[,2]*m2
    d2[,2] <- d[,2]*(1-m2) + d[,1]*m1
    if (!all(round(apply(d,MARGIN=1,sum),5) == round(apply(d2,MARGIN=1,sum),5))) stop("Error: changing total exposed/unexposed")  
  }
  if (exposed == T) {
    d2 <- copy(d)
    d2[1,] <- d[1,]*(1-m1) + d[2,]*m2
    d2[2,] <- d[2,]*(1-m2) + d[1,]*m1
    if (!all(round(apply(d,MARGIN=2,sum),5) == round(apply(d2,MARGIN=2,sum),5))) stop("Error: changing total cases/non-cases") 
  }
  return(d2)
}

## dataset with many combinations of misclassification %s 
allmis[,rr_true:=rr(two)]
allmis[,rr:=lapply(1:nrow(allmis),FUN=function(x) {
  rr(d=misclass(two,m1=allmis$mis1[x],m2=allmis$mis2[x],exposed=F))
})]

## all biased towards null
allmis[mis1 < .5 & mis2 < .5]

allmis[mis1 <.31 & mis1 > .29 & mis2 < .11 & mis2 > .09]

rr(misclass(copy(two),m1=.3,m2=.1,exposed=F))


## try with another table
a <- 100
b <- 120
c <- 150
d <- 80
two <- matrix(data=c(a,b,c,d),nrow=2,byrow=T)

allmis[,rr_true:=rr(two)]
allmis[,rr:=lapply(1:nrow(allmis),FUN=function(x) {
  rr(d=misclass(two,m1=allmis$mis1[x],m2=allmis$mis2[x],exposed=F))
})]

## all biased towards null
allmis[mis1 < .5 & mis2 < .5]








## example from lecture slides showing that OR qualitatively biased if misclassification is > 50% (so non-differential misclassification not always biased towards null)
two <- matrix(c(150,75,350,425),nrow=2,byrow=T)
d2 <- misclass(d=two,m1=.3,m2=.3,exposed=T)
or(d2)
d2 <- misclass(d=two,m1=.7,m2=.7,exposed=T)
or(d2)


