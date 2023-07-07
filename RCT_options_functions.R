
library(data.table)
library(lme4)


dgp1a <- function(N) {
  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  Y0 <- rnorm(n=N,mean=10,sd=2)
  E <- as.numeric(c(1:N) %in% tx)
  Y1 <- (0 + 1*Y0 + E*(-2) + rnorm(n=N,mean=0,sd=1)) 
  id <- 1:N
  
  ## wide data for model type 1
  d_wide <- data.table(as.data.frame(cbind(Y0,E,Y1,id)))
  d_wide[,id:=factor(id)]
  
  ## long data for model type 2
  d_long <- melt(copy(d_wide),id.vars=c("id","E"),variable.name="time",value.name="Y")
  d_long[,time:=as.numeric(substr(time,2,3))]
  setnames(d_long,c("E","time"),c("arm","post"))

  # summary(lm(data=d_long,formula="Y~arm+post+arm*post"))$residuals
  
  # summary(lm(data=d_wide,formula="Y1 ~ Y0 + E"))
  
  
  # summary(lmer(data=d_long,formula="Y~arm+post+arm*post+(1|id)"))
  # 
  # summary(lmer(data=d_long,formula="Y~arm+post+arm*post+(1|id)"))$residuals
  
  
  return(list(d_wide,d_long))
}


dgp1b <- function(N) {
  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  Y0 <- rnorm(n=N,mean=10,sd=2)
  E <- as.numeric(c(1:N) %in% tx)
  Y1 <- (0 + 1*Y0 + E*(-2) + 0.1*Y0*E + rnorm(n=N,mean=0,sd=1))
  id <- 1:N
  
  ## wide data for model type 1
  d_wide <- data.table(as.data.frame(cbind(Y0,E,Y1,id)))
  d_wide[,id:=factor(id)]
  
  ## long data for model type 2
  d_long <- melt(copy(d_wide),id.vars=c("id","E"),variable.name="time",value.name="Y")
  d_long[,time:=as.numeric(substr(time,2,3))]
  setnames(d_long,c("E","time"),c("arm","post"))
  
  return(list(d_wide,d_long))
}


dgp2a <- function(N) {

  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  id <- rep(1:(N),2)
  arm <- as.numeric(id %in% tx)
  post <- c(rep(0,N),rep(1,N))
  Y <- 10 + 0*arm + 0*post - 2*post*arm + rnorm(n=N*2,mean=0,sd=2) + c(rep(0,N),rnorm(n=N,mean=0,sd=1)) 
  ## not sure about how we should simulate extra variance in Y1 (or if we should)
  ## like in DGP1
  
  ## long data for model type 2
  d_long <- data.table(as.data.frame(cbind(id,arm,post,Y)))
  d_long[,id:=factor(id)]
  
  ## wide data for model type 1
  d_wide <- dcast.data.table(copy(d_long),id+arm~post,value.var="Y")
  setnames(d_wide,c("id","arm","0","1"),c("id","E","Y0","Y1"))
  
  # summary(lmer(data=d_long,formula="Y~arm+post+arm*post+(1|id)"))
  

  return(list(d_wide,d_long))
}

dgp2b <- function(N) {
  
  ## generate data
  tx <- sample(x=1:N,size=.5*N) ## make treatment randomization exactly 50%
  id <- rep(1:(N),2)
  arm <- as.numeric(id %in% tx)
  post <- c(rep(0,N),rep(1,N))
  psi <- rep(rnorm(n=N,mean=0,sd=2),2)
  Y <- 10 + 0*arm + 0*post - 2*post*arm + psi + c(rep(0,N),rnorm(n=N,mean=0,sd=1)) 
  ## not sure about how we should simulate extra variance in Y1 (or if we should)
  ## like in DGP1
  
  ## long data for model type 2
  d_long <- data.table(as.data.frame(cbind(id,arm,post,Y)))
  d_long[,id:=factor(id)]
  
  ## wide data for model type 1
  d_wide <- dcast.data.table(copy(d_long),id+arm~post,value.var="Y")
  setnames(d_wide,c("id","arm","0","1"),c("id","E","Y0","Y1"))
  
  return(list(d_wide,d_long))
}



## calculate imbalance in baseline values
imbalance <- function(d) {
  
  tmp <- as.data.frame(matrix(sapply(1:length(d),FUN=function(x) {
    mean(d[[x]][[1]][E==1]$Y0) - mean(d[[x]][[1]][E==0]$Y0)
  }),nrow=1))

  return(tmp)
  
}


## run models and get relevant coefficients
run_model <- function(forms=formlist,d) {
  
  ## set up dataset to fill
  tmp <- matrix(data=NA,nrow=4,ncol=4)
  colnames(tmp) <- names(forms)
  row.names(tmp) <- names(d)
  
  tmp2 <- copy(tmp)
  
  for (i in names(forms)) {
    for (j in names(d)) {
      
      # cat(paste0(i," ",j,"\n")); flush.console()
    
    ## RUN MODEL ##########################
    ## determine if mixed effects or not
    if (grepl("\\|",forms[[i]])) {
      mod <- lmer(data=d[[j]][[2]],formula=forms[[i]])
    } else {
      ## determine if we need long or wide data format for model
      if (grepl("arm",forms[[i]])) {
        mod <- lm(data=d[[j]][[2]],formula=forms[[i]])
      } else {
        mod <- lm(data=d[[j]][[1]],formula=forms[[i]])
      }
    }
    
    ## EXTRACT RESULTS ###################
    coef_i <- ifelse(grepl("E",forms[i]),"E","arm:post")
    tmp[j,i] <- summary(mod)$coefficients[coef_i,1]

    
    ## G-COMPUTATION ###################
    
    ## predictions for 
    
    
    }
  }

  return(tmp)
  
}


