
    ## logit function
    logit <- function(x) {log(x/(1-x))}
    
    ## expit function
    expit <- function(x) {1/(1+(exp(-1*x)))}

    
    ###################################
    ## Logistic Risk Model (Log Odds Model)
    ###################################
    set.seed(123)    
    N <- 100000
    Z <- rbinom(n=N,size=1,prob=0.4)
    X <- rbinom(n=N,size=1,prob=expit(logit(0.3) + log(1.2)*Z))
    Y <- rbinom(n=N,size=1,prob=expit(logit(0.4) + log(0.7)*X + log(1.4)*Z))
    
    glm(Y~X+Z,family=binomial(link="logit"))
    glm(X~Z,family=binomial(link="logit"))
    glm(Z~X,family=binomial(link="logit"))
    ## same coefficients
    
    ###################################
    ## Log-linear risk model
    ###################################
    set.seed(123)    
    N <- 100000
    Z <- rbinom(n=N,size=1,prob=0.4)
    X <- rbinom(n=N,size=1,prob=exp(log(0.3) + log(1.2)*Z))
    Y <- rbinom(n=N,size=1,prob=exp(log(0.4) + log(0.7)*X + log(1.4)*Z))
    
    glm(Y~X+Z,family=binomial(link="log"))
    glm(X~Z,family=binomial(link="log"))
    glm(Z~X,family=binomial(link="log"))
    ## not same coefficients?
    
    ###################################
    ## Linear risk model
    ###################################
    set.seed(123)    
    N <- 100000
    ## only works for certain betas
    Z <- rbinom(n=N,size=1,prob=0.1)
    X <- rbinom(n=N,size=1,prob=0.3 + .2*Z)
    Y <- rbinom(n=N,size=1,prob=0.05 +0.3*X + .4*Z)
    
    glm(Y~X+Z,family=binomial(link="identity"))
    glm(X~Z,family=binomial(link="identity"))
    glm(Z~X,family=binomial(link="identity"))
    ## not same coefficients?
    
    
    
    ####################################
    ## Linear Mean Model
    ####################################
    set.seed(123)    
    N <- 1000
    Z <- rnorm(n=N,mean=5,sd=.1)
    X <- rnorm(n=N,mean=(1+2*Z),sd=0.1)
    Y <- rnorm(n=N,mean=(2+3*X+4*Z),sd=0.001)
        
    sd(Z)^2

    lm(Y~X+Z)
    lm(X~Z)
    lm(Z~X)
    
    (sd(X)^2/sd(Z)^2)
    
    
    ## coefficients are inverse
    
        # X = 1 + 2Z
    # X - 1 = 2Z
    # Z = 1/2X - 1
    dat <- data.frame(x=X,y=Y,z=Z)
    ggplot(data=dat,aes(x=x,y=z)) + geom_point() 
    
    
    ####################################
    ## Log-Linear Rate Model
    ####################################
    set.seed(123)    
    N <- 1000
    ## need to simulate differently
    # Z <- rnorm(n=N,mean=5,sd=1)
    # X <- rnorm(n=N,mean=exp((log(1)+log(2)*Z)),sd=0.00001)
    # Y <- rnorm(n=N,mean=exp(log(2)+log(3)*X+log(4)*Z),sd=0.001)
    # 
    # glm(Y~X+Z,family=poisson(link="log"))
    # glm(X~Z,family=poisson(link="log"))
    # glm(Z~X,family=poisson(link="log"))
    
    
    
    