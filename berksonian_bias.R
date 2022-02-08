
## Berksonian bias
library(data.table)

## functions to switch between probability and odds
prob_to_odds <- function(x) {x/(1-x)}
odds_to_prob <- function(x) {x/(1+x)}

## creating list objects to save results in
sourcepop <- list()
samplepop <- list()

## since we're drawing from distributions to create n obserations, we can do a number of times to see
## what the distribution of bias looks like
for (i in 1:50) {
  cat(paste0("draw ",i)); flush.console()
    
  n <- 100000
  
  ## probabilities and true odds ratios
  ## smoking, high blood pressure, and CVD probabilities
  smoking_prob <- .3
  HBP_prob <- .4
  baseline_CVD_prob <- .1
  
  ## odds of CVD with HBP
  HBP_CVD_OR <- 1.4
  ## odds of CVD with smoking
  smoking_CVD_OR <- 1.6
  
  ## baseline probability of hospitalization in population
  baseline_hosp_prob <- .05
  
  ## odds of hospitalization with HBP
  HBP_hosp_OR <- 1.5
  ## odds of hospitalization with smoking
  smoking_hosp_OR <- 1.5
  ## odds of hospitalization with CVD
  CVD_hosp_OR <- 3
  
  ## make draws of smoking and HBP (assuming independent)
  smoking <- rbinom(n=n,size=1,prob=smoking_prob)
  HBP <- rbinom(n=n,size=1,prob=HBP_prob)
  
  ## calculate CVD odds/probabilities, draw whether people have CVD (1 or 0)
  CVD_odds <- exp(log(prob_to_odds(baseline_CVD_prob))+HBP*log(HBP_CVD_OR)+smoking*log(smoking_CVD_OR))
  CVD_probs <- CVD_odds/(1+CVD_odds)
  CVD <- rbinom(length(CVD_probs),size=1,CVD_probs)
  
  ## calculate hospitalization odds/probabilities, then draw whether ppl hospitalized (1 or 0)
  hosp_odds <- exp(log(prob_to_odds(baseline_hosp_prob))+HBP*log(HBP_hosp_OR)+smoking*log(smoking_hosp_OR)+CVD*log(CVD_hosp_OR))
  hosp_probs <- hosp_odds/(1+hosp_odds)
  hosp <- rbinom(length(hosp_probs),size=1,hosp_probs)
  
  ## logistic regression to predict CVD in the general source population
  mylogit <- glm(CVD ~ HBP + smoking, family = "binomial")
  sourcepop[[i]] <- as.data.frame(t(summary(mylogit)$coefficients[,1]))
  
  ## logistic regression to predict CVD among people who are hospitalized
  mylogit <- glm(CVD[hosp==1] ~ HBP[hosp==1] + smoking[hosp==1], family = "binomial")
  samplepop[[i]] <- as.data.frame(t(summary(mylogit)$coefficients[,1]))

}

## compile results from source pop and sample pop (hospitalized)
sourcepop <- rbindlist(sourcepop)
samplepop <- rbindlist(samplepop)

## calculate percent error comparing hospitalized to source pop estimate
pct_error <- ((sourcepop$HBP-samplepop$`HBP[hosp == 1]`)/sourcepop$HBP)*100
hist(pct_error)
