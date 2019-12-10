#q3
#Assuming average case scenario for all assumptions what is the estimated porfolio value
pacman::p_load(MASS,DEoptim,tidyverse)
savings <- 100000
grwothrate <- 0.03
for (i in 2:length(years)){
  savings <- 100000
  growthrate <- 0.03
  income <- rep.int(145000,25)
  income[i] <- savings + income[i-1]*growthrate
  income[1] <- 145000
  contribution_rate <- rtri(1,0.01,0.03,0.06)
  contribution[i] <- income[i]*contribution_rate
}
rf <- 0.023
rmarket <- 0.011
return_matrix <- c(0.023,0.11)
#weight
return_optim <- function(weights){
  if (sum(weight)==0)
    weights <- weights+1e-4
  weights <- weights/sum(weights)
  return_matrix <- c(0.023,0.11)
  pr_return <- weights %*% return_matrix
  pf_sd <- sd(pf_return)
    return(Inf) #risk constraint at 96% level
  return(-mean(pf_return)
}
optimum <- DEoptim(return_optim,rep(0,5),rep(1,5))
allocation <- optimum$optim$bestmem/sum(optimum$optim$bestmem)

#Simulate various scenarios and find out what is the optimal allocation of resources between Capital Markets and Berkshire Tramway so that Liz can achieve her goal with 96% probability

#assume that Liz start investing her salary this year and the salary grows next year.
#choose the portfolio using the data from the first year
savings <- 100000
income_value <- 145000
growthrate <- runif(1,min = 0.01, max = 0.05)
years <- c(1:25)
#GENERATE TRIANGULARLY DISTRIBUTED RV's
rtri<-function(n,min,ml,max){
  qtri<-function(U){
    F<-(ml-min)/(max-min)
    if (U<F) {min+(U*(max-min)*(ml-min))^.5}
    else {max-((1-U)*(max-min)*(max-ml))^.5}
  }
  y<-runif(n)
  sapply(y,qtri)
}

#generating annual contribution

growthrate <- runif(1,min=0.01,max=0.05)
for (i in 2:length(years)){
  savings <- 100000
  growthrate <- runif(1,min=0.01,max=0.05)
  income <- rep.int(145000,25)
  income[i] <- savings + income[i-1]*growthrate
  income[1] <- 145000
  contribution_rate <- rtri(1,0.01,0.03,0.06)
  contribution[i] <- income[i]*contribution_rate
}

#generating return matrix
rf <- 0.023
rmarket <- rnorm(1,0.11,0.065)
return_matrix <- c(0.023,0.11)


#weights
threshold_sd <- 0.04
n_trials <- 100
rf <- 0.023
rmarket <- rnorm(1,0.11,0.065)
return_optim <- function(weights){
  if (sum(weight)==0)
    weights <- weights+1e-4
  weights <- weights/sum(weights)
  return_matrix <- c(0.023,0.11)
  pr_return <- weights %*% return_matrix
  pf_sd <- sd(pf_return)
  if(pf_sd>threshold_sd)
    return(Inf) #risk constraint at 96% level
  return(-mean(pf_return)
}
optimum <- DEoptim(return_optim,rep(0,5),rep(1,5))

#need return matrix

#Tail Risk is defined as the expected value of the worst case outcome. To minimize the 4% tail risk what is the optimal portfolio allocation that Liz should allocate to achieve her goal of meeting 1,200,000 in 25 years.
return_est <- function(weights){
  return_risk <- c()
  for (i in 1:n_trials){
    pr_return <- weights %*% return_matrix
    for (k in min(pr_return):quantile(pr_return,0.04)[[1]]) {
      tr <- 0
      tr <- tr+sum(pr_return==k)*k
    }
  }
  return(tr)
}

