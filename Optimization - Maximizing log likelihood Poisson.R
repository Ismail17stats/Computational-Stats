
### Numerical optimization  Log Likelihood for the poisson distribution

ll.poisson <- function(par, y, x){
  lambda <- exp(x %*% par) ##### mean of the poisson distribution 
  log.likelihood <- sum(dpois(y, lambda = lambda, log = TRUE))### pdf of the Poisson 
  return(log.likelihood)
}

#### Generate the data 
set.seed(12345)
n <- 1000
x1 <- runif(n,-1,1)
x2 <- runif(n,-1,1)
a <- 1 # intercept
b1 <- .5
b2 <- .25

linear.predictor <- a + b1*x1 + b2*x2
y <- rpois(n,lambda = exp(linear.predictor)) ### Data genering process 

## Maximize the function 
start.value <- c(0,0,0) # we are trying to optimize a , b1 , b2
poisson.model <- optim(par = start.value, y = y, x = cbind(1,x1,x2), fn = ll.poisson , method = "BFGS", hessian = TRUE, control = list(fnscale = -1)  )


ll.poisson(start.value, y, cbind(1,x1,x2))
ll.poisson(c(.2,.3,.4), y, cbind(1,x1,x2)) ## increased why ? this set of parameters has a higher relative likelihood of generating 
                                            ## the data we have on our hand compared to the start values


poisson.model
