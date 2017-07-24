### Lab 2 Questions


## Beverton-Holt recruitment model R = (alpha*S)/(1+(S/K))
## Define function
BH = function(alpha, K, S) {
  a = alpha*S
  b = 1 + (S/K)
  R = a/b
  return(R)
}

### Question 1
S = seq(0, 4000, 0.1) ## Vector for range of Stock values
R = BH(2,3000, S) ## Recruitment, given Stock values

## Plot for predicted recruitment given Stock values
plot(S, R, xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (millions of fish)", type = "l", col = "red")
points(R ~ SSB, dat, col = "blue")

## Question 2
dBH = function(alpha, K, S, delta) {
  c = S^delta ## adding depensation to the Stock vector
  a = alpha*c
  b = 1 + (c/K)
  R = a/b
  return(R)
}


S = seq(0, 4000, 0.1) ## Vector for range of Stock values
R1 = dBH(2, 3000, S, 2)
plot(S, R1, xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (millions of fish)", type = "l", col = "red")
points(R ~ SSB, dat, col = "blue")

## Question 3.
##negLL.BH
params = c(alpha, K, sigma) ##Input parameters must be a vector of 3
negLL.BH = function(params, recruits, stock) {
  alpha = params[1]
  K = params[2]
  sigma = params[3]
  predictions =BH(alpha, K, stock)
  LL = dlnorm(recruits, meanlog = log(predictions)- sigma^2/2, 
              sdlog= sigma, log = TRUE)
  return(-sum(LL)) 
}

##negLL.dBH
params = c(alpha, K, delta, sigma) ##Input parameters must include                                         delta now!
negLL.dBH = function(params, recruits, stock){
  alpha = params[1]
  K = params[2]
  delta = params[3]
  sigma = params[4]
  predictions = dBH(alpha, K, stock, delta)
  LL = dlnorm(recruits, meanlog = log(predictions)- sigma^2/2, 
              sdlog= sigma, log = TRUE)
  return(-sum(LL))
}

### Question 4
#### Optimisation
## non-depensatory
p.BH = c(alpha= 3.5, K= 10000, sigma=3.8)

BH.fit = optim(par=p.BH, 
               fn=negLL.BH, 
               recruits=dat$R, 
               stock=dat$SSB,
               method="Nelder-Mead", 
               control=list(parscale=p.BH, maxit=500000))
BH.fit ### $value gives the minimum log likelihood, par = estimated parameters to start with
### alpha = 4.208 K = 5112.891 sigma = 0.971
### $value = 250.6222

## depensatory
p.dBH = c(alpha= 6.5, K= 3000, delta=1.0, sigma=1.0)

dBH.fit = optim(par=p.dBH, 
                fn=negLL.dBH, 
                recruits=dat$R, 
                stock=dat$SSB,
                method="Nelder-Mead", 
                control=list(parscale=p.dBH, maxit=500000))
dBH.fit
### alpha = 0.4267 K = 1.927e+04 delta=1.44 sigma= 0.95405
## $value = 250.1293

## Plotting models
R.BH = BH(4.208, 5112.891, S) ### Optimised parameters for minimum log-likelihood for non-depensatory
R.dBH = dBH(0.4267, 1.927e+04, S, 1.44)
plot(R ~ SSB, data = dat, xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (millions of fish)")
lines(S, R.BH, col = "blue")
lines(S, R.dBH, col = "green")