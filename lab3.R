### Lab 3
## Using data from Lab 2

## Model comparison
## Question 1
## minimum log likelihoods (from Lab 2)
# Simple (BH) model: 250.622 (BH.fit$value)
# Complex (dBH) model: 250.129 (dBH.fit$value)
D = 2*(BH.fit$value-dBH.fit$value) ## Calculate the test-statistic
pchisq(D, df = 1, lower.tail = FALSE) ##Determining the p-value from a chi                                          -squared distribution 
##0.320788

## Question 2
## Calculating AIC scores from AIC = 2*k - 2*log(likelihood)
AIC.BH = 2*(2) + 2*(BH.fit$value) #505.244
AIC.dBH = 2*(3) + 2*(dBH.fit$value) #506.259
dAIC = AIC.dBH-AIC.BH ##Difference between AIC scores
#1.0143

## Parameter uncertainty
## Question 3
## Creating a for loop to resample stock-recruit data 1000 times
deltas = NULL #Null vector to hold output
for (i in 1:1000) {
  boot = sample(c(1:nrow(dat)), replace=T) #Samples numbers 1:29 with                                                  replacement 29 times
  dat$Rstock = sample(dat$SSB[boot], nrow(dat), replace = T) #Resampling stock data by entire row, collecting it in new column in data frame
  dat$Rrec = sample(dat$R[boot], nrow(dat), replace = T) # Resampling recruit data by entire row, collecting it in new column in data frame
  p.dBH = c(alpha= 6.5, K= 3000, delta=1.0, sigma=1.0) #Parameters to start optimization (estimated in Lab2)
  dBH.fit = optim(par=p.dBH, 
                fn=negLL.dBH, 
                recruits=dat$Rrec, 
                stock=dat$Rstock,
                method="Nelder-Mead", 
                control=list(parscale=p.dBH, maxit=500000))
  ## Optimizing parameters in depensatory Beverton-Holt from a negative log likelihood distribution of recruit data
deltas[i] = dBH.fit$par[4] ## Collecting all 1000 estimates of delta in a vector
}
deltas = sort(deltas)

#Estimated CI's
deltas[26] #0.9498198
deltas[975] #1.847758
# Plotting deltas with CI's
hist(deltas)
abline(v=c(deltas[26], deltas[975]), col = "green")
CI <- quantile(deltas, c(0.025, 0.975)) ## 0.9496 - 1.8478


## Simulated power analysis
### Question 4 
## Re-set estimated parameters for original estimated parameters in Lab2 (necessary?)
p.dBH = c(alpha= 6.5, K= 3000, delta=1.0, sigma=1.0)

dBH.fit = optim(par=p.dBH, 
                fn=negLL.dBH, 
                recruits=dat$R, 
                stock=dat$SSB,
                method="Nelder-Mead", 
                control=list(parscale=p.dBH, maxit=500000))
dBH.fit$par 
# alpha            K        delta        sigma 
# 4.267122e-01 1.927653e+04 1.435774e+00 9.545050e-01

#Parameters for simulated depensatory Beverton-Holt with d.s = 2, using estimated parameters from best-fit model
a.s = BH.fit$par[1]*(BH.fit$par[2])^(1-2) 
K.s = BH.fit$par[2]^2
d.s = 2
sigma.s = BH.fit$par[3]

# Simulated depensatory model, where recruits are predicted from original stock data given depensation of 2
R.ds= dBH(a.s, K.s, dat$SSB, d.s)
# Simulating a stochastic distribution around the curve using a log-normal distribution
sim1=rlnorm(length(dat$SSB), meanlog = log(R.ds)- sigma.s^2/2, 
            sdlog= sigma.s)
plot(dat$SSB, sim1) # Plotting simulated data

### Question 5.
sims = data.frame(t(sim1)) #Starting a data frame for simulated data

# Generating 1000 sets of simulated observations, using code from above in a for loop
for(i in 1:1000){
  R.ds= dBH(a.s, K.s, dat$SSB, d.s)
  sim =rlnorm(length(dat$SSB), meanlog = log(R.ds)- sigma.s^2/2, 
              sdlog= sigma.s)
  sims[i,] = sim
  print(i) # Tracks loop's progress
}
# Plotting 1000 sets of simulated observations.These simulated data fit a depensatory Beverton-Holt with delta = 2, and a log-normal distribution of variance about the dBH curve
plot(dat$SSB,sims[1,], ylim=c(0,75000))
 for(i in 2:1000) {
  points(dat$SSB,sims[i,])
 }

## Creating empty columns to fill with the negative-log likelihoods for the Beverton-Holt and depensatory Beverton-Holt models, run with simulated recruit data 1000 times
sims$BH.nLL = 0
sims$dBH.nLL = 0
## Empty column for delta values
sims$delta = -999999 #we don't want to confuse this with an estimated delta
## Empty columns for likelihood ratio test statistic and p-value for each set of simulated observations
sims$D = 0
sims$p = 0

## For loop optimizing parameters for both the Beverton-Holt and depensatory Beverton-Holt using simulated data
for(i in 1:1000){
  p.BH = c(0.4267,1.92765e+04, 0.9545)
  BH.fit = optim(par=p.BH, 
                 fn=negLL.BH, 
                 recruits=as.numeric(sims[i,1:29]), 
                 stock=dat$SSB,
                 method="Nelder-Mead", 
                 control=list(parscale=p.BH, maxit=500000))
  P.dBH = c(0.4267,1.92765e+04, 1.43577, 0.9545)
  dBH.fit = optim(par=p.dBH, 
                  fn=negLL.dBH, 
                  recruits=as.numeric(sims[i,1:29]), 
                  stock=dat$SSB,
                  method="Nelder-Mead", 
                  control=list(parscale=p.dBH, maxit=500000))
  sims$BH.nLL[i] = BH.fit$value ## Storing neg. log-likelihoods for BH
  sims$dBH.nLL[i] = dBH.fit$value ## Storing neg. log-likelihoods for dBH 
  sims$delta[i] =dBH.fit$par[3] ## Storing deltas from dBH
  sims$D[i] = 2*(BH.fit$value-dBH.fit$value) ## Storing test-statistics
  sims$p[i] = pchisq(sims$D[i], df = 1, lower.tail = FALSE)#Storing pvalues
}

## Proportion of of simulations that result in significantly different depensatory BH models with delta >2
nrow(sims[sims$p < 0.05 & sims$delta >1, ])/nrow(sims) ##0.994

