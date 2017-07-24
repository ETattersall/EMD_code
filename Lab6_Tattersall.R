## EMD lab 6
## Erin Tattersall, started July 13, 2017
setwd("C:/Users/ETattersall/Documents/BMSC_EMD/Labs/Lab6")
set.seed(3587624) ##Set seed!!
#### Simulation_function (code provided) ####
#function to simulate the number of prey eaten by a foraging predator
#size = number of grid cells per side of the square foraging 'arena'
#n = number of prey individuals at the start of the simulation
#time = number of timesteps over which to run the simulation
#handling.time = number of timesteps a predator must wait to restart foraging, 
#after capturing a prey
#draw.plot = whether or not to plot the arena as the simulation proceeds 
#(NOTE: plotting will SLOW DOWN the simulation and open a new plotting window)
sim.predation = function(size =30, n=100, time=100, handling.time=5, draw.plot=F) {
  
  #set up a data frame to represent the prey 
  prey = data.frame(x.pos=floor(runif(n,1,1+size)),
                    y.pos=floor(runif(n,1,1+size)),
                    born = 0, #when were the prey individuals created
                    died = NA)
  
  #another for the predators
  pred = data.frame(x.pos=floor(runif(1,1,1+size)), 
                    y.pos=floor(runif(1,1,1+size)), 
                    state=0) #hunting=0, handling>0
  
  if(draw.plot) {
    x11() #open a new plot window	
  }
  
  #loop through simulation for set number of timesteps
  for(t in 1:time){
    live.prey=which(is.na(prey$died)) #which prey are currently alive
    dead.prey=which(is.na(prey$died)==F) #which prey are dead
    
    #if there are living prey, allow them to move randomly    
    #x.pos can change 1-, 0, or +1... same with y.pos
    prey$x.pos[live.prey] = prey$x.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    prey$y.pos[live.prey] = prey$y.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    # Moving prey at the edges of the board back to the other side so prey up against the wall don't get immediately eaten!!
    prey$x.pos[prey$x.pos<1] = size; prey$x.pos[prey$x.pos>size] = 1
    prey$y.pos[prey$y.pos<1] = size; prey$y.pos[prey$y.pos>size] = 1
    
    #keep track of predators' remaining handling time
    if(sum(pred$state)>0) {
      pred$state[pred$state>0] = pred$state[pred$state>0] - 1 #Reduce 'state' if handling time hasn't run out
    }
    
    #if the predator is hunting, simulate its actions
    if(any(pred$state==0)) {
      #movement (same system as for prey)
      pred[pred$state==0,]$x.pos = pred[pred$state==0,]$x.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred[pred$state==0,]$y.pos = pred[pred$state==0,]$y.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred$x.pos[pred$x.pos<1] = size; pred$x.pos[pred$x.pos>size] = 1
      pred$y.pos[pred$y.pos<1] = size; pred$y.pos[pred$y.pos>size] = 1
      
      hunting.preds = which(pred$state==0) #which predators are hunting (we already know at least one is)
      #cycle through the hunting predators... see if they caught anything
      for(hunting.pred in hunting.preds) {
        #when a predator is at the same position as a live prey item
        prey.sighted = live.prey[prey$x.pos[live.prey]==pred[hunting.pred,]$x.pos &
                                   prey$y.pos[live.prey]==pred[hunting.pred,]$y.pos]
        
        if(length(prey.sighted)>0) { #if a predation event occured
          pred[hunting.pred,]$state = handling.time
          
          #decide which of the sighted prey get eaten, since multiple prey can be in the same position
          eaten.prey = prey.sighted[sample(length(prey.sighted),1)] 
          prey$died[eaten.prey] = t
          
          #add a new prey item to keep n constant
          prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
        }#end predation event
      }#end for(hunting.pred)
    }
    
    #if desired, plot the arena
    if(draw.plot) {	
      #live prey are blue
      plot(prey$x.pos[live.prey],prey$y.pos[live.prey], xlim=c(1,size), ylim=c(1,size), col="blue", cex=1.2,
           xlab="x position",ylab="y position", main="PREDATION ARENA of DEATH")
      par(new=TRUE)
      #hunting predators are blue
      plot(pred$x.pos[pred$state==0],pred$y.pos[pred$state==0], 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,lwd=3,
           col="blue",axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #handling predators are grey
      plot(pred[pred$state>0,]$x.pos,pred[pred$state>0,]$y.pos, 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,col="grey",lwd=3,axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #dead prey are red
      plot(prey$x.pos[dead.prey],prey$y.pos[dead.prey], 
           xlim=c(1,size), ylim=c(1,size),pch=16,col="red", axes=FALSE,
           xlab=NA, ylab=NA)
      Sys.sleep(1) #delay iteration (so the plot sticks around long enough to see it)
    }	
    
  }
  
  return(prey)
  
}#end sim.predation

#### Question 1. Simulate a predation data set ####
### Simulating number of prey eaten for range of prey items
n = 1000 # Selected prey abundance
prey.eaten = rep(0,n) #Empty vector for prey eaten

for (i in 1:n){
  prey = sim.predation(size = 30, 
                          n = i, 
                          time = 100, 
                          handling.time = 5, 
                          draw.plot = F)
  prey.eaten[i] = sum(!is.na(prey$died))
}
plot(prey.eaten,
     xlab = "Prey Abundance",
     ylab = "Prey Eaten")

#### Question 2. Fitting a functional response + CI for estimated curve ####
## Nonlinear least squares function: determine the weighted least-squares estimates of parameters, nonlinear model
## Not to be used on 'zero-residual' data


## Matrices for parameter estimates
estimates = matrix(NA, nrow=1000, ncol=2)
predictions = matrix(NA, nrow=1000, ncol=1000)
N.predict = seq(1,1000,1)  ##New prey abundance data
consumed = prey.eaten ##Vector to take prey eaten data from

# Function for Type 2 functional response
T2fr = function(Tt,Th, alpha, N) {#Accepts handling time, encounter rate and N prey
  a = alpha*Tt*N #numerator
  b = 1 + (Th*alpha*N) #denominator
  eaten = a/b 
# return(eaten) #Returns predicted prey eaten
}



## Loop fitting a type 2 functional response to simulated data
for (i in 1:1000) {
  boot = sample(c(1:n), 
                replace = T) #Sample with replacement 1000 times
  boot.nls <- nls(consumed[boot] ~ (100*alpha*boot)/(1+(Th*alpha*boot)),
                start =list(alpha=0.001,Th=5), ##specifying initial params
                algorithm="port", ## 'nl2sol' algorithm
                lower=c(0,0), ## vectors of upper and lower CIs with length of 'start'
                upper=c(10,100))
  coeff = coef(boot.nls) ## Pulling alpha and Th estimates from nls output
  estimates[i,] = coeff ##Storing coefficients in matrix
  predictions[i,] = T2fr(100, Th = coeff[2], alpha = coeff[1], N = N.predict) ##Storing predicted values
}

# Histograms of parameter estimates
##Sorting estimates to find confidence intervals
Sestimates = apply(estimates,2,sort) ##Sorting estimates into increasing order
hist(Sestimates[,1],xlab = "Alpha", main = "") ## Plotting alphas in a histogram

abline(v=c(Sestimates[26,1], Sestimates[975,1]), col = "green") #Confidence intervals
CI.alpha = c(Sestimates[26,1], Sestimates[975,1]) ##0.000798, 0.000877
opt.alpha = median(Sestimates[,1])#Optimum alpha value = 0.000837
hist(estimates[,2], xlab = "Handling time", main = "") ## Plotting alphas in a histogram
abline(v=c(Sestimates[26,2], Sestimates[975,2]), col = "green") #Confidence intervals
opt.Th = median(Sestimates[,2])#Optimum Th value = 4.255
CI.Th = c(Sestimates[26,2], Sestimates[975,2])#4.17, 4.34


## Estimating confidence intervals for each prediction
## Each row is already sorted from lowest to highest
#Need to sort iterations (rows)
SN.predict = apply(predictions,2,sort)
## Confidence intervals
LCI.predict = SN.predict[26,]
UCI.predict = SN.predict[975,]
best.fit =T2fr(100, Th = 4.255, alpha = 0.000837, N = N.predict)

#Plotting functional response data, best.fit curve, and CIs
plot(prey.eaten,
     xlab = "Prey Abundance",
     ylab = "Prey Eaten",
     col = "coral1") ## Functional response data
lines(N.predict, best.fit, type = "l", col = "darkmagenta") ## best.fit curve
lines(N.predict, LCI.predict, type = "l", col = "red3") #Lower confidence interval
lines(N.predict, UCI.predict, type = "l", col = "red3") #Upper confidence interval

#### Part II: Preparing data ####

## Keeping prey individuals from being replaced (Modified sim. code) ##
sim.predation1 = function(size =30, n=100, time=100, handling.time=5, draw.plot=F) {
  
  #set up a data frame to represent the prey 
  prey = data.frame(x.pos=floor(runif(n,1,1+size)),
                    y.pos=floor(runif(n,1,1+size)),
                    born = 0, #when were the prey individuals created
                    died = NA) #When prey died
  
  #another for the predators
  pred = data.frame(x.pos=floor(runif(1,1,1+size)), 
                    y.pos=floor(runif(1,1,1+size)), 
                    state=0) #hunting=0, handling>0
  
  if(draw.plot) {
    x11() #open a new plot window	
  }
  
  #loop through simulation for set number of timesteps
  for(t in 1:time){
    live.prey=which(is.na(prey$died)) #which prey are currently alive
    dead.prey=which(is.na(prey$died)==F) #which prey are dead
    
    #if there are living prey, allow them to move randomly    
    #x.pos can change 1-, 0, or +1... same with y.pos
    prey$x.pos[live.prey] = prey$x.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    prey$y.pos[live.prey] = prey$y.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    # Moving prey at the edges of the board back to the other side so prey up against the wall don't get immediately eaten!!
    prey$x.pos[prey$x.pos<1] = size; prey$x.pos[prey$x.pos>size] = 1
    prey$y.pos[prey$y.pos<1] = size; prey$y.pos[prey$y.pos>size] = 1
    
    #keep track of predators' remaining handling time
    if(sum(pred$state)>0) {
      pred$state[pred$state>0] = pred$state[pred$state>0] - 1 #Reduce 'state' if handling time hasn't run out
    }
    
    #if the predator is hunting, simulate its actions
    if(any(pred$state==0)) {
      #movement (same system as for prey)
      pred[pred$state==0,]$x.pos = pred[pred$state==0,]$x.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred[pred$state==0,]$y.pos = pred[pred$state==0,]$y.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred$x.pos[pred$x.pos<1] = size; pred$x.pos[pred$x.pos>size] = 1
      pred$y.pos[pred$y.pos<1] = size; pred$y.pos[pred$y.pos>size] = 1
      
      hunting.preds = which(pred$state==0) #which predators are hunting (we already know at least one is)
      #cycle through the hunting predators... see if they caught anything
      for(hunting.pred in hunting.preds) {
        #when a predator is at the same position as a live prey item
        prey.sighted = live.prey[prey$x.pos[live.prey]==pred[hunting.pred,]$x.pos &
                                   prey$y.pos[live.prey]==pred[hunting.pred,]$y.pos]
        
        if(length(prey.sighted)>0) { #if a predation event occured
          pred[hunting.pred,]$state = handling.time
          
          #decide which of the sighted prey get eaten, since multiple prey can be in the same position
          eaten.prey = prey.sighted[sample(length(prey.sighted),1)] 
          prey$died[eaten.prey] = t
          
          #add a new prey item to keep n constant
          #prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
        }#end predation event
      }#end for(hunting.pred)
    }
    
    #if desired, plot the arena
    if(draw.plot) {	
      #live prey are blue
      plot(prey$x.pos[live.prey],prey$y.pos[live.prey], xlim=c(1,size), ylim=c(1,size), col="blue", cex=1.2,
           xlab="x position",ylab="y position", main="PREDATION ARENA of DEATH")
      par(new=TRUE)
      #hunting predators are blue
      plot(pred$x.pos[pred$state==0],pred$y.pos[pred$state==0], 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,lwd=3,
           col="blue",axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #handling predators are grey
      plot(pred[pred$state>0,]$x.pos,pred[pred$state>0,]$y.pos, 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,col="grey",lwd=3,axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #dead prey are red
      plot(prey$x.pos[dead.prey],prey$y.pos[dead.prey], 
           xlim=c(1,size), ylim=c(1,size),pch=16,col="red", axes=FALSE,
           xlab=NA, ylab=NA)
      Sys.sleep(1) #delay iteration (so the plot sticks around long enough to see it)
    }	
    
  }
  
  return(prey)
  
}
#end sim.predation

## Simulating predation without replacement for abundance = 800
prey800 = sim.predation1(size = 30, 
                       n = 800, 
                       time = 4000, 
                       handling.time = 5, 
                       draw.plot = F)
##Prey data frame gives you last position for each individual and when they died (NA if they survived)
prey800$group = rep(800,800) ## Adding column of prey abundance




## Simulating predation without replacement for abundance = 200
prey200 = sim.predation1(size = 30, 
                        n = 200, 
                        time = 4000, 
                        handling.time = 5, 
                        draw.plot = F)
prey200$group = rep(200,200) ## Adding column of prey abundance

#Take a random subset of length = lower abundance
prey800.samp = prey800[sample(nrow(prey800), size = 200, replace = F), ] 


mort = rbind(prey200,prey800.samp) #Combining 2 samples into 1 data frame

mort$status = numeric(400)# Creating status column
mort[,6][is.na(mort[,4])] = 0 ## If individual died, status = 0
mort[,6][!is.na(mort[,4])] = 1 ##If individual was censored = 1
mort[,4][is.na(mort[,4])] = 4000 ## If individual was censored, died = 4000
mort$group = as.factor(mort$group) ## Setting group to as factor

#### Question 3: Survival Analysis ####
library(survival)

## creating surv object
Sdata = Surv(mort$died,mort$status)
plot(survfit(Sdata~1), xlab = "Time", 
     ylab = "Survivorship")
plot(survfit(Sdata~1), log=TRUE) #Log survival curve
plot(survfit(Sdata~mort$group), 
     xlab = "Time", 
     ylab = "Survivorship", 
     col = c("mediumseagreen", "mediumpurple")) #Plotting survival by group
legend(2500, 0.8, legend=c("prey200", "prey800"),
       col=c("mediumseagreen", "mediumpurple"), lwd = 1.3, cex=0.8)
model = coxph(Sdata~mort$group) ##Fitting Cox proportional hazard
summary(model)
#coef = -0.6231 exp(coef) = 0.5363 ~~ prey200 individuals have 1.86 times more hazard

base.model = coxph(Sdata~1)
summary(base.model)


#### Question 4: 3a) Adding predators and conducting two-factor survival analysis ####
sim.predation3 = function(size =30, n=100, n.pred=5, time=100, handling.time=5, draw.plot=F) {
  
  #set up a data frame to represent the prey 
  prey = data.frame(x.pos=floor(runif(n,1,1+size)),
                    y.pos=floor(runif(n,1,1+size)),
                    born = 0, #when were the prey individuals created
                    died = NA)
  
  #another for the predators
  pred = data.frame(x.pos=floor(runif(n.pred,1,1+size)), ##Added argument for number of predators
                    y.pos=floor(runif(n.pred,1,1+size)), 
                    state=0) #hunting=0, handling>0
  
  if(draw.plot) {
    x11() #open a new plot window	
  }
  
  #loop through simulation for set number of timesteps
  for(t in 1:time){
    live.prey=which(is.na(prey$died)) #which prey are currently alive
    dead.prey=which(is.na(prey$died)==F) #which prey are dead
    
    #if there are living prey, allow them to move randomly    
    #x.pos can change 1-, 0, or +1... same with y.pos
    prey$x.pos[live.prey] = prey$x.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    prey$y.pos[live.prey] = prey$y.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    # Moving prey at the edges of the board back to the other side so prey up against the wall don't get immediately eaten!!
    prey$x.pos[prey$x.pos<1] = size; prey$x.pos[prey$x.pos>size] = 1
    prey$y.pos[prey$y.pos<1] = size; prey$y.pos[prey$y.pos>size] = 1
    
    #keep track of predators' remaining handling time
    if(sum(pred$state)>0) {
      pred$state[pred$state>0] = pred$state[pred$state>0] - 1 #Reduce 'state' if handling time hasn't run out
    }
    
    #if the predator is hunting, simulate its actions
    if(any(pred$state==0)) {
      #movement (same system as for prey)
      pred[pred$state==0,]$x.pos = pred[pred$state==0,]$x.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred[pred$state==0,]$y.pos = pred[pred$state==0,]$y.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred$x.pos[pred$x.pos<1] = size; pred$x.pos[pred$x.pos>size] = 1
      pred$y.pos[pred$y.pos<1] = size; pred$y.pos[pred$y.pos>size] = 1
      
      hunting.preds = which(pred$state==0) #which predators are hunting (we already know at least one is)
      #cycle through the hunting predators... see if they caught anything
      for(hunting.pred in hunting.preds) {
        #when a predator is at the same position as a live prey item
        prey.sighted = live.prey[prey$x.pos[live.prey]==pred[hunting.pred,]$x.pos &
                                   prey$y.pos[live.prey]==pred[hunting.pred,]$y.pos]
        
        if(length(prey.sighted)>0) { #if a predation event occured
          pred[hunting.pred,]$state = handling.time
          
          #decide which of the sighted prey get eaten, since multiple prey can be in the same position
          eaten.prey = prey.sighted[sample(length(prey.sighted),1)] 
          prey$died[eaten.prey] = t
          
          #add a new prey item to keep n constant
          #prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
        }#end predation event
      }#end for(hunting.pred)
    }
    
    #if desired, plot the arena
    if(draw.plot) {	
      #live prey are blue
      plot(prey$x.pos[live.prey],prey$y.pos[live.prey], xlim=c(1,size), ylim=c(1,size), col="blue", cex=1.2,
           xlab="x position",ylab="y position", main="PREDATION ARENA of DEATH")
      par(new=TRUE)
      #hunting predators are blue
      plot(pred$x.pos[pred$state==0],pred$y.pos[pred$state==0], 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,lwd=3,
           col="blue",axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #handling predators are grey
      plot(pred[pred$state>0,]$x.pos,pred[pred$state>0,]$y.pos, 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,col="grey",lwd=3,axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #dead prey are red
      plot(prey$x.pos[dead.prey],prey$y.pos[dead.prey], 
           xlim=c(1,size), ylim=c(1,size),pch=16,col="red", axes=FALSE,
           xlab=NA, ylab=NA)
      Sys.sleep(1) #delay iteration (so the plot sticks around long enough to see it)
    }	
    
  }
  
  return(prey)
  
}#end sim.predation

##Prey = 200, pred = 1
p200pr1 = sim.predation3(size = 30,
                         n =200,
                         n.pred = 1,
                         time = 1000,
                         handling.time = 5,
                         draw.plot = F)
p200pr1$group = rep(200,200) ## Adding column of prey abundance
p200pr1$Pred =rep(1, 200) ##Adding column of pred. abundance

##Prey = 200, pred = 2
p200pr2 = sim.predation3(size = 30,
                         n =200,
                         n.pred = 2,
                         time = 1000,
                         handling.time = 5,
                         draw.plot = F)
p200pr2$group = rep(200,200) ## Adding column of prey abundance
p200pr2$Pred =rep(2, 200) ##Adding column of pred. abundance

##Prey = 800, pred = 2
p800pr2 = sim.predation3(size = 30,
                          n =800,
                          n.pred = 2,
                          time = 1000,
                          handling.time = 5,
                          draw.plot = F)
p800pr2$group = rep(800,800) ## Adding column of prey abundance
p800pr2$Pred =rep(2, 800) ##Adding column of pred. abundance

##Prey = 800, pred = 1
p800pr1 = sim.predation3(size = 30,
                          n =800,
                          n.pred = 1,
                          time = 1000,
                          handling.time = 5,
                          draw.plot = F)
p800pr1$group = rep(800,800) ## Adding column of prey abundance
p800pr1$Pred =rep(1, 800) ##Adding column of pred. abundance

#Take a random subset of length = lower abundance, for both higher prey densities
S.p800pr2 = p800pr2[sample(nrow(p800pr2), size = 200, replace = F), ] 
S.p800pr1 = p800pr1[sample(nrow(p800pr1), size = 200, replace = F), ]

## Combining into one data frame
mort2 = rbind(p200pr2,p200pr1,S.p800pr2,S.p800pr1)
mort2$status <- ifelse(is.na(mort2$died), 0, 1) # Creating status column
mort2$died <- ifelse(is.na(mort2$died), 1000, mort2$died) ## If individual was censored, died = 1000
mort2$group = as.factor(mort2$group) ## Setting group to as factor
mort2$Pred = as.factor(mort2$Pred) ## Setting group to as factor

### Survival analysis
Sdata = Surv(mort2$died,mort2$status) ##Survival object
plot(survfit(Sdata~1), xlab = "Time", 
     ylab = "Survivorship") ## Plotting survivorship of all prey
plot(survfit(Sdata~1), log=TRUE) #Log survival curve

#Modelling survival between 2 predator groups
#Plotting survival by Pred only
plot(survfit(Sdata~mort2$Pred), 
     xlab = "Time", 
     ylab = "Survivorship",
     col = c("maroon1", "orange3")) #Plotting survival by group
legend(10, 0.4, legend=c("pred1", "pred2"),
       col=c("maroon1", "orange3"), lwd = 1.3, cex=0.8)


#Plotting survival by group only
plot(survfit(Sdata~mort2$group), 
     xlab = "Time", 
     ylab = "Survivorship",
     col = c("mediumseagreen", "mediumpurple")) #Plotting survival by group
legend(10, 0.4, legend=c("prey200", "prey800"),
       col=c("mediumseagreen", "mediumpurple"), lwd = 1.3, cex=0.8)


#Plotting survival by group and Pred
plot(survfit(Sdata~mort2$group + mort2$Pred), 
     xlab = "Time", 
     ylab = "Survivorship")
##Figuring out which line is which (Run each with plot separately to avoid crazy overlap)
#Prey800, pred1
lines(survfit(Sdata[601:800,]~mort2$group[601:800] + mort2$Pred[601:800]),
      col = "green", type = "l")
#Prey800, pred2
lines(survfit(Sdata[401:600,]~mort2$group[401:600] + mort2$Pred[401:600]),
      col = "deeppink1", type = "l")
#Prey200, pred1
lines(survfit(Sdata[201:400,]~mort2$group[201:400] + mort2$Pred[201:400]),
      col = "green", type = "l")
#Prey200, pred1
lines(survfit(Sdata[1:200,]~mort2$group[1:200] + mort2$Pred[1:200]),
      col = "deeppink1", type = "l")


## Plot for Figure
plot(survfit(Sdata~mort2$group + mort2$Pred), 
     xlab = "Time", 
     ylab = "Survivorship",
     col = c("mediumseagreen", "mediumpurple", "maroon1", "orange3"))
legend(10, 0.6, legend=c("Prey800Pred1", "Prey200Pred1", "Prey800Pred2", "Prey200Pred2"),
       col=c("maroon1", "mediumseagreen", "orange3", "mediumpurple"), lwd = 1.3, cex=0.8)


### Running Cox proportion hazard analyses
model.PP = coxph(Sdata~mort2$group + mort2$Pred)
summary(model.PP)

