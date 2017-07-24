## EMD Lab 1 questions
## Erin Tattersall
## July 5, 2017

### Reading in data
data = read.csv("dissections.csv", stringsAsFactors=FALSE)


## reformatting data to only include relevant columns
data = data.frame(species=as.factor(data$Species), 
                  country = as.factor(data$Country),
                  region = as.factor(data$State.Province),
                  site = as.factor(data$Site),
                  date = as.Date(data$date, format="%d-%b-%y"),
                  diam = as.numeric(data$dia..cm.),
                  height = as.numeric(data$ht.cm.),
                  mass = as.numeric(data$total..g.),
                  gonad.mass = as.numeric(data$gonad.wt..g.))

## Questions 1:
## Histogram for L. pictus with less than 3cm diameter and less than 2cm height
hist(data[data$species == "L. pictus" & data$diam < 3 & data$height < 2,] $mass, 
     xlab = "Mass", 
     main = "Masses for urchins <3cm diameter, <2cm height")

## Question 2:
##Function for plotting mass given diameter
plotMvsD = function(spp.name) {
  plot(mass ~ diam, data = data[data$species==spp.name,])
}
par(mfrow=c(1,3))
plotMvsD("L. pictus")

## Function adding line of best fit (trend line)
CubicLine = function(coeff=1, x.vals=c(0:100), ...) {
  lines(x.vals,coeff*x.vals^3, ...)
}

plotMvsD("L. pictus")
CubicLine(coeff=1, x.vals=seq(0,5,0.1), col="green")

## Question 3:
plot.urch = function(spp.name, coeff = 1, x.vals = c(0:100), ...){
  plot(mass ~ diam, 
       data = data[data$species==spp.name,], 
       col = "green", ...) ## plotting urchin mass to diameter
  points(gonad.mass ~ diam, 
         data[data$species== spp.name, ], 
         col = "purple") ## adding in gonad mass to diameter
  lines(x.vals,
        coeff*x.vals^3, 
        col = "orange") ## adding in cubic curve
}

## Using function for each species, exploring coefficients to find best fit
par(mfrow=c(1,3))
plot.urch(spp.name = "L. pictus", 
          coeff = 0.08, 
          main = "L. pictus")
plot.urch(spp.name = "S. franciscanus", 
          coeff = 0.06, 
          main = "S. franciscanus")
plot.urch(spp.name = "S. purpuratus", 
          coeff = 0.06, 
          main = "S. purpuratus")

## Question 4:

data$ratio = data$gonad.mass/data$mass ## Adding column for gonad                                             mass/ mass ratio to data
boxplot(gonad.mass/mass ~ species, data = data)

## Plotting gonad mass/mass to diameter beside mass to diameter
par(mfrow=c(1,2))
boxplot(gonad.mass/mass ~ species, 
        data = data[data$ratio < 1, ], ## Removing error from data
        main = "gonad mass/mass")
boxplot(mass ~ species, 
        data = data[data$ratio < 1, ], 
        main = "mass")

### Removing row with error
data[data$ratio > 1.5 & !is.na(data$ratio), ] ## Finding row with error
data1 <- data[-10395, ]

tapply(data1$ratio, data1$species, function(x){mean(x,na.rm=TRUE)})

## L. pictus S. franciscanus   S. purpuratus 
## 0.11969090      0.08188271      0.09492477

## Question 5
# Set up empty variables for mass record counts (*.rec) and  mass record sums (*.tot) for each species (pi = L. pictus, fr = S. franciscanus, pu = S. purpuratus)
pi.rec <- 0
pi.tot <- 0
fr.rec <- 0
fr.tot <- 0
pu.rec <- 0
pu.tot <- 0

data2 <- data1[!is.na(data1$mass),] # Remove na's from data set


for(i in 1:nrow(data2)){
  if(data2$species[i] == "L. pictus") {
    pi.tot = data2$mass[i] + pi.tot #sum of L. pictus masses
    pi.rec = pi.rec + 1 #number of L. pictus mass records
    
  } else if(data2$species[i] == "S. franciscanus") {
    fr.tot = data2$mass[i] + fr.tot 
    fr.rec = fr.rec + 1  
    
  } else if(data2$species[i] == "S. purpuratus") {
    pu.tot = data2$mass[i]+ pu.tot
    pu.rec = pu.rec + 1
  }
  mean.pictus = pi.tot/pi.rec # calculating mean
  mean.fran   = fr.tot/fr.rec
  mean.purp   = pu.tot/pu.rec
}
 ## mean.pictus = 9.8236
 ## mean.fran   = 206.2991
 ## mean.purp   = 53.0871

## Check with tapply
tapply(data2$mass, data2$species, function(x){mean(x,na.rm=TRUE)})


## Question 6

par(mfrow=c(1,3))
pl.sp = function(spp.name, coeff = 0.01, x.vals = c(0:100), ...){
  plot(ratio ~ diam, data = data1[data1$species == spp.name, ], ylab = "gonad mass/mass", ...) ## Plot of gonad mass/mass ratio to                                    diameter
  lines(x.vals,coeff*x.vals^3, col = "red") ## Adding cubic curve                                                   line
}

### Plotting for all three species
pl.sp("L. pictus", 
      coeff = 0.006, 
      x.vals=seq(0,5,0.1), 
      col = "green", 
      main = "L. pictus")
pl.sp("S. franciscanus", 
      coeff = 0.00018, 
      x.vals=seq(0,15,0.1),
      col = "blue", 
      main = "S. franciscanus")
pl.sp("S. purpuratus", 
      coeff = 0.001, 
      x.vals=seq(0,10,0.1),
      col = "purple", 
      main = "S. purpuratus")