### EMD: Lab 1 working script
## July 5, 2017
setwd("C:/Users/ETattersall/Documents/BMSC_EMD/Labs")

data = read.csv("dissections.csv", stringsAsFactors=FALSE)
summary(data)
head(data)

hist(data[, "ht.cm."]) # histogram of heightin cm

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
 # Warning message:
  # In data.frame(species = as.factor(data$Species), country = as.factor(data$Country),  :
                  # NAs introduced by coercion
as.numeric(as.factor(c(45,48,307)))

summary(data)
hist(data[data$country == "Canada",] $height) ## histogram of heights only in Canada
nrow(data[data$country=="Canada" & data$species=="S. purpuratus",]) ## subsetting for Canadian                                                                        occurrences of S. purpuratus
hist(data[data$species == "L. pictus" & data$diam < 3 & data$height < 2,] $mass)

plot(data$mass ~ data$diam)

diameters = seq(0,20,0.25)
spheres = pi*4/3*(diameters/2)^3
lines(diameters, spheres, col="red")

hemispheres = (pi*4/3*(diameters/2)^3)/2
lines(diameters, hemispheres, col="red")

par(mfrow=c(1,3))
plot(data[data$species=="L. pictus",]$diam,
     data[data$species=="L. pictus",]$mass)

plotMvsD = function(spp.name) {
  plot(mass ~ diam, data = data[data$species==spp.name,])
}
par(mfrow=c(1,3))
plotMvsD("L. pictus")


CubicLine = function(coeff=1, x.vals=c(0:100), ...) {
  lines(x.vals,coeff*x.vals^3, ...)
}

plotMvsD("L. pictus")
CubicLine(coeff=1, x.vals=seq(0,5,0.1), col="green")

plot.urch = function(spp.name, coeff = 1, x.vals = c(0:100), ...){
  plot(mass ~ diam, data = data[data$species==spp.name,], col = "green", ...)
  points(gonad.mass ~ diam, data[data$species== spp.name, ], col = "purple")
  lines(x.vals,coeff*x.vals^3, col = "orange")
}
par(mfrow=c(1,3)) ## can turn off with dev.off() or par(mfrow=c(1,1))
plot.urch(spp.name = "L. pictus", coeff = 0.08, main = "L. pictus")
plot.urch(spp.name = "S. franciscanus", coeff = 0.06, main = "S. franciscanus")
plot.urch(spp.name = "S. purpuratus", coeff = 0.06, main = "S. purpuratus")

tapply(data$mass, data$species, function(x){median(x,na.rm=TRUE)})

boxplot(gonad.mass/mass ~ species, data = data)
data$ratio = data$gonad.mass/data$mass

par(mfrow=c(1,2))
boxplot(gonad.mass/mass ~ species, data = data[data$ratio < 1, ], main = "gonad mass/mass")
boxplot(mass ~ species, data = data[data$ratio < 1, ], main = "mass")
summary(data$ratio)


data[data$ratio > 1.5 & !is.na(data$ratio), ]
data1 <- data[-10395, ]
tapply(data1$ratio, data1$species, function(x){mean(x,na.rm=TRUE)})

par(mfrow=c(1,3))
for(spp.name in unique(data$species)) {
   plot(gonad.mass ~ as.numeric(format(date,"%j")),
   data=data[data$species==spp.name,])
   title(main=spp.name)
}

for(row in c(1,35,10311,3,54,9126)){
  if(data$species[row]=="S. franciscanus") {
    print("potentially edible")
    } else if(data$species[row]=="S. purpuratus") {
    print("a bit small")
    } else print("way too small")
}




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
data$ratio = data$gonad.mass/data$mass
data1 <- data[data$ratio < 1,]


table(is.na(data1$mass))
data1$rem <- is.na(data1$mass)
table(data1$rem)
data2 <- data[data$rem == "FALSE", ]

pi.rec <- 0
pi.tot <- 0
fr.rec <- 0
fr.tot <- 0
pu.rec <- 0
pu.tot <- 0

data2 <- data1[!is.na(data1$mass),]


for(i in 1:nrow(data2)){
   if(data2$species[i] == "L. pictus") {
    pi.tot = data2$mass[i] + pi.tot
    pi.rec = pi.rec + 1
    
  } else if(data2$species[i] == "S. franciscanus") {
    fr.tot = data2$mass[i] + fr.tot 
    fr.rec = fr.rec + 1  
    
  } else if(data2$species[i] == "S. purpuratus") {
    pu.tot = data2$mass[i]+ pu.tot
         pu.rec = pu.rec + 1
  }
  mean.pictus = pi.tot/pi.rec
  mean.fran   = fr.tot/fr.rec
  mean.purp   = pu.tot/pu.rec
}

tapply(data1$mass, data1$species, function(x){mean(x,na.rm=TRUE)})


table(is.na(data$mass))


### Question 6
plot(ratio ~ diam, data = data)
plot.urch = function(spp.name, coeff = 1, x.vals = c(0:100), ...){
  plot(ratio ~ diam, data = data[data$species==spp.name,], col = "green", ...)
  lines(x.vals,coeff*x.vals^3, col = "orange")
}

par(mfrow=c(1,3))
pl.sp = function(spp.name, coeff = 0.01, x.vals = c(0:100),...){
  plot(ratio ~ diam, data = data1[data1$species == spp.name, ], ...)
  lines(x.vals,coeff*x.vals^3, col = "red")
  }
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



