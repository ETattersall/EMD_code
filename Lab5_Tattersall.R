## Lab5: Hierarchical Models
## Erin Tattersall, started July 11, 2017

#### Pre-lab prep ####
install.packages("lme4")
library(lme4)

setwd("C:/Users/ETattersall/Documents/BMSC_EMD/Labs/Lab5")
library(repmis) ## package required to load data

### Loading data
FishData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

## Subsetting data for years after 2010 and all species except sockeye
FishData <- subset(FishData.all,
                   FishData.all$species != "sockeye")
## Loading corresponding data (temperature, salinity)
SiteData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_siteData.csv")
## Adding temperature and salinity data to FishData data frame
FishData$temp <- SiteData.all$temp[match(FishData$site_id, SiteData.all$site_id)]
FishData$sal <- SiteData.all$salt[match(FishData$site_id, SiteData.all$site_id)]

## Creating variable for parasite total
## Calling names for all parasites at all stages
names(FishData)[c(11:25)] 
## Adding across columns to get total parasite load
FishData$p.total<-apply(FishData[,c(11:25)], 1, sum, na.rm=TRUE)
## Column of binary data (infested or not)
FishData$infested<-as.numeric(FishData$p.total>0)

#### Question 1.####
##Fitting a glm for parasite load as a function of year
fit1 <- glm(p.total ~ as.factor(year), data=FishData, family=poisson(link = "log"))
fit1.coeff = summary(fit1)$coefficients

## List of empty vectors to feed into loop
p.log = numeric(16) ## log(mean lice) per year
p.avg = numeric(16) ## mean lice per year
UCI = numeric(16) ## Upper confidence (mean + 1.96*SE)
LCI = numeric(16) ## Lower confidence (mean - 1.96*SE)

### Filling vectors for year 2001
p.log[1] = fit1.coeff[1]
p.avg[1] = exp(fit1.coeff[1])
UCI[1] = exp(fit1.coeff[1] + 1.96*fit1.coeff[1,2])
LCI[1] = exp(fit1.coeff[1] - 1.96*fit1.coeff[1,2])

## Vector of years 2002-2016
years = row.names(fit1.coeff[2:16,])

## For loop adding elements to above vectors for years 2002-2016
for (i in 2:16){
  p.log[i] = p.log[1] + fit1.coeff[years[i-1],1] ## adding estimate per year to vector
  p.avg[i] = exp(p.log[i]) ## adding mean per year to vector
  Var.U <- summary(fit1)$coefficients['(Intercept)', 2]^2 ## SE of the intercept
  Var.V <- summary(fit1)$coefficients[years[i-1], 2]^2 ## SE  of estimate per year
  Cov.UV <- vcov(fit1)[i,i] ##Covariance between U and V
  UCI[i] = exp(p.log[i] + 1.96*(sqrt(Var.U + Var.V+ 2*Cov.UV))) ##Adding upper confidence limit
  LCI[i] = exp(p.log[i] - 1.96*(sqrt(Var.U + Var.V + 2*Cov.UV))) ## Adding lower confidence limit
}
## Saving and writing to data.frame --> csv
CI.years = as.data.frame(cbind(p.log,p.avg, LCI, UCI), row.names = row.names(fit1.coeff))


#### Incorporating date into data ####
FishData$date <- as.Date(paste(FishData$year, FishData$month, FishData$day, sep="-"), format="%Y-%m-%d") ##Creating a date column
FishData$week<-strftime(FishData$date, format="%W") ## Column for week

#### Question 2. ####
##2a Random effects model, with a random effect for week
fit2 = glmer(p.total ~ as.factor(year) + (1|week), data=FishData, family=poisson(link = "log"))
summary(fit2)
fit2.coeff = summary(fit2)$coefficients ## coefficients for random effects model

## Best Linear Unbiased Predictors
BLUPs = ranef(fit2)

## 2c)
## Estimates for louse abundance in the random effects model
## List of empty vectors to feed into loop
p.log = numeric(16) ## log(mean lice) per year
p.avg = numeric(16) ## mean lice per year
UCI = numeric(16) ## Upper confidence (mean + 1.96*SE)
LCI = numeric(16) ## Lower confidence (mean - 1.96*SE)

### Filling vectors for year 2001
p.log[1] = fit2.coeff[1]
p.avg[1] = exp(fit2.coeff[1])
UCI[1] = exp(fit2.coeff[1] + 1.96*fit2.coeff[1,2])
LCI[1] = exp(fit2.coeff[1] - 1.96*fit2.coeff[1,2])

## Vector of years 2002-2016
years = row.names(fit2.coeff[2:16,])

## For loop adding elements to above vectors for years 2002-2016
for (i in 2:16){
  p.log[i] = p.log[1] + fit2.coeff[years[i-1],1] ## adding estimate per year to vector
  p.avg[i] = exp(p.log[i]) ## adding mean per year to vector
  Var.U <- summary(fit2)$coefficients['(Intercept)', 2]^2 ## SE of the intercept
  Var.V <- summary(fit2)$coefficients[years[i-1], 2]^2 ## SE  of estimate per year
  Cov.UV <- vcov(fit2)[i,i] ##Covariance between U and V
  UCI[i] = exp(p.log[i] + 1.96*(sqrt(Var.U + Var.V+ 2*Cov.UV))) ##Adding upper confidence limit
  LCI[i] = exp(p.log[i] - 1.96*(sqrt(Var.U + Var.V + 2*Cov.UV))) ## Adding lower confidence limit
}
## Saving and writing to data.frame
CI.years.fit2 = as.data.frame(cbind(p.log,p.avg, LCI, UCI), row.names = row.names(fit2.coeff))
CI.years = cbind(CI.years,CI.years.fit2) ## COmbining estimates for both models
CI.years$meandiff = (CI.years[,2] - CI.years[,6])
CI.years$GLM.range = CI.years[,4] - CI.years[,3]
CI.years$GLMM.range = CI.years[,8] - CI.years[,7]
CI.years$range.diff = (CI.years[,4] - CI.years[,3]) - (CI.years[,8] - CI.years[,7])
write.csv(CI.years, "CI.years.csv")

#### Question 3. Sea lice between chum and pink salmon ####
unique(FishData$species) ## Blanks counted as a species
FishData$species[FishData$species==""] = NA ## Blanks converted to NA's

fit3 = glmer(p.total ~ as.factor(year) + species + (1|week), data=FishData, family=poisson(link = "log"))
summary(fit3)


### Log ratio tests comparing p.total ~ as.factor(year) + species + (1|week) to p.total ~ as.factor(year) + (1|week)
LL.glmer.simp = logLik(fit2) ## - 52151 df = 17
LL.glmer.comp = logLik(fit3) ## -51884 df = 18
LRT = -2*(LL.glmer.simp - LL.glmer.comp) ## 533.9
pchisq(LRT, df = 1, lower.tail = FALSE) ## 3.91e-118

###Plotting sea lice abundance per year per species
plot(p.total ~ year, 
     data = FishData[FishData$species == "chum",], 
     col = alpha("seagreen",0.5), 
     pch = 16, cex = 1.0,
     xlab = "Year", 
     ylab = "Sea lice abundance")
points(p.total ~ year,
       data = FishData[FishData$species == "pink",],
       col = alpha("deeppink3", 0.5),
       pch = 16, cex = 1.0)
legend(2014, 80, legend=c("Chum", "Pink"),
       col=c("seagreen", "deeppink3"), pch = 16, cex=0.8) 


