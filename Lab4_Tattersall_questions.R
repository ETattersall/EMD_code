## Lab 4: Generalised Linear Models
## Erin Tattersall, July 10, 2017

#### Lab4 pre-lab ####
setwd("C:/Users/ETattersall/Documents/BMSC_EMD/Labs/Lab4")
library(repmis) ## package required to load data

### Loading data
FishData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

## Subsetting data for years after 2010 and all species except sockeye
FishData <- subset(FishData.all,
                   FishData.all$year > 2010 & FishData.all$species != "sockeye")
## Loading corresponding data (temperature, salinity)
SiteData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_siteData.csv")
## Subsetting for years after 2010
SiteData <- subset(SiteData.all, SiteData.all$year > 2010)

## Adding temperature and salinity data to FishData data frame
FishData$temp <- SiteData$temp[match(FishData$site_id, SiteData$site_id)]
FishData$sal <- SiteData$salt[match(FishData$site_id, SiteData$site_id)]

## Creating variable for parasite total
## Calling names for all parasites at all stages
names(FishData)[c(11:25)] 
## Adding across columns to get total parasite load
FishData$p.total<-apply(FishData[,c(11:25)], 1, sum, na.rm=TRUE)
## Column of binary data (infested or not)
FishData$infested<-as.numeric(FishData$p.total>0)
#### Question 1: Body condition and infestation ####
## 1a) 
fit1<-lm(height ~ length * infested, data=FishData) ## Linear model relating height                                                       to length, given infestation
summary(fit1)

# Comparative model without including infestation
fit1b <- lm(height ~ length, data=FishData)
summary(fit1b)

# Likelihood ratio test
## 2*(-log(likelihood simpler)-log(likelihood complex))

## 1b) Calculating likelihood for each model
LL.fit1 = logLik(fit1) ## -28678.44 (df=5): includes residual variance as parameter
LL.fit1b = logLik(fit1b) ##-28682.39 (df=3)
LRT = -2*(LL.fit1b - LL.fit1) ## 7.907 (df = 3)
pchisq(LRT, df = 2, lower.tail = FALSE) ## p = 0.0192

## 1c) Plotting body condition for uninfested and infested fish

slope.infested = sum(summary(fit1)$coefficients[c(2,4)]) 
## for x2 = 1, fish are infected: use coefficients for length and interaction

slope.uninfested = summary(fit1)$coefficients[2]
## for x2 = 0, fish are uninfected, so beta2 and beta3 cancel to zero

intercept2 = sum(fit1$coefficients[c(1,3)]) #infested intercept
#When fish are infected x2 = 1, so B2 acts as a constant to be added to
intercept = summary(fit1)$coefficients[1] #unifested intercept
 
## Plotting height as a function of length for uninfested and infested fish
plot(height~length, 
     data = FishData[FishData$infested == "1",], 
     col = "purple", 
     pch = 16, cex = 1.3, 
     xlab = "Fish length (mm)", 
     ylab = "Fish height (mm)") ## Plotting points for infested fish
points(height~length,
       data = FishData[FishData$infested == "0",],
       col = "red",
       pch = 16, cex = 1.3) ## Plotting points for uninfested fish
abline(a= intercept2, b = slope.infested, col = "blue") ## infested regression line
abline(a= intercept, b = slope.uninfested, col = "green") ## uninfested regression line
legend(40, 30, legend=c("Infested", "Uninfested"),
       col=c("blue", "green"), lwd = 1.2, cex=0.8) ## Adding legend for regression lines

#### 2. Testing the hypothesis that chum and coho salmon have different numbers of parasite ####
## 2a)
fit.glm <- glm(p.total ~ species, data = FishData, family = poisson(link="log"))
summary(fit.glm)
glm.coeff = summary(fit.glm)$coefficients ## Vector of coefficients for both

## Likelihood ratio tests
glm2 <- glm(p.total~1, data = FishData, family = poisson(link = "log")) ### simpler model, p.total doesn't vary with species
summary(glm2)

## logLikelihoods
LL.glm.simp = logLik(glm2) ## -26645df = 1
LL.glm.comp = logLik(fit.glm) ## -26593 df = 2
LRT = -2*(LL.glm.simp - LL.glm.comp) ## 104.1
pchisq(LRT, df = 1, lower.tail = FALSE) ## 1.94e-24


##2c.
## CI.95 = estimate +/- 1.96*SD in linear predictor space
Est.chum = glm.coeff[1,1] ## Estimate of intercept for chum
Est.pink = glm.coeff[1,1] + glm.coeff[2,1] ## Estimate of intercept for pink
SE.chum = glm.coeff[1,2] ## Std.error for chum
### SE.pink = sqrt(Var(U) + Var(V)) = sqrt((Var(U)^2 + (Var(V)^2 + 2*Cov(U,V))
glm.cov = vcov(fit.glm)[2,1]
SE.pink = sqrt((glm.coeff[1,2])^2 + (glm.coeff[2,2])^2 + 2*glm.cov)
## Converting back to real space lambda = exp(LP) upper and lower confidence intervals
UCI.chum =  exp(Est.chum + 1.96*SE.chum)
LCI.chum =  exp(Est.chum - 1.96*SE.chum) 
UCI.pink =  exp(Est.pink + 1.96*SE.pink)
LCI.pink =  exp(Est.pink - 1.96*SE.pink)


##2d.
dev.df.ratio = 33381/18783 ## =1.78 Ratio of residual deviance to degrees of freedom, testing for overdispersion

##2e. Adding species, length and length*species interaction to glm
glm3 <- glm(p.total ~ species + length + species*length,
            data = FishData, 
            family = poisson(link="log"))
summary(glm3)

## Simpler model p.total~species + length
glm3a <- glm(p.total ~ species + length,
            data = FishData, 
            family = poisson(link="log"))


## Test for significant difference against fit.glm (p.total~species) and glm3a (p.total~species + length) = simpler model to glm3
## logLikelihoods for fit.glm and glm3
LL.glm.simp = logLik(fit.glm) ## -26593df = 2
LL.glm.comp = logLik(glm3) ## -26484 df = 4
LRT = -2*(LL.glm.simp - LL.glm.comp) ## 219.2
pchisq(LRT, df = 2, lower.tail = FALSE) ## 2.54e-48

## logLikelihood tests for glm3a and fit.glm
LL.glm.simp = logLik(fit.glm) ## -26593df = 2
LL.glm.comp = logLik(glm3a) ## -26529 df =3
LRT = -2*(LL.glm.simp - LL.glm.comp) ## 127.9
pchisq(LRT, df = 1, lower.tail = FALSE) ## 1.203e-29

## logLikelihood tests for glm3a and glm3
LL.glm.simp = logLik(glm3a) ## -26529 df = 3
LL.glm.comp = logLik(glm3) ## -26484 df = 4
LRT = -2*(LL.glm.simp - LL.glm.comp) ## 91.32
pchisq(LRT, df = 1, lower.tail = FALSE) ##1.22e-21

##Plotting parasite load in chum and pink salmon: regression lines for p.total~species + length + species*length
x.vals = seq(0, 140, 0.1)
y.valsChum = glm3$coefficients[1] + glm3$coefficients[3]*x.vals
y.valsPink = (glm3$coefficients[1] + glm3$coefficients[2]) + (glm3$coefficients[3] + glm3$coefficients[4])*x.vals
## Plotting height as a function of length for uninfested and infested fish
plot(p.total ~ length, 
     data = FishData[FishData$species == "chum",], 
     col = "green", 
     pch = 16, cex = 1.3,
     xlab = "Fish length (mm)", 
     ylab = "No. of parasites") ## Plotting points for infested fish

points(p.total ~length,
       data = FishData[FishData$species == "pink",],
       col = "pink",
       pch = 16, cex = 1.3) ## Plotting points for uninfested fish
lines(x.vals, y.valsChum, col = "blue")
lines(x.vals, y.valsPink, col = "green")
legend(25, 20, legend=c("Chum", "Pink"),
       col=c("blue", "green"), lwd = 1.2, cex=0.8) 


#### Question 3: Testing blue blotch hypotheses ####
##3a
FishData$blue_blotches[is.na(FishData$blue_blotches)]<-0 ## Turning na's into zero

## GLM examining relationship between blue blotches and total parasites
glm4 <- glm(blue_blotches~p.total, data = FishData, family = binomial)
summary(glm4)
glm4.coeff = summary(glm4)$coefficients

glm5 = glm(blue_blotches~1, data = FishData, family = binomial)## simpler model

###
LL.glm.no = logLik(glm5) ## -11376 df = 1
LL.glm.blu = logLik(glm4) ## -11265 df = 2
LRT = -2*(LL.glm.no - LL.glm.blu) ## 222.7
pchisq(LRT, df = 1, lower.tail = FALSE) ##2.32e-050

##Plotting blue blotches as a function of parasites
x.vals = seq(-5,30, 0.1) ## Vector of no.parasites
y = glm3$coefficients[1] + glm3$coefficients[2]*x.vals ## equation for terms in linear predictor space

## Plotting linear predictor space
plot(x.vals, y, 
     type = "l", 
     col = "green", 
     lwd = 1.5, 
     ylab = "Linear predictor", 
     xlab = "Number of parasites")

## Converting back to response variable p(blue blotches)
p = (exp(y))/(1 + exp(y))

## Plotting a presence of blue blotches as a function of parasite load
plot(jitter(blue_blotches)~p.total, 
     data = FishData,
     col = "maroon",
     cex = 0.8, pch = 16,
     xlab = "Number of parasites",
     ylab = "P(blue blotches)")
lines(x.vals, p, type = "l", col = "blue", lwd = 1.5)



## Converting back to response variable p(blue blotches)
CI.int = exp(1.96*SE.int)/(1+exp(1.96*SE.int))
CI.blu = exp(1.96*SE.blu)/(1+exp(1.96*SE.blu))
mean.int = (exp(Est.int)/(1 + exp(Est.int)))## 0.184
mean.blu = (exp(Est.blu)/(1 + exp(Est.blu))) ##0.187


##3b. Examining relationship between blue blotches and salinity
glm6 <- glm(blue_blotches~sal, data = FishData, family = binomial)
summary(glm6)
glm6.coeff = summary(glm4)$coefficients


## Likelihood ratio tests
glm5 <- glm(blue_blotches~1, data = FishData, family = binomial) ### simpler model, where blue blotches doesn't vary as a function of salinity
summary(glm5)

LL.glm.no = logLik(glm5) ## -11376 df = 1
LL.glm.blu = logLik(glm6) ## -11303 df = 2
LRT = -2*(LL.glm.no - LL.glm.blu) ## 146.3
pchisq(LRT, df = 1, lower.tail = FALSE) ## 1.001e-33
### Both are very poorly fit models. It makes sense that glm5 is poorly fit, glm4 adds significantly more likelihood but this is not biologically significant.



##plotting effect of salinity on blue blotch presence
boxplot(sal~blue_blotches, 
        data = FishData, 
        cex.axis=1.2,
        ylab="Salinity (ppt)", 
        names= c("No blue blotches", "Blue blotches"),
        cex.lab=1.2,col=c("green","blue"),boxwex=0.6)




##Plotting blue blotches as a function of salinity
x.vals = seq(-5,40, 0.1) ## Vector of salinity values
y.glm4 = glm4$coefficients[1] + glm4$coefficients[2]*x.vals ## equation for terms in linear predictor space

## Plotting linear predictor space
plot(x.vals, y.glm4, 
     type = "l", 
     col = "green", 
     lwd = 1.5, 
     ylab = "Linear predictor", 
     xlab = "Salinity")

## Converting back to response variable p(blue blotches)
p.sal = (exp(y.glm4))/(1 + exp(y.glm4))

## Plotting a presence of blue blotches as a function of parasite load
plot(jitter(blue_blotches)~sal, 
     data = FishData,
     col = "maroon",
     cex = 0.8, pch = 16,
     xlab = "Salinity (ppt)",
     ylab = "P(blue blotches)")
lines(x.vals, p.sal, type = "l", col = "blue", lwd = 1.5)
## Plot looks weird --> supposed to?
