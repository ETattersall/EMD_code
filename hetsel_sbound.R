## Modeling differential selection between genotypes ##

## Stable equilibrium, mean fitness = 1. Genotype fitnesses:
#w.AA = 1-t
#w.Aa = 1-s
#w.aa = (1-p^2(1-t)-2*p*q*(1-s))/q^2

#### Solving for selection against heterozygotes ####
## Input = p for given population
hetsel = function(p,t){
  a = p*t
  b = 3*(1-p)
  s=(2/3)-(a/b)
  return(s)
}
s= seq(0,1,0.01)
t = seq(0,1,0.01) #t as a range between 0-1
s.PR = hetsel(p = 0.69, t = t) ## p for Princess Royal = 0.69
## Line equation at p = 0.69
slope.PR = 0.69/(3*(0.31))
## s = 0.67 - 0.74*t
## PR s=t
hetsel(p = 0.69, t = 0.38) ## s = 0.38 SAME SAME



#### Bounding possible selection coefficients for w.aa > 1 ####
## s > p*t/2*(1-p)
s.bound = function(p,t){
  a = p*t
  b = 2*(1-p)
  s = a/b
  return(s)
}
s.boundPR = s.bound(p = 0.69, t = t) ## p value for Princess Royal p = 0.69


#### Plotting coefficient lines ####
### Plotting t-s line 
plot(x = t, y = s.PR, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Princess Royal Island (q = 0.3)") 
lines(x = t, y = s.boundPR, type = "l", lty = 2, col = "dodgerblue4") ## Bounding line (line below = coefficents without het. disadvantage)




### Shading in regions where w.aa < 1 (out of bounds for our selection hypothesis)
cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.69, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = s.PR, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Princess Royal Island (q = 0.31)")
lines(x = t, y = s.boundPR, type = "l", lty = 2, col = "dodgerblue4")
# Finding intersection point
locator(n = 1, type = "p") ##(0.36, 0.40)
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
points(x = 0.38, y = 0.38, pch = 16, cex = 1.6, col = "red2") ## s =t
lines(x = t, y = s, col = "red", type = "l") # s= t line

### Gribbell Island p = 0.3
s.Grib = hetsel(p = 0.7, t = t)
s.boundGrib = s.bound(p = 0.7, t = t)
## Line equation at p = 0.7
slope.PR = 0.7/(3*(0.3))
## s = 0.67 - 0.78*t
## Gribbell s=t
hetsel(p = 0.7, t = 0.375) ## s = 0.375

cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.7, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = s.Grib, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Gribbell Island (q = 0.30)")
lines(x = t, y = s.boundGrib, type = "l", lty = 2, col = "dodgerblue4")
# Finding intersection point
locator(n = 1, type = "p") ##(0.34, 0.40)
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
points(x = 0.375, y = 0.375, pch = 16, cex = 1.6, col = "red2") ## s =t
lines(x = t, y = s, col = "red", type = "l") # s= t line

### Gribbell Island, Hedrick and Ritland values p = 0.435
s.G2012 = hetsel(p = 0.43, t = t)
s.boundG2012 = s.bound(p = 0.43, t = t)
## Line equation at p = 0.43
slope.PR = 0.43/(3*(0.57))
## s = 0.67 - 0.25*t
t = seq(0,1,0.01)
cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.43, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = s.G2012, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Gribbell Island (Ritland et al., 2001; q = 0.57)")
lines(x = t, y = s.boundG2012, type = "l", lty = 2, col = "dodgerblue4")
# Finding intersection point
locator(n = 1, type = "p") ##(No intersection point- 1.0,0.41)
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
points(x = 0.53, y = 0.53, pch = 16, cex = 1.6, col = "red2") ## s =t
## Gribbell old
hetsel(p = 0.43, t = 0.53) ## 0.53


### Roderick Island, p = 0.96
s.Rod = hetsel(p = 0.96, t = t)
s.boundRod = s.bound(p = 0.96, t = t)
## Line equation at p = 0.96
slope.PR = 0.96/(3*(0.04))
## s = 0.67 - 8*t
## Roderick s=t?
hetsel(p=0.96, t = 0.02) ## No

cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.96, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = s.Rod, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Roderick Island (q = 0.04)")
lines(x = t, y = s.boundRod, type = "l", lty = 2, col = "dodgerblue4")
# Finding intersection point
locator(n = 1, type = "p") ##(0.0326, 0.397)
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)




#### Constructing a heat map for q freq with heterozygote disadvantage ###
## Need a matrix of q's for which s and t combinations give heterozygote disadvantage
## Model solved for q
hetsel.q = function(s,t){ ##Solving for q
  a = s-(2/3)
  b = 3/t
  c = a*b
  d = a*b-1
  p = c/d
  q = 1 - p
  return(q)
}
#testing that equations are equivalent
hetsel(p = 0.69,t = 0.36)
hetsel.q(s=0.40,t = 0.36)


###### Redoing Roderick Island for p = 0.04
s.Rod = hetsel(p = 0.04, t = t)
s.boundRod = s.bound(p = 0.04, t = t)
## Line equation at p = 0.96
slope.PR = 0.96/(3*(0.96))
## s = 0.67 - 8*t

cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.96, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = s.Rod, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "Roderick Island (q = 0.96)")
lines(x = t, y = s.boundRod, type = "l", lty = 2, col = "dodgerblue4")
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
lines(x = t, y = s, col = "red", type = "l")

hetsel(p = 0.04, t = 0) ## 0.67
hetsel(p = 0.04, t = 1) ## 0.65
hetsel(p = 0.04, t = 0) ## 0.67
hetsel(p = 0.04, t = 0.66) ## 0.66




### q = 0.2, p = 0.8
p.2=hetsel(p = 0.65, t = t)
boundp.2 = s.bound(p=0.65, t = t)
cord.x <- c(-0.05,seq(-0.05,1.05,0.01), 1.05) 
cord.y <- c(-0.05,s.bound(p = 0.65, t = seq(-0.05,1.05,0.01)),-0.05) 
plot(x = t, y = p.2, 
     type = "l",
     lwd  = 1.6,
     ylab = "s",
     cex.lab  = 1.4,
     ylim = c(0,1),
     xlim = c(0,1),
     main = "q = 0.35")
lines(x = t, y = boundp.2, type = "l", lty = 2, col = "dodgerblue4")
# Finding intersection point
locator(n = 1, type = "p") ##(0.36, 0.40)
# Shading out values for which there is no heterozygote disadvantage
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
points(x = 0.38, y = 0.38, pch = 16, cex = 1.6, col = "red2") ## s =t
lines(x = t, y = s, col = "red", type = "l") # s= t line



#### Giving up on heat map idea ####
## Creating the matrix to feed into heat map
## Testing range of s and t that make sense
p = seq(0,1,0.1)

test = hetsel(p=p,t=0)#s is 0.67 at t = 0 for all values of p
s = seq(0,1,0.01) ## vector accepting s
t = seq(0,1,0.01) ## vector accepting t

q = matrix(0, nrow = length(s), ncol = length(t)) ## matrix accepting s,t combinations, giving q values
i = 1 #Indexing row being populated
j = 1 #Indexing column being populated
for (T in t){
  for (S in s){
    q[i, j] = hetsel.q(s=S, t=T) ##Populating cells with q's
    i=i+1 # Moving down a row
  }
  i=1 #Resetting to top row
  j=j+1 #Moving over a column
}

## Creating the heat map itself
heatmap(q, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10), xlab = "t", ylab = "s")



library(plotly) ### Getting errors loading plotly
plot_ly(z = q, type = "heatmap",color = colorRamp(c("red", "yellow")))
## Heat map looks weird as fuuuuuck