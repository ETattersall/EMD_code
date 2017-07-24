##### Checking heterozygote selection model for varying t

## Given differential selection against homozygous dominant and heterozygotes where:
## w.AA = 1-t
## w.Aa = 1-s
## w.aa = 1


#### Plotting s for a range of q's at various t values

## Model equation
hetsel = function(p,t){
  a = p*t
  b = 3*(1-p)
  s=(2/3)-(a/b)
  return(s)
}

hetsel(p = 0.67, t = 0.40)


## Plotting over a range of q's, varying t
q = seq(0,1,0.01)
p = 1 - q # to input into function

## Bound for heterozygote disadvantage
#### Bounding possible selection coefficients for w.aa > 1 ####
## s > p*t/2*(1-p)
s.bound = function(p,t){
  a = p*t
  b = 2*(1-p)
  s = a/b
  return(s)
}

###Re-arranged to solve for q
q.func = function(s,t){
  a=2*s
  b=t+2*s
  q=1-a/b
  return(q)
}



## For t= 0 
t0 = hetsel(p = p, t=0)
t.2 = hetsel(p=p, t=0.2)
t.4 = hetsel(p=p, t=0.4)
t.6 = hetsel(p=p, t=0.6)
t.8 = hetsel(p=p, t=0.8)
t1 = hetsel(p=p, t=1)

## Finding s values for bounding
boundt0=s.bound(p=p,t=0)
boundt.2=s.bound(p=p,t=0.2)
boundt.4=s.bound(p=p,t=0.4)
boundt.6=s.bound(p=p,t=0.6)
boundt.8=s.bound(p=p,t=0.8)
boundt1=s.bound(p=p,t=1.0)

####ADD VERTICAL LINES
## t0
plot(t0,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     main = "t=0",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt0, y = q, type = "l", lty = 2, col = "dodgerblue4")

## For t= 0.2
##Shading
###Re-arranged to solve for q (for shading under the curve)
q.func = function(s,t){
  a=2*s
  b=t+2*s
  q=1-a/b
  return(q)
}

########
cord.x <- c(-0.05,boundt.2,-0.05) 
cord.y <- c(-0.05,q.func(s = boundt.2, t=0.2),-0.05)  
plot(t.2,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "t = 0.2",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt.2, y = q, type = "l", lty = 2, col = "dodgerblue4")
abline(v= 0.68, col = "red")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
q.func(s=0.2, t =0.2) 
#q=0.33 at s=t



## For t= 0.4
cord.x <- c(-0.05,boundt.4,-0.05) 
cord.y <- c(-0.05,q.func(s = boundt.4, t=0.4),-0.05)  
plot(t.4,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "t = 0.4",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt.4, y = q, type = "l", lty = 2, col = "dodgerblue4")
abline(v= 0.68, col = "red")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
q.func(s=0.4, t =0.4)
#q=0.33 at s=t
points(x = 0.4, y = 0.33, pch = 16, cex = 1.6, col = "red2") ## s =t

## For t= 0.6 
cord.x <- c(-0.05,boundt.6,-0.05) 
cord.y <- c(-0.05,q.func(s = boundt.6, t=0.6),-0.05)  
plot(t.6,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "t = 0.6",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt.6, y = q, type = "l", lty = 2, col = "dodgerblue4")
abline(v= 0.68, col = "red")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
points(x = 0.6, y = 0.33, pch = 16, cex = 1.6, col = "red2") ## s =t
q.func(s=0.6, t =0.6)
#q=0.33 at s=t

## For t= 0.8 
cord.x <- c(-0.05,boundt.8,-0.05) 
cord.y <- c(-0.05,q.func(s = boundt.8, t=0.8),-0.05)  
plot(t.8,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "t = 0.8",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt.8, y = q, type = "l", lty = 2, col = "dodgerblue4")
abline(v= 0.68, col = "red")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)

## For t= 1.0 
cord.x <- c(-0.05,boundt1,-0.05) 
cord.y <- c(-0.05,q.func(s = boundt1, t=1.0),-0.05)  
plot(t1,q, ylab = "q",
     xlab = "s",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "t = 1.0",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = boundt1, y = q, type = "l", lty = 2, col = "dodgerblue4")
abline(v= 0.68, col = "red")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)

## Checking polynomials for s= 0.2, t= 0.2, q=0.2
#1
(0.2)^2*(2*0.2-0.2)+0.2*(2*0.2-3*0.2)+0.2-0.2 ##-0.032

#2
(0.2)^2*(2-0.2-2*0.2)+0.2*(3*0.2+2*0.2-3)-0.2-0.2-1
##-1.744

#### Given q = 0.3, solve s for a range of t
t = seq(0,1, 0.01)
s.3 = hetsel(q=0.3, t)
sel.coef.3 = as.data.frame(cbind(t,s.3))
## What is the slope?
sel.coef.3$st.ratio = sel.coef.3[,2]/sel.coef.3[,1]


plot(t,s, type = "l",
     col = "mediumpurple")
# q= 0.4
s.4=hetsel(q=0.4, t)
sel.coef.4 = as.data.frame(cbind(t,s.4))
sel.coef.4$st.ratio = sel.coef.4[,2]/sel.coef.4[,1]

#q = 0.1
s.1=hetsel(q=0.1, t)
sel.coef.1 = as.data.frame(cbind(t,s.1))
sel.coef.1$st.ratio = sel.coef.1[,2]/sel.coef.1[,1]

plot(t,s.4, type = "l",
     col = "mediumseagreen",
     ylim = c(0,1))
lines(t,s, type = "l",col = "mediumpurple")
lines(t,s.1, type = "l", col = "deeppink3")
legend(0.6, 0.5, legend=c("q = 0.4", "q = 0.3", "q=0.1"),
       col = c("mediumseagreen", "mediumpurple", "deeppink3"), lwd = 1.3)


## Plot s/t as a function of q?
# Given range of q between 0 and .5 and t values


#### New function when mean fitness w(bar) = 1  ####
##w.aa = (1-p^2*w.AA-2*p*q*w.Aa)/q^2
func = function(q,t){
  p=1-q
  a= 1-p^2*(1-t)-q
  b= 3*p*q
  s = 1 - a/b
  return(s)
}

###function for t>p/(2(1-p))*s
t.great = function(q,s){
  p=1-q
  t=(2*(1-p)/p)*s
  return(t)
}

s=seq(0,1,0.01)
tfit=t.great(q=0.3,s=s)

## Solve given q = 0.3 for a range of t
sfit = func(q=0.3, t=t)
plot(t,sfit, type = "l",
     ylim = c(0,1),
     ylab = "s")
lines(tfit,s, type = "l", col = "darkgreen")

sfit1 = func(q = 0.1, t= t)

###
