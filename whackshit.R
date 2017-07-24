##### Plotting q for a range of t at given s

## Differential selection rearranged for t
bwdis = function(q,s){
  a = (3*q)/(1-q)
  b = (2/3)-s
  t = a*b
  return(t)
}

## Bounding curve rearranged
###Re-arranged to solve for q (for shading under the curve)
q.func = function(s,t){
  a=2*s
  b=t+2*s
  q=1-a/b
  return(q)
}

q = seq(0,1,0.01)
### Output = t's for a range of q's
s0 = bwdis(q = q, s = 0)
s.2 = bwdis(q = q, s = 0.2)
s.4 = bwdis(q = q, s = 0.4)
s.6 = bwdis(q = q, s = 0.6)
s.8 = bwdis(q = q, s = 0.8)
s.1 = bwdis(q = q, s = 1)

### Bounding curve
t.bound = function(q,s){
  a=2*s*q
  b= 1-q
  t = a/b
  return(t)
}
### Values of t delineating advantage line
bound0 = t.bound(q = q, s = 0)
bound.2 = t.bound(q = q, s = 0.2)
bound.4 = t.bound(q = q, s = 0.4)
bound.6 = t.bound(q = q, s = 0.6)
bound.8 = t.bound(q = q, s = 0.8)
bound1 = t.bound(q = q, s = 1)



###### Didn't get plots to make any sense....
## s0
plot(s0,q, ylab = "q",
     xlab = "t",
     cex.lab = 1.4,
     type = "l",
     main = "s=0",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = bound0, y = q, type = "l", lty = 2, col = "dodgerblue4")

## For s= 0.2
##Shading
###Re-arranged to solve for q (for shading under the curve)
q.func = function(s,t){
  a=2*s
  b=t+2*s
  q=1-a/b
  return(q)
}

########
cord.x <- c(-0.05,bound.2,-0.05) 
cord.y <- c(-0.05,q.func(s = 0.2, t=bound.2),-0.05)  
plot(s.2,q, ylab = "q",
     xlab = "t",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "s = 0.2",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = bound.2, y = q, type = "l", lty = 2, col = "dodgerblue4")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)




## For s= 0.4
cord.x <- c(-0.05,bound.4,-0.05) 
cord.y <- c(-0.05,q.func(s = 0.4 , t= bound.4),-0.05)  
plot(s.4,q, ylab = "q",
     xlab = "t",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "s = 0.4",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = bound.4, y = q, type = "l", lty = 2, col = "dodgerblue4")
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)

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
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)

## For s= 0.8 
cord.x <- c(-0.05,bound.8,-0.05) 
cord.y <- c(-0.05,q.func(s = 0.8, t=bound.8),-0.05)  
plot(s.8,q, ylab = "q",
     xlab = "t",
     cex.lab = 1.4,
     type = "l",
     #col = "dodgerblue4", 
     main = "s = 0.8",
     ylim =  c(0.04,1),
     xlim = c(0.04,1))
lines(x = bound.8, y = q, type = "l", lty = 2, col = "dodgerblue4")
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
polygon(cord.x,cord.y,col='dodgerblue4', density = 30, angle = 35)
