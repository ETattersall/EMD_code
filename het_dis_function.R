#### 1. Creating a  for changing allele frequencies under heterozygote disadvangtage ####
library(dplyr)
## Where fitness coefficients (w) are:
## w.AA: 1-t
## w.Aa: 1-s
## w.aa: 1
## Function should receive q,s, and t; give output dq (change in q)

hetdef = function(q,s,t){
  p    = 1-q
  w.AA = 1-t
  w.Aa = 1-s
  w.aa = 1
  a    = p*q*(q*(w.aa - w.Aa) + p*(w.Aa - w.AA))
  b    = p^2*w.AA + 2*p*q*w.Aa + q^2*w.aa
  dq   = a/b
  return(dq)
}

## Plotting with differing t's
s = seq(0,1,0.01) #For w.Aa between 0 and 1

#t=0 (no selection)
dqt.0 = hetdef(q=0.565, s= s, t=0)
q.freq = as.data.frame(cbind(s,dqt.0))
plot(x = q.freq$s, y = q.freq$t.0, 
     xlab = "s", ylab = "delta.q", 
     type = "l", col = "darkgreen", 
     main = "t = 0")
## at dq = 0, s = 0
## x- axis is wrong scale, but adding xlim = c(0,1) only give dq = 0 at s = 1 (wrong...)


##Solve for values of s = 0-1 with constant q = 0.565 and t = 0.2 (from Hedrick & Ritland)
dqt.2 = hetdef(q = 0.565,s, t=0.2)
q.freq$dqt.2 = dqt.2
plot(x = q.freq$s, y = q.freq$dqt.2, 
     xlab = "s", ylab = "delta.q", 
     type = "l", col = "red", 
     main = "t = 0.2")
## at dq = 0, s = 0.022 

##t = 0.5
dqt.5 = hetdef(q=0.565, s= s, t=0.5)
q.freq$t.5 = dqt.5
plot(x = q.freq$s, y = q.freq$t.5,
     xlab = "s", ylab = "delta.q", 
     type = "l", col = "blue", 
     main = "t = 0.5")

## at dq = 0, s = 0 .059


##t = 0.8
dqt.8 = hetdef(q=0.565, s= s, t=0.8)
q.freq$t.8 = dqt.8
plot(x = q.freq$s, y = q.freq$t.8,
     xlab = "s", ylab = "delta.q", 
     type = "l", col = "orange3", 
     main = "t = 0.8")
## at dq = 0, s = 0.10

##t = 1.0
dqt1 = hetdef(q=0.565, s= s, t=1)
q.freq$t1 = dqt1
plot(x = q.freq$s, y = q.freq$t1,
     xlab = "s", ylab = "delta.q", 
     type = "l", col = "mediumpurple", 
     main = "t = 1.0")
## at dq = ~0, s = 0.13

