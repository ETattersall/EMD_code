### Function estimating heterozygote selection given a range of q's
#### Checking if my 2 equations are equivalent ####
## Both take values of q and t (both between 0 and 1)
# Output = s

## 1. Equation 1 ##
hetsel1 = function(q, t){
  w.AA = 1-t
  p    = 1-q
  a    = q*(q-1)+p^2*w.AA
  b    = p*(1-2*q)
  s    = 1-(a/b)
  return(s)
}

## 2. Equation 2 ##
hetsel2 = function(q, t){
  w.AA = 1-t
  p    = 1-q
  a    = q*(1-q)- p^2*w.AA
  b    = p*(2*q-1)
  s    = 1 -(a/b)
  return(s)
}

## Plot for a range of q's, t = 0.2 
q = seq(0,1,0.01)
s1 = hetsel1(q =q, t = 0.2) 
s2 = hetsel2(q =q, t = 0.2)

plot(q,s1,
     col = "red",
     type = "l")
plot(q,s2,
     col = "blue",
     type = "l")
### Equations are the same!!YAY