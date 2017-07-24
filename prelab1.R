##EMD prelab 1: functions and for loops
## July4,2017

f2 <- function(x,y){
    z1 <- 2*x+y
    z2 <- x+2*y
    return(c(z1,z2))
}
f2(x=2,y=5)

for(i in 1:10) {
  print(i)
}

#Counting integers in vector a
a <- 0
for(i in 1:10){
  a <- a+1
}

# Creating an empty vector and filling it with output from for loop
a <- NULL
for(i in 1:50){
  a[i] <- i
}


a <- seq(1:25)
for(i in 1:length(a)){
  a[i] <- a[i]^2
}
## Same as a^2
