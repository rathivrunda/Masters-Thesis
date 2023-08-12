library(reshape2)
library("ggplot2")
source("Network.R")


#parameters

#discretization parameter
m= 10000
#exponent of distance prportion
exp = 1
#size of network grid p*q
p =50
q= 50

PQ= p*q

#number of iterations
T = 2000


#initial opinions, uniform iid
A = runif(PQ, min =0, max =1)
#A<- matrix(A, nrow= q)
#A <-matrix(A, nrow=1)


#form network matrix

M <- randplanar_adjmatrix(p, q, exp)
msum<-rowSums(M)
M<-t(t(M/msum))
hist(msum)
#update dynamics
             

for (t in 1:T){
  A = M %*% A
  for (i in 1:PQ){
    if((A[i]*m - floor(A[i]*m)) > (ceiling (A[i] *m) - A[i]*m)){
      A[i] <- (ceiling (A[i] *m))/m
    } else if ((A[i] - floor(A[i]*m)) < (ceiling (A[i] *m) - A[i])){
      A[i] <- (floor (A[i] *m))/m
    }
    i = i+1
  }

  A[384] <- 0.1
  
  t=t+1
}


#print image
 
Adf <- data.frame (matrix(A, nrow= p))


flat_df <- melt(t(Adf), value.name = "Value") 

ggplot(data=flat_df,aes(y=Var2,x=Var1))+geom_tile(aes(fill=Value))+ scale_fill_gradient2(
  low = "red",
  mid = "yellow",
  high = "dark green",
  midpoint = 0.5,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill",
  limits = c(0,1)
)


