
planar_adjmatrix <- function(p, q) {
  M <- array(numeric(0), c(p*q, p*q))
  
  # Go through each house on each street and determine their influence on every node, house by house, street by street
  for (i1 in 1:p){
    for(j1 in 1:q){
      for (i2 in 1:p){
        for(j2 in 1:q){
            if(((abs (i1 - i2) <= 1) & (abs(j1 -j2) <= 1)) & (abs(i1-i2)+abs(j1-j2) != 0)){
            M[(j1*p + i1 - p), (j2*p + i2 -p)]= 1
          } else {
            M[(j1*p + i1 - p), (j2*p + i2 -p)]= 0
          }

        }
      } 
      
    }
  }
  return (M)
}

randplanar_adjmatrix <- function(p, q, exp) {
  M <- array(numeric(0), c(p*q, p*q))
  
  # Go through each house on each street and determine their influence on every node, house by house, street by street
  for (i1 in 1:p){
    for(j1 in 1:q){
      for (i2 in 1:p){
        for(j2 in 1:q){
          
          dist = sqrt((i1-i2)*(i1-i2) + (j1- j2)*(j1- j2))
          if (dist == 0 ){
            M[(j1*p + i1 - p), (j2*p + i2 -p)] = 0
          } else{
            M[(j1*p + i1 - p), (j2*p + i2 -p)] = rbinom(1, 1, 1/(dist)^exp)
          }
          
          
        }
      } 
      
    }
  }
  return (M)
}