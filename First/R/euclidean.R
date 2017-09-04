euclidean <-function(a,b){
  
  stopifnot(is.numeric(a) | length(a) > 1 & is.numeric(b) | length(b) > 1)
  
  vect <- c(a,b)
  
  a <- max(vect)
  b <- min(vect)
  
  q <- 1
  r0 <- 1
  #s? l?nge r0 inte ?r = 0 s? ska den forts?tta
  while(r0 != 0){
    #hittar vilket q som ?r det h?gsta m?jliga, men g?r ett q f?r mycket s? tar bort det :)
    while( (q)*b <= a ){
      q <- q+1
    }
    q <- q-1
    r0 <- a - q*b
    a <- b
    b <- r0
    #?terst?ller q
    q <- 1
    
  }
  
  return(a)
}