#'@title Takes the euclidian algorithm
#'
#'@description An algorithm that computes the greatest common divisor (GDC) of two numeric numbers.
#'@param a shall be a numeric value and a scalar
#'@param b shall be a numeric value and a scalar
#'@export
#'@return Returns a numeric scalar that is the greatest common divisor of a and b.
#'@examples 
#'euclidean(123612, 13892347912)
#'euclidean(100, 1000)
#'@references The info about the algorithm can be found at this link  
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{wikepedia.}

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