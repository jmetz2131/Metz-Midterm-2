##Setting up the function
integrateIt <- function(x, y, a, b, Rule){ 
  ##The arguments in my integrateIt function are as follows: 
  ## x = a vector of x values, y = a vector of f(x) values, a = the starting point 
  ##from which the user chooses to integrate, b = the ending point to which 
  ##the user chooses to stop the integration, Rule = whether the user is 
  ##using the Trapezoidal Rule or Simpson's Rule
  
  ##For clarification, I am using f(x) values in this function, so am dealing with
  ##y in this function.
  
  ##Because this is in R, I have to account for the fact that the first term of a vector is
  ##1 and not 0. That means that in the actual equation itself for the Simpson's rule, the 
  ##y value of every even-numbered term (n=2,n=4) is multiplied by 4 and the y value 
  ##of every odd-numbered term is multiplied by 2 (n=2,n=4) and the first and last 
  ##y values put into the equation are simply added. The first and last f(x) values 
  ##are represented in my equation by  y[1] and tail(y, n=1), respectively.
  
  n <- length(y) ##n represents the number of partitions the user uses, which in this case
  ##is simply the number of y values themselves. I chose to use length(y) because the y values
  ##are going to be used in the actual Simpson's rule equation itself
  
  odd <- y[y%%2==1]
  
  odd <- odd[-1]
  
  even <- y[y%%2==0]
  
  even <- even[-length(even)]
  
  h <- (b-a)/n ##This represents the delta x value and takes the values set in the arguments
  ##to calculate h. 
  
  if(Rule=="Simp"){
    s_answer <- (h/3)*(y[1]+tail(y,n=1)+2*(sum(y[y%%2==1])))
    ##s_answer is the actual Simpson's rule itself: 
    ##h/3*(f(x0)+4f(x1)+2f(x2)+4f(x3)...+4f(xn-1))+f(xn))
    ##(x[x%%2 == 1])*-1
  }
  return(s_answer) ##This outputs the calculation from s_answer itself
}