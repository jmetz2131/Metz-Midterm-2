##Setting up the function
integrateIt <- function(x, y, a, b, n, Rule){ 
  ##The arguments in my integrateIt function are as follows: 
    ## x = a vector of x values, y = a vector of f(x) values, a = the starting point 
    ##from which the user chooses to integrate, b = the ending point to which 
    ##the user chooses to stop the integration, n = the number of partitions the 
    ##user wants, Rule = whether the user is using the Trapezoidal Rule or Simpson's Rule
  x <- seq(a, b, by=n) ##UNSURE OF THIS
  
  ##In the actual equation itself for the trapezoidal rule, the y value of every
  ##term is multiplied by 2 except for the first and last x values put into the equation.
  ##The first and last x values are represented in my equation by y[1] and tail(y, n=1), respectively.
  ##The following value penultimate represents the second to last value. I needed it to
  ##sum the y values of the second term to the penultimate term as dictated by the number
  ##of partitions the user chooses.
  penultimate <- length(y)-1 
  
  h <- (b-a)/n ##This represents the delta x value and takes the values set in the arguments
  ##to calculate h. 
  
  if(Rule=="Trap"){
    t_answer <- (h/2)*(y[1]+tail(y,n=1)+2*(sum(y[2:penultimate])))
    ##t_answer is the actual Trapezoidal rule itself: 
    ##h/2*(f(x0)+2(f(x1)+...f(xn-1))+f(xn))
  }
  return(t_answer) ##This outputs the calculation from t_answer itself
}

##These are my test vectors for integrating from 1 to 5 on the equation 2x.
x<-seq(1, 5, by=1)
y<-c(2,4,6,8,10)

integrateIt(x, y, 1, 5, 4,"Trap")
