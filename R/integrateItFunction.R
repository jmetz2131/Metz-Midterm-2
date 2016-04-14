##Setting up the function
integrateIt <- function(x, y, a, b, Rule){ 
  ##The arguments in my integrateIt function are as follows: 
    ## x = a vector of x values, y = a vector of f(x) values, a = the starting point 
    ##from which the user chooses to integrate, b = the ending point to which 
    ##the user chooses to stop the integration, Rule = whether the user is 
    ##using the Trapezoidal Rule or Simpson's Rule
  
  ##For clarification, I am using f(x) values in this function, so am dealing with
  ##y in this function.
  
  n <- length(y)-1 ##n represents the number of partitions the user uses, which in this case
  ##is simply the number of y values themselves. I chose to use length(y) because the y values
  ##are going to be used in the actual equations themselves.
  
  h <- (b-a)/n ##This represents the delta x value and takes the values set in the arguments
  ##to calculate h. 
  
  if(Rule=="Trap"){
    penultimate <- length(y)-1   ##In the actual equation itself for the trapezoidal rule, the y value of every
    ##term is multiplied by 2 except for the first and last x values put into the equation.
    ##The first and last x values are represented in my equation by y[1] and tail(y, n=1), respectively.
    ##The following value penultimate represents the second to last value. I needed it to
    ##sum the y values of the second term to the penultimate term as dictated by the number
    ##of partitions the user chooses.
    
    answer <- (h/2)*(y[1]+y[length(y)]+2*(sum(y[2:penultimate])))
    ##answer is the actual Trapezoidal rule itself: 
    ##h/2*(f(x0)+2(f(x1)+...f(xn-1))+f(xn))
  }
  
  if(Rule=="Simp"){
    noEndPoints <- y[-c(1, length(y))]
    
    even <- noEndPoints[c(TRUE, FALSE)]
    
    odd <- noEndPoints[c(FALSE, TRUE)]
    
    answer <- (h/3)*(y[1]+y[length(y)]+4*(sum(even)) + 2*(sum(odd)))
    ##answer is the actual Simpson's rule itself: 
    ##h/3*(f(x0)+4f(x1)+2f(x2)+4f(x3)...+4f(xn-1))+f(xn))
  }
  return(answer) ##This outputs the calculation from the Rule the user specifies
}

##These are my test vectors for integrating from 1 to 5 on the equation 2x.
test_x<-seq(1, 7, by=1)
test_y<-c(sqrt(1:7))

length(test_y)

integrateIt(test_x, test_y, 1, 7,"Trap")

integrateIt(test_x,test_y, 1,7,"Simp")

