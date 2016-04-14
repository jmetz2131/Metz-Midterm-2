##Setting up the function
integrateIt <- function(x, y, a, b, Rule){ 
  ##The arguments in my integrateIt function are as follows: 
    ## x = a vector of x values, y = a vector of f(x) values, a = the starting point 
    ##from which the user chooses to integrate, b = the ending point to which 
    ##the user chooses to stop the integration, Rule = whether the user is 
    ##using the Trapezoidal Rule or Simpson's Rule
  
  ##For clarification, I am using f(x) values in this function, so am dealing with
  ##y in this function.
  
  n <- length(y)-1 ##n represents the number of subdivisions the user uses. 
  ##I subtracted one from length(y) to ensure that the width of each subdivision
  ##was 1 like Professor Montgomery and I discussed. For example, an integration from
  ##1 to 9 would have 8 subdivisions and this code reflects that.
 
  h <- (b-a)/n ##This represents the delta x value and takes the values set in the arguments
  ##to calculate h. 
  
  if(Rule=="Trap"){
    penultimate <- length(y)-1   ##In the actual equation itself for the trapezoidal rule, the y value of every
    ##term is multiplied by 2 except for the first and last x values put into the equation.
    ##The first and last x values are represented in my equation by y[1] and y[length(y)], respectively.
    ##The following value penultimate represents the second to last value. I needed it to
    ##sum the y values of the second term to the penultimate term as dictated by the number
    ##of partitions the user chooses.
    
    answer <- (h/2)*(y[1]+y[length(y)]+2*(sum(y[2:penultimate])))
    ##answer is the actual Trapezoidal rule itself: 
    ##h/2*(f(x0)+2(f(x1)+...f(xn-1))+f(xn))
  }
  
  if(Rule=="Simp"){
    noEndPoints <- y[-c(1, length(y))] 
    ##Given the Simpson's Rule, I used this code to remove the first and last
    ##values because those two values will not be multiplied in any way in the
    ##rule itself.
    
    even <- noEndPoints[c(TRUE, FALSE)]
    ##Here I used noEndPoints and used the TRUE, FALSE pattern above  
    ##to isolate the even numbered indices so I know which f(x) values to
    ##multiply by 4.
    
    odd <- noEndPoints[c(FALSE, TRUE)]
    ##Here I used noEndPoints and used the TRUE, FALSE pattern above 
    ##to isolate the odd numbered indices so I know which f(x) values to 
    ##multiply by 2.
    
    answer <- (h/3)*(y[1]+y[length(y)]+4*(sum(even)) + 2*(sum(odd)))
    ##answer is the actual Simpson's rule itself: 
    ##h/3*(f(x0)+4f(x1)+2f(x2)+4f(x3)...+4f(xn-1))+f(xn))
  }
  return(answer) ##This outputs the calculation from the Rule the user specifies
}

##These are my test vectors for integrating from 1 to 9 on the equation sqrt(x).
test_x<-seq(1, 9, by=1) ##x values from 1 to 9 for y=sqrt(x)
test_y<-c(sqrt(1:9)) ##y values for f(1) to f(9) for y=sqrt(x)

integrateIt(test_x, test_y, 1, 9, "Trap") ##This correctly comes out to 17.306

integrateIt(test_x,test_y, 1, 9, "Simp") ##This correctly comes out to 17.33209

##The definite integral value of y=sqrt(x) from 1 to 9 is 17.333. The Simpson's Rule
##equation is supposed to be more accurate than the Trapezoidal Rule equation and they 
##both come close to the actual definite integral value.

