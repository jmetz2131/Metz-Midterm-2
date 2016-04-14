#' integrateIt function
#'
#' integrates a given dataset of values and caluclates the output value based on either the Trapezoidal rule or Simpson's Rule
#'
#' @param x This is a vector of x values that the user inputs 
#' @param y This is a vector of f(x) values that the user inputs
#' @param a The starting point from which the user chooses to integrate
#' @param b The ending point to which the user chooses to stop the integration
#' @param Rule Whether the user is using the Trapezoidal Rule or Simpson's Rule
#' @return An object of class Squares containing
#'  \item{x}{This is a vector of x values that the user inputs} 
#'  \item{y}{This is a vector of f(x) values that the user inputs}
#'  \item{result}{The calculated output when using one of the rules}
#' @author Jacob Metz
#' @examples
#' 

#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
#####Setting up the function
integrateIt <- function(x, y, a, b, Rule){ 
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
    
    output <- new("Trap", x=x, y=y,result=answer ) ##This gives the correct output in line with what the items in help file specify.
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
    
    output<-new("Simp", x=x, y=y, result=answer) ##This gives the correct output in line with what the items in help file specify.
  }
  return(output) ##This outputs the calculation from the Rule the user specifies
}