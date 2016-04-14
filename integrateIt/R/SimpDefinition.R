#' Simpson's Rule class definition
#' 
#' Object of class \code{Simp} as created by the \code{integrateIt} functions
#'
#' 
#' An object of the class Simp has the following slots:
#' \itemize{
#' \item \code{x} The x values for a function the user chooses to integrate using Simpson's Rule
#' \item \code{y} The y values for a function the user chooses to integrate using Simpson's Rule
#' \item \code{result} This is the result of integrating the function with Simpson's Rule.
#' }
#'
#' @author Jacob Metz: \email{jacob.metz@wustl.edu}
#' @aliases Simp-class initialize,Simp-method plot,Simp-method print
#' @rdname Simpson's Rule
#' @export
##Setting up a class Simp to output the x and y values of a function
##and the resulting integration when using Simpson's Rule.
setClass(Class="Simp",
         representation=list(
           x = "numeric",
           y = "numeric",
           result = "numeric"
         ),
         prototype=prototype(
           x = c(),
           y = c(),
           result = c()
         )
)
#' @export
##This method allows me to specify what a new object of class Trap would require
setMethod("initialize", "Simp", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setMethod("print", "Simp",
          function(x){
            cat("This integration was calculated using Simpson's Rule.")
            cat(x@result)
          })

#' @export
setValidity("Simp", function(object){
  evenPartitions <- length(object@y)%%2==1
  dataPoints <- length(object@x)==length(object@y)
  if(!evenPartitions | !dataPoints){
    return("The user has not entered values consistent with the rules set up for this class! Try again!")
  }
})










