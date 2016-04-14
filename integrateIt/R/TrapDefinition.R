#' Trapezoidal Rule class definition
#' 
#' Object of class \code{Trap} as created by the \code{integrateIt} functions
#'
#' 
#' An object of the class Trap has the following slots:
#' \itemize{
#' \item \code{x} The x values for a function the user chooses to integrate using the Trapezoidal Rule
#' \item \code{y} The y values for a function the user chooses to integrate using the Trapezoidal Rule
#' \item \code{result} This is the result of integrating the function with the Trapezoidal Rule.
#' }
#'
#' @author Jacob Metz: \email{jacob.metz@wustl.edu}
#' @aliases Trap-class initialize,Trap-method plot,Trap-method print
#' @rdname Trapezoidal Rule
#' @export
##Setting up a class Trap to output the x and y values of a function
##and the resulting integration when using the Trapezoidal Rule.
setClass(Class="Trap",
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
setMethod("initialize", "Trap", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setMethod("print", "Trap",
          function(x){
            cat("This integration was calculated using the Trapezoidal Rule.")
            cat(x@result)
          })
#' @export
setValidity("Trap", function(object){
  dataPoints <- length(object@x)==length(object@y)
  if(!dataPoints){
    return("The user has not entered values consistent with the rules set up for this class! Try again!")
  }
})

