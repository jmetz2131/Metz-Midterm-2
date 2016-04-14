## Load libraries and set working directory
library(devtools)
library(roxygen2)
getwd()
setwd("/Users/jacobmetz/Desktop/New Class/Metz-Midterm-2")

package.skeleton(name = "integrateIt")

current.code <- as.package("primaryrace")
load_all(current.code)

##These are my test vectors for integrating from 1 to 9 on the equation sqrt(x).
test_x<-seq(1, 9, by=1) ##x values from 1 to 9 for y=sqrt(x)
test_y<-c(sqrt(1:9)) ##y values for f(1) to f(9) for y=sqrt(x)

integrateIt(test_x, test_y, 1, 9, "Trap") ##This correctly comes out to 17.306

integrateIt(test_x,test_y, 1, 9, "Simp") ##This correctly comes out to 17.33209

##The definite integral value of y=sqrt(x) from 1 to 9 is 17.333. The Simpson's Rule
##equation is supposed to be more accurate than the Trapezoidal Rule equation and they 
##both come close to the actual definite integral value.


setMethod("plot", "Simp",
          )

#####Setting up plotting
example <- integrateIt(test_x, test_y, 1, 9, "Trap")
plot(example$x, example$y, type="l")
segments(example$x, example$y, example$x, 0)
