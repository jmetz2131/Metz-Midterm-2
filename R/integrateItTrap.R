##Setting up the function
integrateIt <- function(x, y, a, b, n, Rule){ 
  x <- seq(a, b, by=n)
  penultimate <- length(y)-1
  h <- (b-a)/n
  if(Rule=="Trap"){
    t_answer <- (h/2)*(y[1]+tail(y, n=1)+2*(sum(y[2:penultimate])))
  }
  return(t_answer)
}

x<-seq(1, 5, by=1)
y<-c(2,4,6,8,10)

integrateIt(x, y, 1, 5, 4,"Trap")
