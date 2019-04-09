
f<-function(x)
{
  return (exp(x)) 
}
x = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
y = c(f(0),f(0.1),f(0.2),f(0.3),f(0.4),f(0.5),f(0.6),f(0.7),f(0.8),f(0.9),f(1))
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")

lagrange.poly <- function(x, y)
{
  require(rSymPy)
  
  if (length(x) != length(y)) {
    stop('x and y must be of equal length')
  }
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  # Define x variable for rSymPy to simplify
  x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}
DatosX = x[1:11]; DatosY = y[1:11]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Funcion")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Funcion")
curve(Ajuste_Polinomio,add=T,from =0,to =1)
aux =lagrange.poly(x,y)
f<-as.function(alist(a = , eval (parse((text=aux)))))
curve(Ajuste_Polinomio,add=T,from =0,to =1)