newtonInterpolacion = function(x, y, a) {
  n = length(x) 
  A = matrix(rep(0, times = n^2), nrow = n, ncol = n) 
  A[,1] = y 
  for (k in 2:n) { 
    A[k:n, k] = (A[k:n, k-1] - A[(k-1):(n-1), k-1] ) / (x[k:n] - x[1:(n-k+1)]) 
  } 
  smds = rep(NA, length = n) smds[1] = 1
  for (k in 2:n) { 
    smds[k] = (a - x[k-1])*smds[k-1]
  } 
  return(A) 
}
#funcion del librp
A1=newtonInterpolacion(hora[1:4], tim[1:4],1)
hora<-c(6,8,10,12,14,16,18,20)
tim<-c(8.5,9.2,12.7,18.4,21.6,17.9,11,9)
B=c(tim[1],tim[2],tim[3],tim[4])
C=  solve(A1,B)
#polinomio de grado 3
curve((A1[1,1]+A1[2,2]*(x-hora[1])+A1[3,3]*(x-hora[1])*(x-hora[2])+A1[4,4]*(x-hora[1])*(x-hora[2])*(x-hora[3])), from = -10, to=40,ylab = "y",col="blue")
points(hora[1],tim[1], col = "red")
points(hora[2],tim[2], col = "red")
points(hora[3],tim[3], col = "red")
points(hora[4],tim[4], col = "red")
points(hora[5],tim[5], col = "red")
points(hora[6],tim[6], col = "red")
points(hora[7],tim[7], col = "red")
points(hora[8],tim[8], col = "red")
#polinomio de grado 5
A1=newtonInterpolacion(hora[1:6], tim[1:6],1)
curve( A1[1,1]+A1[2,2]*(x-hora[1])+A1[3,3]*(x-hora[1])*(x-hora[2])+A1[4,4]*(x-hora[1])*(x-hora[2])*(x-hora[3]) + A1[5,5]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])+A1[6,6]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])*(x-hora[5]), from = -10, to=40,ylab = "y", add = TRUE,col="red" )
#polinomio de grado 7
A1=newtonInterpolacion(hora[1:8], tim[1:8],1)
curve( A1[1,1]+A1[2,2]*(x-hora[1])+A1[3,3]*(x-hora[1])*(x-hora[2])+A1[4,4]*(x-hora[1])*(x-hora[2])*(x-hora[3]) + A1[5,5]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])+A1[6,6]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])*(x-hora[5])+A1[7,7]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])*(x-hora[5])*(x-hora[6])+A1[8,8]*(x-hora[1])*(x-hora[2])*(x-hora[3])*(x-hora[4])*(x-hora[5])*(x-hora[6])*(x-hora[7]), from = -10, to=40,ylab = "y", add = TRUE,col="green" )

