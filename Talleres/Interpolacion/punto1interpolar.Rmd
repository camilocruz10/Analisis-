newtonInterpolacion = function(x, y, a) { 
n = length(x) 
A = matrix(rep(NA, times = n^2), nrow = n, ncol = n) 
A[,1] = y 
for (k in 2:n) { 
A[k:n, k] = (A[k:n, k-1] - A[(k-1):(n-1), k-1] ) / (x[k:n] - x[1:(n-k+1)]) 
} 
smds = rep(NA, length = n) 
smds[1] = 1
for (k in 2:n) { 
smds[k] = (a - x[k-1])*smds[k-1] 
}
return(sum(diag(A)*smds) )
}
#funcion de librp
hora<-c(6,8,10,12,14,16,18,20)
tim<-c(8.5,9.2,12.7,18.4,21.6,17.9,11,9)
#install.packages("PolynomF")

#require(PolynomF)


polyAjuste = poly.calc(hora,tim)

polyAjuste

plot(hora,tim, pch=19, cex=1, col = "blue", asp=1)

curve(polyAjuste,add=T)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 7)

resu<-c(remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 9)

resu<-c(resu,remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 11)

resu<-c(resu,remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 13)

resu<-c(resu,remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 15)

resu<-c(resu,remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 17)

resu<-c(resu,remp)

remp<-newtonInterpolacion(hora[0:8], tim[0:8], 19)

resu<-c(resu,remp)

pts<-c(7,9,11,13,15,17,19)
points(pts,resu)