library(polynom)
library(PolynomF)
#install.packages("polynom")
require(graphics)
library(graphicsQC)
x=c(1,2,6,9,14,17.6,20,23,24.5,26.85,28.7,29,25.51,11.15,9.32,8.37,9.03,7.76,1) 
y=c(3,3.7,4.5,7.12,6.7,4.45,7,6.5,5.6,5.87,5.05,3.71,0.47,1.65,1.22,1.7,2.92,2.36,3)
                                 
xx=c(1,2,6,13,17.6,20,23.5,24.5,26.5,27.5,28,29,24.10,10.10,9.20,8.37,9.03,6.98,1)  
yx=c(3,3.7,7.12,6.7,4.45,7,6.1,5.8,5.6,5.87,5.15,5,4.9,2.5,2,0.9,2.54,2.80,3)   
#xx=c(1,2,6,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29)
length(xx)
length(yx)
plot(x,y, pch=19, cex=0.9, col = "blue", asp=1,xlab="X", ylab="Y", main="Perro ")
n=19
pint<-function(x,y){
  t = 1:length(x)
  sx = spline(t,x)
  sy = spline(t,y)
  lines(sx[[2]],sy[[2]],type='l', col="red")
}
pint(x,y)
#puntos origales / dados / puntos inicilaes geogebra
    

#valores no seleccionados
inter = splinefun(x,y,method = "natural")
i=0
cat(x[c(i)],"      ",xx[c(i)])
for(i in 2:n-1){
if(x[c(i)]!=xx[c(i)]){
  valorinter = inter(xx[i])
  cat(xx[i],",",yx[i],",",valorinter,"\n")
}
}
errores=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
cont=0
#error relativo valores interpolados
cat("x   ","y    ","s(x)   ","   error relativo")  
inter = splinefun(x,y,method = "natural")
acumerrorrela=0
mayor=0
for(i in 2:n-1){
  valorinter = inter(xx[i])
  errabs=abs(yx[i]-valorinter)
  error = errabs/yx[i] * 100
  acumerrorrela=acumerrorrela+error
  cat(xx[i],",",yx[i],",",valorinter,",",error,"\n")
}
cat("error total:   ", acumerrorrela)
for(i in 2:n-1){
  if(errores[c(i)]>mayor){
    mayor=errores[c(i)]
  }
}
#grafica cota
plot(seq(1,19),errores,pch=20,main = "Gráfica de errores",xlab="Puntos",ylab="Error porcental (%)")
lines(seq(1,19),errores)
abline(mayor, 0 ,col="blue")
