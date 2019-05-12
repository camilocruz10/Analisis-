a=-6
b=-2
c=32
w=-3
#punto 1
#install.packages("deSolve")
require(deSolve)
rungekutta = function(f,t0,y0,h,n){
  t = seq(t0, t0+n*h, by=h)
  y = rep(NA, times=(n+1))
  # length(t)==length(y)
  y[1] = y0
  for(k in 2:(n+1)){
    k1=h/2*f(t[k-1],y[k-1])
    k2=h/2*f(t[k-1]+h/2, y[k-1]+k1)
    k3=h/2*f(t[k-1]+h/2, y[k-1]+k2)
    k4=h/2*f(t[k-1]+h, y[k-1]+2*k3)
    y[k] = y[k-1]+1/3*(k1+2*k2+2*k3+k4)
  }
  dat = cbind(t,y)
  print(as.matrix(dat))
  plot(t,y,pch=20, col="red")
}
fp = function(x,z, parms){
    dx= z
    dz=z+6*x
    return((c(dx, dz)))
}
rungekutta(fp,2,-1,0.1,20)
rungekutta(fp,2,-1,0.2,20)
#punto 2
fp3 = function(x,y, parms){
  dx=3*x+2*y
  dy=5*x-4*y
  return((c(dx, dy)))
}
euler1 = function(f, t0, y0, h, n) {
  #Datos igualmente espaciados iniciando en x0 = a, paso h. "n" datos
  t = seq(t0, t0 + (n-1)*h, by = h) # n datos
  y = rep(NA, times=n) # n datos
  y[1]=y0
  for(i in 2:n ) y[i]= y[i-1]+h*f(t[i-1], y[i-1])
  print(cbind(t,y)) # print
  plot(t,y, pch=19, col="red") # gráfica
}

euler1(fp3, 3, 6, 0.2, 10)
#punto 3
fp7= function(x,datos,parms){
  y = datos[1]
  z = datos[2] 
  dy = z
  dz =z+x-y-1
  return (list(c(dy,dz)))
}
tis = seq(0,2,by = 0.1)
sol = ode(c(1,2),tis,fp7,parms = NULL, method = "rk4")
tabla = cbind(tis, sol[,2])
tabla
#punto 4
fp5 = function(t,z){  
  cos(2*t)-z*(t-2*pi)*cos(2*(t-2*pi))
}
euler1 = function(f, t0, y0, h, n) {
  #Datos igualmente espaciados iniciando en x0 = a, paso h. "n" datos
  t = seq(t0, t0 + (n-1)*h, by = h) # n datos
  y = rep(NA, times=n) # n datos
  y[1]=y0
  for(i in 2:n ) y[i]= y[i-1]+h*f(t[i-1], y[i-1])
  print(cbind(t,y)) # print
  plot(t,y, pch=19, col="red") # gráfica
}
#install.packages(Deriv)
require(Deriv) # derivadas parciales
#--- Metodo de Taylor, orden 4
mtaylor4 = function(f, t0, y0, h, n){
  #Datos igualmente espaciados iniciando en t0 = a, paso h. "n" datos 
  t = seq(t0, t0 + (n-1)*h, by = h) # n datos
  y = rep(NA, times=n) # n datos
  y[1] = y0
  # Derivadas parciales con el paquete Deriv. Deriv(f)
  ft=Deriv(f,"t"); fy=Deriv(f,"y")
  f1 = function(t,y)
    ft(t,y)+fy(t,y)*f(t,y)
  f1t=Deriv(f1,"t");   f1y=Deriv(f1,"y")
  f2= function(t,y) f1t(t,y)+f1y(t,y)*f(t,y)
  f2t=Deriv(f2,"t");    f2y=Deriv(f2,"y")
  f3= function(t,y) f2t(t,y)+f2y(t,y)*f(t,y) # orden m = 4
  for(i in 2:n ){ 
    f0i = f(t[i-1], y[i-1])
    f1i = f1(t[i-1], y[i-1])
    f2i = f2(t[i-1], y[i-1])
    f3i = f2(t[i-1], y[i-1])
    y[i] = y[i-1] + h*(f0i + h/2*f1i + h^2/6*f2i + h^3/24*f3i )
  }
  print(cbind(t,y))                   #imprimir
  plot(t,y, pch=19, col="red",cex = 2) #gráfica
}
mtaylor4(fp5,4,0,0.2,32)
euler1(fp5,4,0,0.2,32)
#punto 5
fp8= function(t,datos,parms){
  x = datos[1]
  y = datos[2] 
  dx = x-3*y
  dy = 3*x+7*y
  return (list(c(dx,dy)))
}
tis = seq(0,2,by = 0.1)
sol = ode(c(2,-1),tis,fp,parms = NULL, method = "rk4")
tabla2 = cbind(tis, sol[,2])
tabla2
fp9 <- function(x){
  fx = -exp(-2*x)-exp(3*x)
  return(fx)
}
plot(tis, sol[,2], main="Metodo de Runge Kutta Grado 4")
b = poly.calc(tis,sol[,2])
curve(fp9, xlim=c(0,2),add=T)
curve(b,add=T, col="greem")
abline(h=0,v=0,col="blue")
print(b)
cat("x"," f(x)","error")
for(i in 1:length(sol[,2])){
  err = abs((sol[,2][i]-fp9(tis[i]))/(fp9(tis[i])))
  cat(tis[i],fp9(tis[i]),err*100,"\n")
}
#punto 6
#install.packages("pracma")
#install.packages("invLT")
#install.packages("pbdDMAT")
library(pracma)
#c1 y c2 son las constantes luego de hallar
# la solucion, w es la fuerza aplicada, I es el
# moento de inercia y E es el modulo de young
c1
c2
w
E=12
I=123
F
Fuerzaw<-function(s) (1/(24*s**5)-1/(s**5))
fs<- function(s)(1/(s**3)+1/(s4) +1/(24*s**5)-1/(s**5)   )
#fs<- function(s)( c1/(s**3)+c2/(s**4) +Fuerzaw )
L1<-invlap(fs, 0.01, 5, 4, 6, 40, 20)
L.t(p)
L1[[1]]
L1[[2]]
y<- function (x) ((c1/2)*x**2+(c2/6)*x**3+(w/(E*I))*F)
fyy<-function (x) ( c1+c2*x +  D*F )
#fy=D(y,"x")
#fyy=D(fy,"x")
a1=rbind(c(0,0,1/2,1/6),c(1,1,0.5,0))
b1=rbind(c(0,0))
qr.solve(a1,b1)
#punto 7
library(deSolve)
yi = c(X = 1, Y = 1, Z = 1)
Lorenz = function (t, y, parms) {
  with(as.list(y), {
    #a b y c declarados al principio del notebook
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}
times = seq(from = 0, to = 25, by = 0.1) 
out = ode(y = yi, times = times, func = Lorenz,parms = NULL)
# Gráfica
plot(out, lwd = 2) 
plot(out[,"X"], out[,"Y"], type = "l", xlab = "X",ylab = "Y", main = "Mariposa")
#las funciones de lorenz taylor y euler fueron sacadas del libreo de analiis numeric
#punto 8
fp4 = function(x,y, parms){
  dx=1*x-3*y
  dy=3*x+7*y
  return((c(dx, dy)))
}
rungekutta(fp4,0,0,0.1,20)