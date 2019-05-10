
#Definición de las funciones
f<-function(x)
{
  sin(x)
}
g<-function(x)
{
  cos(x)
}


####Segundo Paso
#Graficacion de las dos funciones
x <- seq(-pi,pi*2, by = 0.001)
plot(f, -pi, pi*2, lwd = 3, col = "red")
lines(x, g(x),lwd = 3, col = "blue")


####Tercer Paso 

#Número de particiones
particiones <- 12
#Longitud entre los puntos de las particiones
n <- (pi+(pi*2))/particiones
#Vector con los valores en x de las particiones
xi <- seq(-pi,pi*2, by = n)
#Vectores de cada funcion con sus y correspondientes a los valores en x de las particiones
y1 <- c(f(xi))
y2 <- c(g(xi))
approx.df <- data.frame(cbind(xi, y1, y2))
colnames(approx.df) <- c('X', 'Y1', 'Y2')
approx.df


####Cuarto Paso 
#Ciclo que encuentra las intersecciones entre funciones
xInterseccion <- seq(-pi,pi*2, by = 0.1)
y1Interseccion <- c(f(xInterseccion))
y2Interseccion <- c(g(xInterseccion))
nInsterseccion <- (pi+(pi*2))/0.001
interseccion <- c(0,0,0)
contador <- 1
for(i in 1:nInsterseccion)
{
  if(is.null(y1Interseccion[i] < y2Interseccion[i]))
  {
    if(is.null(y1Interseccion[i+1] > y2Interseccion[i+1]))
    {
      interseccion[contador] <- (xInterseccion[i] + xInterseccion[i+1])/2
      contador<- contador+1
    }
  }
    if(is.null(y1Interseccion[i] > y2Interseccion[i]))
    {
      if(is.null(y1Interseccion[i+1] < y2Interseccion[i+1]))
      {
        interseccion[contador] <- (xInterseccion[i] + xInterseccion[i+1])/2
        contador<- contador+1
      }
    }
}
contador <- 1



#Valores auxiliares
total <- 0
acumulado <- 0.00
acumulado2 <- 0.00
acumulado3 <- 0.00
area1 <- 0.00
area2 <- 0.00
#Ciclo que calcula los trapecios entre las funciones
for(i in 1:particiones)
{
  if(xi[i] < xInterseccion[contador] && xInterseccion[contador] < xi[i+1])
  {
    area1 <- n*((y1[i]+f(xInterseccion[contador]))/2)
    area2 <- n*((y2[i]+f(xInterseccion[contador]))/2)
    area3 <- n*((y1[i+1]+f(xInterseccion[contador]))/2)
    area4 <- n*((y2[i+1]+f(xInterseccion[contador]))/2)
    if(area1*area2 < 0)
    {
      ifelse(area1 < 0 && area1 < area2, acumulado2 <- area1 - area2, acumulado2 <- area2 - area1 )
      ifelse(area1 > 0 && area1 > area2, acumulado2 <- area1 - area2, acumulado2 <- area2 - area1 )
    }
    if(area1*area2 > 0)
    {
      ifelse(area2 < area1, acumulado2 <- area1 - area2, acumulado2 <- area2 - area1) 
    }
    if(area3*area4 < 0)
    {
      ifelse(area3 < 0 && area3 < area4, acumulado3 <- area3 - area4, acumulado3 <- area4 - area3 )
      ifelse(area3 > 0 && area3 > area4, acumulado3 <- area3 - area4, acumulado3 <- area4 - area3 )
    }
    if(area3*area4 > 0)
    {
      ifelse(area4 < area3, acumulado3 <- area3 - area4, acumulado3 <- area4 - area3) 
    }
    acumulado <- acumulado2+acumulado3
     
    
  }
  if(!(xi[i] < xInterseccion[contador] && xInterseccion[contador] < xi[i+1]))
  {
    area1 <- n*((y1[i]+y1[i+1])/2)
    area2 <- n*((y2[i]+y2[i+1])/2)
    
    if(area1*area2 < 0)
    {
      ifelse(area1 < 0 && area1 < area2, acumulado <- area1 - area2, acumulado <- area2 - area1 )
      ifelse(area1 > 0 && area1 > area2, acumulado <- area1 - area2, acumulado <- area2 - area1 )
    }
    if(area1*area2 > 0)
    {
      ifelse(area2 < area1, acumulado <- area1 - area2, acumulado <- area2 - area1) 
    }
  }
  total <- total + acumulado
  print("area actual")
  print(total)
}


####Gráfica
plot(f, -pi, pi*2, lwd = 3, col = "red")
lines(x, g(x),lwd = 3, col = "blue")
vectorx <- c(-pi, pi*2)
vectory <- c(0,0)
lines(vectorx,vectory)
for(i in 1:particiones)
{
  ax <- c(xi[i],xi[i])
  ay1 <- c(0,y1[i])
  ay2 <- c(0,y2[i])
  bx <- c(xi[i+1],xi[i+1])
  by1 <- c(0,y1[i+1])
  by2 <- c(0,y2[i+1])
  hx <- c(xi[i],xi[i+1])
  hy1 <- c(y1[i],y1[i+1])
  hy2 <- c(y2[i],y2[i+1])
  lines(ax,ay1,lwd = 2, col="red")
  lines(bx,by1, col="red")
  lines(hx,hy1, col="red")
  lines(ax,ay2, col="blue")
  lines(bx,by2, col="blue")
  lines(hx,hy2, col="blue")
}
print("Acumulado")
print(total)
