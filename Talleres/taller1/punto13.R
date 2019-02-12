#Pontifica Universidad Javeriana
#Analisis numerico
#camilo cruz

n=as.double(readline("Ingrese un numero, cuya raiz desea encontrar:"))
x=as.double(readline("Ingrese un valor inicial:"))

f<-function(n,E,x)
{
  y<-0.5*(x+(n/x))
  repeat{
    x=y
    y=0.5*(x+(n/x))
    aux=abs(x-y)
    if(aux>E)
      break
  }
 return(y) 
}

cat(f(n,0.0000001,6))