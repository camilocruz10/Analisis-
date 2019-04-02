require(pracma)

f = function(x) exp(x)
g= function (x) 1/x
p = taylor(f, 0, 4) # Polinomio de Taylor de orden 4, alrededor dex0=0.
pa= taylor(g, 5, 4) # esta funcion no se puede aproximar en x0=0 debido a que al evaluar este punto la funcion es indeterminada
pa1=taylor(g,-5,4)
curve(f, col= "red", from = -10, to= 10)
curve(p[1]*x^(4)+p[2]*x^(3)+p[3]*x^(2)+p[4]*x+p[5],add=TRUE, col="blue", from = -100, to= 100)
curve(g, col= "green", from = -10, to= 10)
curve(pa[1]*x^(4)+pa[2]*x^(3)+pa[3]*x^(2)+pa[4]*x+pa[5],add=TRUE, col="red", from = -100, to= 100 )
curve(pa1[1]*x^(4)+pa1[2]*x^(3)+pa1[3]*x^(2)+pa1[4]*x+pa1[5],add=TRUE, col="red", from = -100, to= 100 )

cat(p[1],"x^(4)+",p[2],"*x^(3)+",p[3],"*x^(2)+",p[4],"x+",p[5],"/n")
#polinomio para la parte positiva
cat(pa[1],"x^(4)+",pa[2],"*x^(3)+",pa[3],"*x^(2)+",pa[4],"x+",pa[5],"/n")
#polinomio para la parte negativa
cat(pa1[1],"x^(4)+",pa1[2],"*x^(3)+",pa1[3],"*x^(2)+",pa1[4],"x+",pa1[5],"/n")

#c. debido a que el polinomio es de grado 4 da una aproximacion acertada hasta cierto 
#punto, ademas hay que tener en cuenta el punto en el que va a estar centrada el polinomio 