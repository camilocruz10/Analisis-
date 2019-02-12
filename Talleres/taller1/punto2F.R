a =as.double(readline(prompt = "Ingrese un numero "))
while(a>0){
  d=a%%2
  a=a%/%2
  cat(d,"\n")
}
