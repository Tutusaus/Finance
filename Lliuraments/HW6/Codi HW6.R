# INTRODUCTION TO FINANCIAL ENGINEERING (HW6)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 

rm(list=ls())

# Librerías necesarias
library(quantmod) 



## Ejercicio 1

# Queremos hacer los gráficos de un conjunto de carteras o portfolios

### Apartado (a)

options("getSymbols.warning4.0"=FALSE) 
cat("\f")

# Descargamos los datos
getSymbols('AAPL',scr='yahoo',from="2015-01-01",to="2015-12-31")
getSymbols('GOOG',scr='yahoo',from="2015-01-01",to="2015-12-31")
getSymbols('MSFT',scr='yahoo',from="2015-01-01",to="2015-12-31")
getSymbols('META',scr='yahoo',from="2015-01-01",to="2015-12-31")

# Convertimos los datos en formato numérico
goog = as.numeric(GOOG$GOOG.Adjusted)
meta = as.numeric(META$META.Adjusted)
aple = as.numeric(AAPL$AAPL.Adjusted)
msft = as.numeric(MSFT$MSFT.Adjusted)


### Apartado (b)

# Creamos una función para calcular los rendimientos netos de los precios de las acciones
get_net = function(data) {
  nets=c()
  for (i in 2:length(data)) {
    nets[i - 1]=(data[i] / data[i - 1]) - 1
  }
  return(nets)
}

# Aplicamos la función creada
google_net=get_net(goog)
meta_net=get_net(meta)
aple_net=get_net(aple)
msft_net=get_net(msft)

# Ahora calculamos las esperanzas
google_mean=mean(google_net)
meta_mean=mean(meta_net)
aple_mean=mean(aple_net)
msft_mean=mean(msft_net)

# Y definimos un vector con ellas
mu=c(google_mean,meta_mean,aple_mean,msft_mean)
mu

# Finalmente hacemos la matriz de covariancias
data=cbind(google_net,meta_net,aple_net,msft_net)
C=cov(data)
C


### Apartado (c)

# Generamos números aleatorios uniformamente distribuidos, para ello vamos a establecer semillas 
# diferentes para garantizar que las secuencias de números aleatorios sean diferentes entre sí
set.seed(123)
r1=runif(1)
set.seed(124)
r2=runif(1)
set.seed(125)
r3=runif(1)
set.seed(126)
r4=runif(1)

# Normalizamos los pesos de cartera o portfolio weight
w1=r1/(r1+r2+r3+r4)
w2=r2/(r1+r2+r3+r4)
w3=r3/(r1+r2+r3+r4)
w4=r4/(r1+r2+r3+r4)
w=c(w1,w2,w3,w4)

# Calculamos la media y la varianza de la cartera asociada a los pesos w_i
mu_p=t(w)%*%mu
std2=t(w)%*%C%*%w


### Apartado (d)

# Hacemos el mismo cálculo que en el apartado anterior para 1000 vectores aleatorios w
n=1000
mu_p=c()
std2=c()
for (i in 1:n) {
  r1=runif(1, 0, 1)
  r2=runif(1, 0, 1)
  r3=runif(1, 0, 1)
  r4=runif(1, 0, 1)
  w1=r1/(r1+r2+r3+r4)
  w2=r2/(r1+r2+r3+r4)
  w3=r3/(r1+r2+r3+r4)
  w4=r4/(r1+r2+r3+r4)
  
  w=c(w1, w2, w3, w4)
  
  mu_p[i]=t(w)%*%mu
  std2[i]=t(w)%*%C%*%w
}

# Trazamos su gráfico
plot(std2, mu_p)



## Ejercicio 2

### Apartado (a)

# Construimos el siguiente vector de longitud N=500
N=500
rbase=seq(min(mu), max(mu), length = N)


### Apartado (b)

# Nuestro objetivo ahora es trazar la frontera eficiente usando los multiplicadores lagrangianos

# Definimos 2 matrices, Q y b
u=c(1, 1, 1, 1)
Q=cbind(2*C, mu, u) # Unimos columnas
mut=c(t(mu), 0, 0) # Creamos el vector transpuesto de mu
ut=c(t(u), 0, 0) # Creamos el vector transpuesto de u
Q=rbind(Q, mut) # Unimos la fila de mut a la matriz Q
Q=rbind(Q, ut) # Unimos la fila d ut a la matriz Q
Q=round(Q, 6) # Redondeamos los números para que no tengan más de 6 dígitos


b=matrix(c(rep(0, 4*N), rbase, rep(1, N)), nrow=6, byrow=TRUE) # creamos una matriz concatenando valores de
# 0, rbase y 1 tal y como nos pide el enunciado
b=round(b, 6) # Redondeamos a 6 dígitos


### Apartado (c)

# Utilizando la función solve y las matrices que acabamos de definir para resolver el modelo lineal
y=solve(Q, b[, 1])
y=t(y) # transponemos en vector y
y1=matrix(0, N, nrow = 6) # creamos una matriz Nx6 inicializada con 0
# Creamos dos vectores vacíos para almacenar resultados
mu_p1=c()
std2_1=c()
for (i in 1:N) {
  y1[, i]=solve(Q, b[, i]) # resolvemos el sistema lineal
  mu_p1[i]=t(y1[1:4, i])%*%mu
  std2_1[i]=t(y1[1:4, i])%*%C%*%y1[1:4, i]
}


### Apartado (d)

# Dibujamos gráficamente la frontera  
plot(std2_1, mu_p1, type="l") 

# Y la frontera eficiente, para ello empezamos buscando el índice del mínimo valor de la volatilidad 
# en la frontera eficiente
which(std2_1%in%min(std2_1))
m=mu_p1[which(std2_1%in%min(std2_1))]
mu_p2=mu_p1[mu_p1>=m] # creamos un vector que contiene los valores mayores o iguales a m
std2_2=std2_1[which(mu_p1%in%mu_p2)]
lines(std2_2, mu_p2, col="lawngreen", lwd=4)
# Escribimos la leyenda
legend("bottomright", legend = c("Efficient frontier","Frontier"), 
       col = c("lawngreen", "black"),lty = c(1,1), cex=0.8)



## Ejercicio 3

# Unimos los gráficos del ejercicio 1 y del 2
plot(std2_1,mu_p1, type="l") # frontera
points(std2,mu_p, col="blue") # conjunto factible
lines(std2_2, mu_p2, col="lawngreen", lwd=4) # frontera eficiente
points(min(std2_2), m, col="red", pch=16) # cartera de variación mínima
# Escribimos la leyenda
legend("bottomright", legend = c("Efficient frontier","Frontier", "Feasible set", "Minimum variance portfolio"), 
       col = c("lawngreen", "black", "blue", "red"),lty = c(1,1,0,0),pch = c("-", "-","○","●"), cex=0.6)


