# INTRODUCTION TO FINANCIAL ENGINEERING (HW5)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 

install.packages("Rlab")
install.packages("nortest")


rm(list=ls())
library(Rlab)
library(nortest)



## Ejercicio 1

# Queremos construir un simulador de movimiento browniano


### Apartado (a)

# Creamos una función que tome un parámetro N y genere un vector que cuando se trace sea la ruta de ganancias
# y pérdidas de un juego de lanzar una moneda
coin_tossing_sample_path=function(N) {
  profit= 1 / sqrt(N) # añadimos la regla de que la ganancia o perdida de cada apuesta sea 1/sqrt(N)
  l=rbern(N, prob = 0.5) # creamos un vector l que contiene muestras aleatorias de una distribución Bernoulli
  # simulando el resultado de cada apuesta dónde 0 representa una pérdida y 1 una ganancia
  path=numeric(N+1)
  path[1]=0 # empezamos con una ganancia o pérdida de 0
  for (i in 1:N) {
    if (l[i] == 0) {
      path[i + 1]=path[i] + profit
    } else {
      path[i + 1]=path[i] - profit
    }
  }
  return(path)
}

# Hacemos los gráficos que nos piden para los distintos valores de N

# N=5 
p1=plot(seq(0,1,1/5), coin_tossing_sample_path(5), type="l", xlab = "Time", 
     ylab = "Profit/Loss($)", main="Coin tossing sample path for N=5")

# N=10
p2=plot(seq(0,1,1/10), coin_tossing_sample_path(10), type="l", xlab = "Time", 
     ylab = "Profit/Loss($)", main="Coin tossing sample path for N=10")

# N=50
p3=plot(seq(0,1,1/50), coin_tossing_sample_path(50), type="l", xlab = "Time", 
     ylab = "Profit/Loss($)", main="Coin tossing sample path for N=50")

# N=100
p4=plot(seq(0,1,1/100), coin_tossing_sample_path(100), type="l", xlab = "Time", 
     ylab = "Profit/Loss($)", main="Coin tossing sample path for N=100")

# N=10000
p5=plot(seq(0,1,1/10000), coin_tossing_sample_path(10000), type="l", xlab = "time", 
     ylab = "Profit/Loss($)", main="Coin tossing sample path for N=10000")


### Apartado (b)

# Creamos ahora una función que tome los valores de N y de m y llame m veces a la función anterior con el 
# parámetro N
sample_path_distribution=function(N, m) {
  final_value=c()
  for (i in 1:m) {
    final_value[i]=coin_tossing_sample_path(N)[N + 1]
  }
  return(final_value)
}

# Usemos la función para los valores dados por el enunciado, trazaremos también la gráfica de su densidad y realizaremos
# una prueba de normalidad. Para esta última usaremos la prueba Shapiro-Wilk, recordemos que la hipótesis nula es
# la muestra proviene de una población normalmente distribuida y la alternativa es que la muestra no proviene 
# de una población normal.

# (N,m)=(100,100)
s1=sample_path_distribution(100,100)
d1=density(s1)
plot(d1, type="l", main="Density plot for (N,m)=(100,100)")
# Hacemos la prueba de normalidad
shapiro.test(s1) # p-valor mayor que 0,05, no rechazamos hipótesis nula

# (N,m)=(1000,1000)
s2=sample_path_distribution(1000,1000)
d2=density(s2)
plot(d2, type="l", main="Density plot for (N,m)=(1000,1000)")
shapiro.test(s2) # p-valor mayor que 0,05, no rechazamos hipótesis nula

# (N,m)=(10000,10000)
s3=sample_path_distribution(10000,10000)
d3=density(s3)
plot(d3, type="l", main="Density plot for (N,m)=(10000,10000)")
shapiro.test(s3) # podemos ver que no podemos usar la prueba Shapiro-Wilk ya que tiene un limite de datos de 
# 5000. Realizamos entonces la prueba Anderson-Darling, ésta tiene las mismas hipótesis que la anterior
ad.test(s3) # p-valor mayor que 0,05, no rechazamos hipótesis nula



## Ejercicio 2

# Programaremos un algoritmo de Monte-Carlo para calcular los precios de las opciones


### Apartado (a)

# Creamos una función que muestree un camino de la ecuación diferencial estocástica de Black-Scholes
# usando la discretización de Euler
path_sample=function(N, t0, tn, S0, r, sigma){
  x0=c() # creamos un vector vacío
  x0[1]=S0 # añadimos el precio inicial del stock
  for(i in 1:N){
    x0[i+1]= x0[i]+r*x0[i]*((tn-t0)/N)+sigma*x0[i]*sqrt((tn-t0)/N)*rnorm(1) # el error es aleatorio
  }
  return(x0)
}

# Aplicamos la función que hemos creado a los siguientes datos y trazamos su gráfico
N=1000; t0=0; tn=1; S0=100; r=0.01; sigma =0.30
plot(seq(from=t0, to=tn, by=1/N), path_sample(N, t0, tn, S0, r, sigma),type="l", xlab="Time",
     ylab="Stock price")

#Miramos como cambia r
for(i in 1: 20){
  stock = path_sample(N, t0 , tn , S0 , 0.1*i , sigma)
  if(i==1){
    plot(seq(from = t0, tn, by= 1/N),stock,ylim=c(0,500), type="l", xlab = "Time", 
         ylab = "Stock price", main="Repercussion of interest in the price of the stock")
    
  }
  else{
    lines(seq(from = t0, tn, by= 1/N), stock,col=rgb(0,i/20,i/20))
  }
}

#Miramos como cambia sigma
for(i in 1: 10){
  stock = path_sample(N, t0 , tn , S0 , r, 0.1*i)
  if(i==1){
    plot(seq(from = t0, tn, by= 1/N),stock,ylim=c(0,250), type="l", xlab = "Time", ylab = "Stock price", 
         main="Impact of volatility on the stock price")
    
  }
  else{
    lines(seq(from = t0, tn, by= 1/N), stock,col=rgb(i/10,0,i/10))
  }
}


### Apartado (b)

# Añadimos la siguiente función que evalua una función de pago para una opción CALL con un strike dado K
payoff_function=function(S,K){
  return(max(S-K, 0))
}


### Apartado (c)

# Implementamos el algoritmo de Monte-Carlo a la función del apartado (a)
monte_carlo=function(M, N, t0, tn, S0, r, sigma, payoff_function, K){
  vec=c() # creamos un vector vacío
  for(i in 1:M){
  vec[i]=payoff_function(path_sample(N, t0, tn, S0, r, sigma)[N+1], K)
  }
  price=mean(vec)*exp(-r*(tn-t0)) # calculamos el precio de la opción
  return(price)
}


### Apartado (d)

# Modificamos la función de pago para dar el precio de una opción PUT
payoff_function=function(S, K) {
  return(max(K - S, 0))
}

# Ahora calculamos el precio de una opción PUT para los siguientes datos
N=1000  
t0=0
tn=1 # tiempo en años
S0=90 # 90 EUR
r=0.05 # interés del 5%
sigma=0.4 # volatilidad del 40%
K=75 # strike de la opción PUT
M=1000

monte_carlo(M, N, t0, tn, S0, r, sigma, payoff_function, K) 
