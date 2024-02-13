# INTRODUCTION TO FINANCIAL ENGINEERING (HW8)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 

rm(list=ls())
cat("\f")

## Ejercicio 1


# Escribimos las librerias y los datos que usamos en el HW2 añadiendo el Black Monday y hacemos su gráfica
library(Ecdat)
library(fGarch)
data(SP500 ,package="Ecdat")

returnBM=SP500$r500[1805]
x=SP500$r500[(1805-2*253+1):1805]
plot(c(x,returnBM),type="l", main="S&P500") # observamos que en el Black Monday hay una caída inusual
results=garchFit(~arma(1,0)+garch(1,1),data=x,cond.dist="std") 
dfhat=as.numeric(results@fit$par[6]) 
forecast=predict(results,n.ahead=1)


### Apartado (a)

# Hacemos ahora el histograma de returns sin inlcuir el Black Monday, por lo tanto los datos que usaremos son
x_1=SP500$r500[(1804-2*253+1):1804]
hist(x_1, breaks=100, freq=FALSE , main = "Histogram of the SP500 returns", xlab="Data", ylab="Frequency")
# Usamos freq=FALSE porque queremos el histograma de su densidad
lines(density(x_1), col="red")


### Apartado (b)

# Calculamos ahora el VaR con tiempo horizonte 1 día y con una confianza del 99%
a=quantile(x_1,1-0.99); a

# Lo podemos situar gráficamente
hist(x_1, breaks=100, freq=FALSE , main = "Histogram of the SP500 returns", xlab="Data", ylab="Frequency")
abline(v=a, col="red", lty = 2, lw = 2)



## Ejercicio 2

# Extraemos el siguiente trozo de código del HW5, haciendo uso de la función que creamos del camino simple y
# del algoritmo de Monte Carlo
payoff_function=function(S, K) {
  return(max(K - S, 0))
}

path_sample=function(N, t0, tn, S0, r, sigma){
  x0=c() # creamos un vector vacío
  x0[1]=S0 # añadimos el precio inicial del stock
  for(i in 1:N){
    x0[i+1]= x0[i]+r*x0[i]*((tn-t0)/N)+sigma*x0[i]*sqrt((tn-t0)/N)*rnorm(1) # el error es aleatorio
  }
  return(x0)
}
#esta es nuestra función montecarlo de la hw5:
monte_carlo=function(M, N, t0, tn, S0, r, sigma, payoff_function, K){
  vec=c() # creamos un vector vacío
  for(i in 1:M){
    vec[i]=payoff_function(path_sample(N, t0, tn, S0, r, sigma)[N+1], K)
  }
  price=mean(vec)*exp(-r*(tn-t0)) # calculamos el precio de la opción
  return(price)
}

# Los datos que tenemos son
N=1000  
t0=0
tn=1 # tiempo en años
S0=90 # 90 EUR
r=0.05 # interés del 5%
sigma=0.4 # volatilidad del 40%
K=75 # strike de la opción PUT
M=1000


### Apartados (a) y (b)

# Queremos calcular el VaR y su cola con un 99% de confianza, para ello tenemos que modificar nuestra función
# monte_carlo 
monte_carlo=function(M, N, t0, tn, S0, r, sigma, payoff_function, K){
  vec=c() # creamos un vector vacío
  for(i in 1:M){
    vec[i]=payoff_function(path_sample(N, t0, tn, S0, r, sigma)[N+1], K) # generamos un camino de precios 
  }
  price=exp(r*(t0-tn))*mean(na.omit(vec)) # precio promedio de las opciones simuladas
  #utilizando la formulada dada en el tema 7 en el apartado "Montecarlo simulation and option pricing"
  payoff=vec # beneficios de cada simulación
  v<-numeric(M)
  for (i in 1:M){
    v[i]<-price-payoff[i] # diferencia entre los beneficios individuales y el precio promedio
    }
  return(v)
}

# Hacemos una simulación de los precios con los datos que nos dan y creamos el histograma de simulaciones
simulation=monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function, K)
hist(simulation, breaks=20, freq=F)

# Calculamos el VaR, nos quedamos con el cuantil 0.01 y lo representamos con una línea roja en el histograma
VaR=quantile(simulation, 0.01); VaR
abline(v=VaR, col="red", lty = 2, lw = 2)

#Ahora utilizando los datos de la simulacion, nos quedamos con los valores
#que son menores que el VaR y calculamos la media de estos lo que corresponde con
#la cola del VaR. Por último, la reperesentamos en el histograma con una línea azul

cola<-c()
for (i in 1:999){
  if (is.na(simulation[i])==FALSE){
    if(as.numeric(simulation[i]) < VaR){
      print(simulation[i])
      cola[i]=as.numeric(simulation[i])
    }
    }
}

TVaR=mean(na.omit(cola)); TVaR
abline(v=TVaR, col="blue", lty=2, lw=2 )


