# INTRODUCTION TO FINANCIAL ENGINEERING (HW1)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 


# Paquetes a instalar para que el código funcione
#install.packages("Ecdat")
#install.packages("tseries")
#install.packages("forecast")


# Iniciamos el programa con los paquetes y librerías que utilizaremos
data(Tbrate ,package="Ecdat")
library(tseries)
library(Ecdat)
library(forecast)
library(lmtest)
library(xts)
library(ggplot2)

# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate


## Ejercicio 1

### Apartados (a) y (b)

# Hacemos los gràficos de la sèrie temporal

plot (Tbrate)
# Vemos que r presenta una tencencia positiva hasta 1980, y presenta tendencia positiva siempre y pi tiene una 
# volatilidad muy alta.

# En el siguiente gràfico consideramos solo su diagonal, la cuál corresponde al AFC de r, de y y de pi
acf(Tbrate)
# El gráfico ACF de r, y pi muestra una lenta caída a cero, lo que es un signo de una memoria de dependencia prolongada 
# o de un proceso no estacionario.

# Usamos ahora la prueba Dickey-Fuller en la serie temporal para determinar si es estacionaria o no. La Prueba de 
# Dickey-Fuller busca determinar la existencia o no de raíces unitarias en una serie temporal. La hipótesis nula de 
# esta prueba es que existe una raíz unitaria en la serie. Los resultados incluyen estadísticas de la prueba, el valor p. 
# Sabemos que si este valor es menor que un nivel de significancia dado (0,05), podemos rechazar la hipótesis nula. 
# El Lag order es el número de retardos utilizados en la regresión del ADF.
adf.test(Tbrate[,1])
adf.test(Tbrate[,2]) 
adf.test(Tbrate[,3])
# No podemos rechazar la hipótesis nula porque el valor p no es menor que 0,05 en ninguno de los casos.
# Por lo tanto, aceptamos la hipótesis nula, es decir, existe una raíz unitaria en la serie y por lo tanto presenta 
# un modelo no estacionario.


### Apartados (c) y (d)

# Diferenciamos la serie temporal y volvemos a usar la prueba Dickey-Fuller
diff_rate=diff(Tbrate)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3]) 
# Ahora, a partir de la prueba, como el p-valor es menor que 0,05 rechazamos la hipotesis nula 
# (nos dice que la serie temporal tiene una raíz unitaria y por lo tanto no es estacionaria) y nos quedamos con la 
# hipótesis alternativa concluyendo que la serie es estacionaria para todas las variables.

# Veamos los gráficos de la serie diferenciada
plot(diff_rate) 
acf(diff_rate)
# Vemos que ens los plots de las series, los gráficos no presentan tendencias.

# En el ACF de r, se ven picos positivos y negativos en el ACF plot a retardos específicos, 
# esto indica que la serie de tiempo tiene autocorrelaciones positivas en algunos 
# retardos y autocorrelaciones negativas en otros retardos. En otras palabras, 
# la serie de tiempo muestra patrones de correlación tanto positiva como negativa en diferentes intervalos de tiempo.

#En el ACF de y, se ven picos positivos en el ACF plot a retardos específicos, 
#esto indica autocorrelación positiva en esos retardos

#En el ACF de pi, se ven picos negativos en el ACF plot a retardos específicos, 
#esto indica autocorrelación negativa en esos retardos.


### Apartados (e), (f), (g) y (h)

auto.arima(Tbrate[,1], max.P=0, max.Q=0, ic="aic")
# Observamos que obtenemos ARIMA(0,1,1), donde los AFC también nos dicen lo mismo. 

# Observamos también que el AIC eligió el modelo ma(1).

# Los dos criterios de bondad más comunes para esta función son el AIC y el BIC,  Cuanto menor sea el valor de AIC, 
# mejor se considera el modelo. El AIC penaliza menos la complejidad del modelo en comparación con el BIC, 
# lo que significa que puede seleccionar modelos más complejos si proporcionan un mejor ajuste a los datos.
# De esta manera, el criterio de bondad de ajuste que usamos aquí será e AIC más pequeño.

# Cambiamos ahora el criterio de bondad a bic
auto.arima( Tbrate[,1], max.P=0 , max.Q=0 , ic="bic") 
# Como podemos ver, no cambia el modelo que más se ajusta ya que sigue eligiendo el modelo ma(1).


### Apartado (i)

# Añadimos el orden de diferenciación que hemos obtenido en el apartado (e)
fit1=arima(Tbrate[ ,1],order=c(0,1,1))
acf(residuals(fit1))
checkresiduals(residuals(fit1))

Box.test(residuals(fit1),lag=10,type="Ljung")
# Podemos observar que los datos se distribuyen de forma independiente. 


## Ejercicio 2

# Tenemos el siguiente modelo AR
# Y_t=5-0.55*Y_{t-1}+epsilon_t
# y asumimos 
sigma_epsilon2=1.2

### Apartado (a)

# Vemos que el proceso sí que es estacionario ya que es un modelo AR el cual sabemos que es estacionariamente débil si
# el valor absoluto de phi es menor que 1. En nuestro caso 
phi=-0.55
# lo cual estamos bajo las hipótesis.


### Apartado (b)

# Sabemos que el proceso AR se puede escribir como Y_t-mu=phi(Y_{t-1}-mu)+epsilon_t, así
mu=5/(1-phi); mu


### Apartado (c)

# Buscamos la varianza del proceso, sabemos también que, sea sigmaY2 la varianza de Y_t
# sigmaY2=phi^2+sigmaY2+sigma_epsilon2
# así tenemos
sigmaY2=sigma_epsilon2/(1-phi^2); sigmaY2


### Apartado (d)

# La función de covarianza de este proceso es
covariance_function=function(h){
  return(sigma_epsilon2*phi^abs(h)/(1-phi^2))
}


## Ejercicio 3

# Los estimadores que nos dan en este ejercicio son
mu_est=104
phi1_est=0.4
phi2_est=0.25
phi3_est=0.1

# Las cuatro últimas observaciones son
Yn_3=105
Yn_2=102
Yn_1=103
Yn=99

# Hacemos el pronóstico para Y_{n+1} y Y_{n+2}, usamos que Y_{n+k}=mu_est+phi_est^k(Y_n-mu_est)
Y_n1=mu_est+phi1_est*(Yn-mu_est)+phi2_est*(Yn_1-mu_est)+phi3_est*(Yn_2-mu_est); Y_n1
Y_n2=mu_est+phi1_est*(Y_n1-mu_est)+phi2_est*(Yn-mu_est)+phi3_est*(Yn_1-mu_est); Y_n2


## Ejercicio 4

# Usamos el siguiente código
data(Mishkin) 
tb1=log(Mishkin[,3])


### Apartado (a)

# Hacemos un grafico
plot(tb1, main="Mishkin plot")
acf(tb1, main="Mishkin ACF plot")
ggtsdisplay(tb1)

# Diferenciamos la serie temporal y hacemos gráficos de nuevo para saber cuál es su tendencia
tbdiff1=diff(tb1)
plot(tbdiff1)
acf(tbdiff1, main="")

# Por los gráficos obtenidos de la série transformada parece que nuestra série temporal es débilmente estacionaria, hacemos
# una prueba Dickey-Fuller para reafirmar nuestras conclusiones
adf.test(tbdiff1)
# como el p-valor es 0,01<0,05 entonces rechazamos la hipótesis nula (no es estacionaria). De esta manera reafirmamos
# nuestras conclusiones y vemos que necesitamos transformar la série una vez para que ésta sea estacionaria.


### Apartado (b)

# Usamos la función auto.arima para determinar los modelos ARIMA no estacionales que mejor se ajustan
(aic_model=auto.arima(tb1, max.P=0, max.Q=0, ic="aic"))
(bic_model=auto.arima(tb1, max.P=0, max.Q=0, ic="bic"))
# En el AIC observamos que obtenemos un modelo ARIMA(3,1,5) y en el BIC un modelo ARIMA(0,1,1)

# Hagamos un test para ver si los coeficientes son estadisticamente significativos:

coeftest(aic_model)
# Observamos que en el aic_model nos encontramos que los dos últimos coeficientes y el drift no son significativos, 
# por lo tanto podemos considerar que son 0, de esta forma consideramos el siguiente modelo:
(aic_model2=arima(tb1, c(3,1,3)))
# Obtenemos un modelo con dos variables menos y un AIC más negativo, por lo tanto, es mejor que el ARIMA(3,1,5). 

coeftest(bic_model)
# En este caso el bic_model tiene coeficientes significativos. 

### Apartado (c)

# Examinamos los residuos de los dos modelos que hemos elegido
checkresiduals(aic_model2, lag=100)
checkresiduals(bic_model, lag=100)

# Una vez hecha la prueba en ambos modelos vemos que en el aic_model2 no podemos rechazar la hipótesis nula ya que el
# p-valor es mayor que 0,05, por lo tanto podemos decir que los datos se distribuyen de forma independiente (es decir, 
# las correlaciones en la población de la que se toma la muestra son 0, de modo que cualquier correlación observada en 
# los datos es el resultado de la aleatoriedad del proceso de muestreo).
# En cambio, en el bic_model vemos que los datos no se distribuyen de forma independiente ya que rechazamos la hipótesis
# nula debido a que el p-valor=5,075e-08<0,05. 
