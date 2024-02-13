# INTRODUCTION TO FINANCIAL ENGINEERING (HW2)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 

# Paquetes a instalar para que el código funcione

#install.packages("Ecdat")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("fGarch")
#install.packages("ggplot2")
#install.packages("xts")
#install.packages("zoo")
#install.packages("rugarch")
#install.packages("fBasics")

rm(list=ls())

## Ejercicio 1

# Empezamos el ejercicio escribiendo los paquetes y librerías que usaremos
data(Tbrate ,package="Ecdat")
library(tseries) 
library(fGarch)
library(xts)
library(ggplot2)
library(forecast)

# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate Tbill=Tbrate[,1]


### Apartado (a)

# Tenemos la siguiente serie temporal con su transformada, queremos estudiar si estas son o no estacionarias
Tbill=Tbrate[,1]
Del.Tbill=diff(Tbill)

# Empezamos haciendo los gráficos con el comando plot
plot(Tbill) 
plot(Del.Tbill)

# Miramos ahora los gráficos obtenidos con la función ACF
acf(Tbill)
acf(Del.Tbill)

# Aquí podemos ver una comparación del gráico, del gráfico ACF y del gráfico PACF
ggtsdisplay(Tbill)
ggtsdisplay(Del.Tbill)

# Realizamos la prueba Dickey-Fuller para ambas series
adf.test(Tbill) # el p-valor es 0,6075 y por lo tanto no es estacionaria
adf.test(Del.Tbill) # el p-valor es menor que 0,01 lo que significa que  es una serie estacionaria

# Ahora realizamos la prueba Kwiatkowski-Phillips-Schmidt-Shin (KPSS)
kpss.test(Tbill) # el p-valor es menor que 0,05 y por lo tanto es no estacionaria
kpss.test(Del.Tbill) # el p-valor es menor que 0,05 así que la serie temporal no es estacionaria

# Comprobamos la heteroscedasticidad
checkresiduals(Del.Tbill)


### Apartado (b)

# Ajustamos la serie temporal a un modelo ARMA(1,0)/GARCH(1,0)
garch.model=garchFit(formula=~arma(1,0)+garch(1,0),Del.Tbill) 
summary(garch.model)

# Obtenemos la estimación de los parámetros de los modelos y sus p-valores que nos dicen si éstos son
# significativos o no
garch.model@fit$matcoef # vemos que todos los parámetros son significativos ya que el p-valor es menor
# que 0,05 en todos ellos


### Apartados (c), (d) y (e)

# Estudiamos ahora los residuos y los residuos estandarizados de la serie temporal diferenciada
res=residuals(garch.model) 
res_std=res/garch.model@sigma.t 

# Hacemos el gráfico de los residuos
plot(res, type="l")
abline(h=0, col="red",lwd=1) # vemos una reversión a la media con cambios de volatilidad leves

# Hacemos el gráfico ACF de los residuos y los residuos al cuadrado
acf(res) # ningún lag sobrepasa el nivel de significancia, no es posible ajustar los residuos con el modelo ARMA 
acf(res^2) # tres lags sobrepasan el nivel de significancia por lo tanto hay autocorrelación nula

# Repetimos el mismo procedimiento para los residuos estandarizados
acf(res_std)
acf(res_std ^2)
# Todos los valores se encuentran dentro del nivel de significancia cero, así pues los residuos
# estandarizados no estan correlacionados


### Apartado (f)

# Estudiamos el comportamiento del gráfico de los residuos estandarizados al cuadrado
plot(res_std, type="l")
abline(h=0, col="red",lwd=1)
# Se observa que muchos residuos están centrados en el cero, podrían seguir una distribución white noise


### Apartado (g)

# Ahora ajustamos un modelo ARMA/GARCH a la siguiente serie transformada
log_Tbill=diff(log(Tbill))
plot(log_Tbill, type="l")

# Empezamos ajustando la partee ARMA
acf(log_Tbill) # q=1
pacf(log_Tbill) # p=0
# ARMA(0,1)

# Ajustamos la parte GARCH usando la serie al cuadrado
acf(log_Tbill^2) # q=1
pacf(log_Tbill^2) # p=1 o 2
# GARCH(1,1) o GARCH(2,1)

# Escribimos los dos modelos posibles
mod1=garchFit(formula = ~arma(0,1)+garch(1,1),log_Tbill)
mod2=garchFit(formula = ~arma(0,1)+garch(2,1),log_Tbill)

# Comprobamos la heteroscedasticidad
checkresiduals(log_Tbill)
checkresiduals(log_Tbill^2)

# Usamos la funcion auto.arima para ver que modelo nos propone
auto.arima(log_Tbill)

# Comprobamos si los parámetros son significativos o no 
mod1@fit$matcoef # vemos que todos tienen un p-valor menor que 0,05 y por lo tanto el modelo ajusta bien



## Ejercicio 2

rm(list=ls())

# Escribimos las librerias y los datos que usaremos
library(Ecdat)
library(fGarch)
data(SP500 ,package="Ecdat")

returnBM=SP500$r500[1805]
x=SP500$r500[(1804-2*253+1):1804]
plot(c(x,returnBM),type="l", main="S&P500") 
results=garchFit(~arma(1,0)+garch(1,1),data=x,cond.dist="std") 
dfhat=as.numeric(results@fit$par[6]) 
forecast=predict(results,n.ahead=1)

### Apartado (a)

# Queremos calcular la probabilidad condicionada de que nos devuelvan menos o igual que -0,288 en el Black Monday

# La probabilidad condicionada con los datos que tenemos se calcula de la siguiente manera:
# P(Y_t < returnBM) = P(Y_t - \hat{Y_t} < returnBM - \hat{Y_t}) = P(\sigma_t*\epsilon_t < returnBM - \hat{Y_t}) 
# = P(\epsilon_t < (returnBM - \hat{Y_t})/\sigma_t) con \epsilon_t ~ t_\hat{df}

pt((returnBM-forecast$meanForecast)/forecast$standardDeviation,df=dfhat)
# Obtenemos que la probabilidad de que nos devuelvan menos o igual que el 28,8% es de 8,132712e-05


### Apartado (b) 

# Calculamos los residuos estandarizados y hacemos un gráfico de éstos
res=residuals(results)
stand_res=res/sd(res)
plot(stand_res, type="l", main="standard residuals")

# Hacemos el ACF de los residuos estandarizados y los residuos estandarizados al cuadrado
acf(stand_res)
acf(stand_res^2)

# Hacemos el AFC de los residuos y de sus cuadrados
acf(res)
acf(res^2)

# Veamos si la parte AR(1) y la parte GARCH(1,1) nos indican un buen modelo usando la prueba Ljung-Box
Box.test(res, type="Ljung") # p-valor de 0.3358 mayor que el valor de significancia, podemos decir que la parte 
# AR(1) ajusta bien
Box.test(res^2, type="Ljung") # p-valor de 0.06368 mayor también que 0.05, por lo tanto la parte GARCH(1,1)
# ajusta bien


### Apartado (c)

# Vemos ahora si un modelo AR(1)/ARCH(1) proporciona un ajuste adecuado, ajustemos los datos
results2=garchFit(~arma(1,0)+garch(1,0),data=x) 

# Seguimos el mismo procedimiento que antes, calculamos los residuos estandarizados y hacemos los gráficos ACF
# de éstos y de sus cuadrados
res2=residuals(results2)
stand_res2=res2/sd(res2)
acf(stand_res2)
acf(stand_res2^2)

# Hacemos el AFC de los residuos y de sus cuadrados
acf(res2)
acf(res2^2)

# Vemos si ahora tenemos un ajuste adecuado para el modelo
Box.test(res2, type="Ljung") # p-valor de 0.3046, por lo tanto el modelo AR(1) ajusta bien
Box.test(res2^2, type="Ljung") # p-valor de 0.07339 por lo tanto podemos decir que ARCH(1) ajusta bien


### Apartado (d)

# Finalmente queremos ver si un modelo AR(1) proporciona un ajuste adecuado
results3=arima(x,order=c(1,0,0)) 

# De nuevo estudiaremos los residuos, empezamos haciendo los gráficos del modelo
res3=residuals(results3)
plot(res3, type="l")
acf(res3)

# Hacemos una prueba Ljung-Box para ver si es un buen modelo
Box.test(res3, type="Ljung") # obtenemos un p-valor de 0,9855 y por lo tanto nos quedamos con la hipótesis nula: 
# los datos se distribuyen de forma independiente. De esta manera afirmamos que es un buen modelo.
checkresiduals(res3)



## Ejercicio 3

# Queremos, dados los siguientes datos, ajustar un modelo ARMA(p,q)/GARCH(P,Q)

# Las librerias que usaremos son las siguientes
library(forecast)
library(rugarch)
library(tseries)
library(lmtest)
require(fBasics)

# Eliminamos todas las variables de nuestra zona de trabajo
rm(list=ls())
cat("\f")

# Añadimos los datos descargados
get(load("C:/Users/guill/OneDrive - Universitat Autonoma de Barcelona/Escritorio/Mates/Cursos/Mates 4t/Gestió financera/Lliuraments/HW2/data_HW_3.RData" ))
# es necesario cambiar la esta línea del codigo según donde tenga cada uno los datos guardados

# Representamos la serie temporal
plot(x, type="l") # aunque no sabemos si es estacionaria o no parece que no necesite ser transformada
par(mfrow=c(1,2))
acf(x)
pacf(x)
# Podemos ver que parece que sigue un modelo ARMA(2,0), ya que se ve un descenso fuerte en la gráfica PACF y en
# la gráfica ACF una decadéncia sin un corte brusco, por lo tanto q=0.

# Intentamos predecir el modelo ARMA
fit1=auto.arima(x); fit1 # obtenemos un AIC de 1880,44 y un BIC de 1901,51
# Hacemos la siguiente prueba para ver si los coeficientes son significativos
coeftest(fit1) # uno de los dos coeficientes de AR no es significativo

# Ajustamos otro modelo ARMA, hacemos el mismo procedimiento usando el modelo ARMA(2,0), éste es el modelo
# el cual hemos visto que se ajusta gráficamente
fit2=arima(x, c(2,0,0)); fit2 # obtenemos un AIC de 1880,08 (menor que el anterior)
BIC(fit2) # obtenemos un BIC de 1896.941 (también menor que el anterior)
# Como el AIC y el BIC son más pequeños, este modelo es mejor que el que hemos predecido, sólo nos falta ver
# si los dos coeficientes son significativos por tal de elegirlo
coeftest(fit2) # esta vez los dos coeficientes de AR sí que son significativos, por lo tanto nos quedamos con 
# este modelo

# Estudiamos ahora los residuos de la serie
res=residuals(fit2)
plot(res)
par(mfrow=c(1,2))
acf(res)
pacf(res)
checkresiduals(fit2) # el p-valor es mayor que el nivel de significancia, por lo tanto no estan correlacionados
# y podemos decir que ARMA(2,0) ajusta bien el modelo

# Pasamos a estudiar ahora el modelo ARMA(2,0)/GARCH(P,Q), tenemos que encontrar que P y que Q
# hacen que el modelo elegido tenga un buen ajuste. Para eso representamos la serie temporal de 
# los residuos al cuadrado
par(mfrow=c(1,1))
acf(res^2)
pacf(res^2)
# Podemos ver que parece que siga un modelo GARCH(1,0) ya que hay un fuerte descenso en el gráfico PACF (p=1) y un 
# descenso muy lento en el gráfico ACF (q=0)

# Ajustamos ahora el modelo con diferentes órdenes
fit3=garchFit(~arma(2,0)+garch(1,0),data=x) #ARMA(2,0)/GARCH(1,0)
fit4=garchFit(~arma(2,0)+garch(1,1),data=x) #ARMA(2,0)/GARCH(1,1)
fit5=garchFit(~arma(2,0)+garch(1,2),data=x) #ARMA(2,0)/GARCH(1,2)
fit6=garchFit(~arma(2,0)+garch(2,0),data=x) #ARMA(2,0)/GARCH(2,0)
fit7=garchFit(~arma(2,0)+garch(2,1),data=x) #ARMA(2,0)/GARCH(2,1)
fit8=garchFit(~arma(2,0)+garch(2,2),data=x) #ARMA(2,0)/GARCH(2,2)

# Miramos el AIC y el BIC de cada modelo
summary(fit3) # AIC=3.161444, BIC=3.203590
summary(fit4) # AIC=3.165406, BIC=3.215982
summary(fit5) # AIC=3.172969, BIC=3.231974
summary(fit6) # AIC=3.168999, BIC=3.219575
summary(fit7) # AIC=3.172999, BIC=3.232004
summary(fit8) # AIC=3.176969, BIC=3.244403
# Observamos que el modelo que tiene el AIC y BIC más pequeños es GARCH(1,0)


# Usamos la función ugarchspec, ésta se utiliza para ajustar modelos GARCH a series temporales financieras.
# Con esta función podemos definir los parámetros del modelo GARCH
umodel3=ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
umodel4=ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,1)
umodel5=ugarchspec(variance.model = list(garchOrder=c(1,2)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,2)
umodel6=ugarchspec(variance.model = list(garchOrder=c(2,0)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(2,0)
umodel7=ugarchspec(variance.model = list(garchOrder=c(2,1)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(2,1)
umodel8=ugarchspec(variance.model = list(garchOrder=c(2,2)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(2,2)

# Usamos ahora la función ugarchfit, ésta es una función que permite ajustar un modelo GARCH a nuestros datos y 
# estimar los parámetros del modelo. Por lo cual nos facilita el análisis y la gestión de la volatilidad en las 
# series temporales financieras.
ufit3=ugarchfit(umodel3, x)
ufit4=ugarchfit(umodel4, x)
ufit5=ugarchfit(umodel5, x)
ufit6=ugarchfit(umodel6, x)
ufit7=ugarchfit(umodel7, x)
ufit8=ugarchfit(umodel8, x)

# Miramos ahora los AIC y BIC obtenidos
infocriteria(ufit3) # AIC=3.171588, BIC=3.213734
infocriteria(ufit4) # AIC=3.175441, BIC=3.226017
infocriteria(ufit5) # AIC=3.179441, BIC=3.238446
infocriteria(ufit6) # AIC=3.175435, BIC=3.226011
infocriteria(ufit7) # AIC=3.179435, BIC=3.238440
infocriteria(ufit8) # AIC=3.183435, BIC=3.250869

# Vemos que el modelo que tiene tanto el AIC como el BIC más bajo es el ARMA(2,0)/GARCH(1,0)

# Finalmente comprobamos que todos los coeficientes son significativos
ufit3 # todos tienen p-valor mayor que 0,05, por lo tanto son significativos

# Para acabar comprobamos los residuos, hacemos su gráfico y sus gráficos ACF y PACF
res=residuals(ufit3)
plot(res)
acf(res) # los datos no estan correlacionados
pacf(res) # los datos estan dentro del nivel de significancia 0, vemos que los dos gráficos son ruido blanco
# Vemos que los residuos no estan correlacionados, lo que nos indica que es un buen modelo
Box.test(res, type="Ljung") # p-valor mayor que 0,05, por lo tanto afirmamos que los residuos no están
# correlacionados y que hemos ajustado un buen modelo
