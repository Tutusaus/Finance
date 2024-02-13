# INTRODUCTION TO FINANCIAL ENGINEERING (HW0)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 



# Iniciamos el programa con los paquetes y librerías que utilizaremos

rm(list=ls())                     
library("quantmod")                      
library("moments")
library("fBasics")
options("getSymbols.warning4.0"=FALSE) 
cat("\f")


getSymbols('GOOG', scr='yahoo', from="2011-01-03", to="2014-12-31")
google_close=GOOG$GOOG.Close

getSymbols('SP', scr='yahoo', from="2011-01-03", to="2014-12-31")
SP_close=SP$SP.Close



## Ejercicio 1

# Antes de responder los apartados del ejercicio, calculamos los rendimientos netos diarios, para ello utilizaremos la
# función diff() que nos calcula la diferencia de precios (P_t-P_[t-1]) y la función lag() que calcula el precio 
# retrasado (P_[t-1]).

# Empezamos calculando el rendimiento neto diario de Google
function_google_net_returns=diff(google_close)/lag(google_close)
vector_google_net_returns=as.numeric(function_google_net_returns) # creamos un vector donde guardamos el rendimiento neto
google_net_returns=head(na.omit(vector_google_net_returns), n=length(function_google_net_returns)) # omitimos los NA

# Hagamos exactamente lo mismo para SP
function_SP_net_returns=diff(SP_close)/lag(SP_close)
vector_SP_net_returns=as.numeric(function_SP_net_returns)
SP_net_returns=head(na.omit(vector_SP_net_returns), n=length(function_SP_net_returns))


### Apartado (a)

# Empecemos calculando los parámetros que nos piden por Google
mean(google_net_returns) # media
sd(google_net_returns) # desviación estándar
skewness(google_net_returns) # asimetría
kurtosis(google_net_returns) # exceso de cortosis
min(google_net_returns) # mínimo
max(google_net_returns) # máximo

# Calculamos estos mismos parámetros por SP
mean(SP_net_returns) # media
sd(SP_net_returns) # desviación estándar
skewness(SP_net_returns) # asimetría
kurtosis(SP_net_returns) # exceso de cortosis
min(SP_net_returns) # mínimo
max(SP_net_returns) # máximo


### Apartado (b)

# Para obtener la función de densidad de forma empírica del rendimiento neto de las acciones de Google, haremos un plot
# de la densidad de ésta
density_function=density(google_net_returns)
plot(density_function, main="Empirical density function of the net returns of Google stock", 
     cex.main =0.95, xlab="Net returns of Google stock", ylab="Relative frequency", cex.lab = 0.6)

# Para ver si los datos siguen una distribución normal realizamos la prueba de normalidad de Shapiro-Wilk, por lo que
# supondremos que la variable presenta una distribución normal y refutaremos o aceptaremos ésta a partir del p-valor obtenido.
# Si el p-valor<0.005 refutamos la hipótesis, si el p-valor>0.005 lo aceptamos
shapiro.test(google_net_returns)


### Apartado (c)

# Calculamos el log return de Google utilizando el rendimiento neto (R_t), ya que r_t=log(1+R_t)
function_google_log_returns=log(1+function_google_net_returns)
vector_google_log_returns=as.numeric(function_google_log_returns) # creamos un vector donde guardamos el log returns
google_log_returns=head(na.omit(vector_google_log_returns), n=length(function_google_log_returns)) # omitimos los NA

# Calculamos el log return de SP
function_SP_log_returns=log(1+function_SP_net_returns)
vector_SP_log_returns=as.numeric(function_SP_log_returns)
SP_log_returns=head(na.omit(vector_SP_log_returns), n=length(function_SP_log_returns))

# Calculamos los parámetros que nos piden tanto por Google como por SP del log return
mean(google_log_returns) # media Google
sd(google_log_returns) # desviación estándard Google
skewness(google_log_returns) # asimetría Google
kurtosis(google_log_returns) # exceso de cortosis Google
min(google_log_returns) # mínimo Google
max(google_log_returns) # máximo Google

mean(SP_log_returns) # media SP
sd(SP_log_returns) # desviación estándard SP
skewness(SP_log_returns) # asimetría SP
kurtosis(SP_log_returns) # exceso de cortosis SP
min(SP_log_returns) # mínimo SP
max(SP_log_returns) # máximo SP


### Apartado (d)

# Hagamos un Test-T para probar la hipótesis nula, ésta es que la media es cero. Al igual que con la prueba la prueba de
# normalidad de Shapiro-Wilk, si el p-valor<0.005 refutamos la hipótesis y si el p-valor>0.005 no la rechazamos
t.test(google_log_returns)


### Apartado (e)

# Hagamos dos plots de las densidades de los log returns tanto de Google como de SP
density_function_google=density(google_log_returns)
plot(density_function_google, main="Empirical density function of the log returns of Google stock", 
     cex.main =0.95, xlab="Log returns of Google stock", ylab="Relative frequency", cex.lab = 0.6)

density_function_SP=density(SP_log_returns)
plot(density_function_SP, main="Empirical density function of the log returns of S&P composite index", 
     cex.main =0.8, xlab="Log returns of S&P composite index", ylab="Relative frequency", cex.lab = 0.6)

# Vemos si los datos siguen una distribución normal utilizando de nuevo la prueba Shapiro-Wilk
shapiro.test(google_log_returns)
shapiro.test(SP_log_returns)


### Apartatdo (f)

#Finalmente construimos el intervalo de confianza del 95% del log return de las acciones de Google, hagámoslo utilizando 
# el Test-T
t.test(google_log_returns, conf.level=0.95)$conf.int


## Ejercicio 2

cat("\f")

# Introducimos los datos que nos da el enunciado
D=240000 # préstamo que tomamos (en EUR) 
t=30 # tiempo que tenemos para pagar el préstamo (en años)
R=1.99/100 # interés (en %)


### Apartado (a)

# Calculamos la cuota mensual del préstamo a partir de la formula x=D*R/12+(D*R/12)/((1+R/12)^{12*t}-1)
monthly_installment=(D*R/12)+((D*R/12)/((1+(R/12))^{12*t}-1)); monthly_installment


### Apartado (b)

# Creamos una tabla de los meses que tiene nuestra variable t (son 30 años que equivalen a 360 meses)
data = data.frame(
  month=1:(12*t)
)

# Añadimos la primera fila de datos
data$debt[1]=c(D)
data$interest_accrued[1]=c(D*R/12)
data$capital_repayment[1]=c((D*R/12)/((1+(R/12))^{12*t}-1))

# Creamos un bucle que nos calcula los datos de los intereses pagados y la parte de amortización de capital 
# de la cuota para todos los pagos (360 pagos)
for(i in 2:360){
  data$debt[i]=(data$debt[i-1]-data$capital_repayment[i-1])
  data$interest_accrued[i]=(data$debt[i])*R/12
  data$capital_repayment[i]=data$capital_repayment[1]*(1+R/12)^{i-1}
}

tail(data) # revisamos que lo que hemos calculado sea correcto

# Creamos una tabla para añadir los mismos datos agrupados por años
data_anual=data.frame(
  year=1:t
)

# Agrupamos los datos anteriores en años
for(i in 1:t){
  data_anual$debt[i]=data$debt[12*(i-1)+1] # lo escribimos así porque estamos considerando el primer mes de cada año
  data_anual$interest_accrued[i]=sum(data$interest_accrued[(12*(i-1)+1):(12*i)])
  data_anual$capital_repayment[i]=sum(data$capital_repayment[(12*(i-1)+1):(12*i)])
}


### Apartado (c)
# Para saber el total de dinero que hemos pagado al banco, multiplicamos la cuota mensual (ya calculada en el apartado (a))
# por los meses que hemos estado pagando ésta (360 meses que equivalen a 30 años)
bank_c=monthly_installment*360; bank_c

# Miramos cuanto dinero acabamos pagando de más
diff_money=bank_c-D; diff_money


### Apartado (d)
Deute=240000 # Préstamo (en Euros)
Termini_crèdit=30 # Plazo de 30 años para pagarlo
Interès=1.99/100 # Interés (en %), es decir que inicialmente el interés es del 1.99%
Devolució_capital=(Deute*Interès/12)/((1+Interès/12)^(12*Termini_crèdit)-1) # Amortización inicial.
m=matrix(nrow=Termini_crèdit*12, ncol=3)
m[1,]=c(Deute,Deute*Interès/12,Devolució_capital)

for(j in 1:30){
  
  for(i in 1:12){ # Cada mes del año el Interest Accrued y el Capital Repayment se actualizan siguiendo las siguientes 
    # condiciones (hay que tener en cuenta que la suma de estos dos debe ser constante por año pagado, momento en el 
    # que se incrementará el interés un 0.1%)
    m[i+1+12*(j-1),1]=m[i+12*(j-1),1]-m[i+12*(j-1),3]
    m[i+1+12*(j-1),2]=m[i+1+12*(j-1),1]*Interès/12
    m[i+1+12*(j-1),3]=Devolució_capital*(1+Interès/12)^i
  }
  Termini_crèdit=Termini_crèdit-1
  Deute=m[12*j,1]-m[12*j,3]
  Interès=Interès+0.1/100 # Calculamos el nuevo interés
  Devolució_capital=(Deute*Interès/12)/((1+Interès/12)^(12*Termini_crèdit)-1) # Recalculamos la nueva amortización a principios de año
  
  m[12*j+1,]=c(Deute,Deute*Interès/12,Devolució_capital)# Añadimos las nuevas condiciones
  
}
m = m[-361,]

# Finalmente pagamos al banco un total de
bank_d=sum(m[,2],m[,3])
bank_d



