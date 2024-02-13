#Pràctica 6
#install.packages("moments")
#install.packages("symmetry")
# Aquests són tots els paquets que farem servir

rm(list=ls())
require(zoo)
require(xts)
library("quantmod")
library("moments")
library(symmetry)

# 1a)

getSymbols('GOOG', scr='yahoo', from="2015-01-01", to = "2015-12-31")
getSymbols('AAPL', scr='yahoo', from="2015-01-01", to = "2015-12-31")
getSymbols('MSFT', scr='yahoo', from="2015-01-01", to = "2015-12-31")
getSymbols('META',scr='yahoo',from="2015-01-01",to="2015-12-31")

google_adj <- GOOG$GOOG.Adjusted
facebook_adj <- META$META.Adjusted
apple_adj <- AAPL$AAPL.Adjusted
microsoft_adj <- MSFT$MSFT.Adjusted
# Ja tenim les dades

# 1b)

daily_google <- rep(1,length(google_adj))
daily_facebook <- rep(1,length(facebook_adj))
daily_apple <- rep(1,length(apple_adj))
daily_microsoft <- rep(1,length(microsoft_adj))

# Les passem a daily returns
for(i in 2:length(daily_microsoft)){
  daily_google[i] <- (as.numeric(google_adj[[i]])/as.numeric(google_adj[[i-1]])) - 1
  daily_facebook[i] <- (as.numeric(facebook_adj[[i]])/as.numeric(facebook_adj[[i-1]])) - 1
  daily_apple[i] <- (as.numeric(apple_adj[[i]])/as.numeric(apple_adj[[i-1]])) - 1
  daily_microsoft[i] <- (as.numeric(microsoft_adj[[i]])/as.numeric(microsoft_adj[[i-1]])) - 1
}

# I borrem l'1 del principi
daily_google <- daily_google[-1]
daily_facebook <- daily_facebook[-1]
daily_apple <- daily_apple[-1]
daily_microsoft <- daily_microsoft[-1]


#Calculem les mitjanes
mean(daily_google)
mean(daily_facebook)
mean(daily_apple)
mean(daily_microsoft)
#I fem la matriu d'elles
mitjanes <- c(mean(daily_google),
              mean(daily_facebook),
              mean(daily_apple),
              mean(daily_microsoft))
mitjanes <- matrix(mitjanes,nrow = 4)

#Calculem ara les covariances
data <- data.frame(daily_google,daily_facebook,daily_apple,daily_microsoft)
covariancies <- as.matrix(cov(data))


# 1c)

# Creem els pesos aleatoris
W <- runif(4,0,1)
w <- rep(0,length=4)

for(i in 1:4){
  w[i] <- (W[i]/sum(W))
}
#efectivament la suma ?s igual a 1
sum(w)
w

#Calculem ara la mitjana i la cov del portfoli associades a w(pesos)
vecN <- t(matrix(w))
portW <- matrix(w)
vecN
portW
mitjanaPort <- vecN%*%mitjanes
sigmaPort <- vecN%*%covariancies%*%portW
mitjanaPort
sigmaPort


# 1d)

#Calculem els pesos per 1000 vectors aleatoris diferents
matriuD <- matrix(rep(0,4000),nrow = 4,ncol=1000)

for(i in 1:1000){
    matriuD[,i] <- runif(4,0,1)
}

for(i in 1:1000){
  a = sum(matriuD[,i])
  for(j in 1:4){
    matriuD[j,i] <- matriuD[j,i]/a
  }
  a = 0
}

#I calculem la mitjana i les cov associades a cada un d'ells
matriuDT <- t(matriuD)
vecM <- rep(0,length=1000)
vecS <- rep(0,length=1000)

for(i in 1:1000){
  vecM[i] <- matriuDT[i,]%*%mitjanes
}
for(i in 1:1000){
  vecS[i] <- matriuDT[i,]%*%covariancies%*%matriuD[,i]
}
# I finalment ho dibuixem
plot(vecS,vecM)

# 2a)
# Creem les rbases
rbase <- seq(min(mitjanes),max(mitjanes),length=500)

# 2b)

uns <- matrix(rep(1,length=4),nrow=4)
unsT <- t(uns)
zeros <- matrix(rep(0,length=4),nrow = 2)

# La matriu Q
Q <- matrix(rbind(cbind(covariancies,mitjanes,uns),cbind(rbind(t(mitjanes),t(uns)),zeros)),nrow=6,ncol=6)
Q
#el vector b
b <- matrix(rep(0,length=3000),nrow=6,ncol=500)
for(i in 1:500){
  b[5,i] <- rbase[i]
  b[6,i] <- 1
}

# 2c)
#resolem el sistema
y <- matrix(rep(0,3000),nrow=6,ncol=500)
for(i in 1:500){
  y[,i] <- solve(Q,b[,i])
}

# 2d)
#repetim els calculs de 1c per aquest cas
calculs <- matrix(rep(0,2000),nrow=4,ncol=500)
for(i in 1:500){
  for(j in 1:4){
    calculs[j,i] <- y[j,i]
  }
}
# Fem els c?lculs
vecM1 <- rep(0,length=500)
vecS1 <- rep(0,length=500)
calculsT <- t(calculs)
for(i in 1:500){
  vecM1[i] <- calculsT[i,]%*%mitjanes
}
for(i in 1:500){
  vecS1[i] <- calculsT[i,]%*%covariancies%*%calculs[,i]
}
# Ja tenim el resultat amb els multiplicadors de lagrange
plot(vecS1,vecM1)

# 3
#dibuixem els dos dibuixos trobats anteriorment junts
plot(vecS1,vecM1)
points(vecS,vecM)






