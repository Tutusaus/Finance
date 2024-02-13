#Practica 8

library('Ecdat')
library('MASS')
library(PerformanceAnalytics)
library(stats)
library(normtest)
library(ggpubr)
library(symmetry)
library(ggplot2)
library(lattice)
library(moments)
library(quantmod)
# Aquests són tots els paquets que farem servir


# Exercici 1
data(SP500, package='Ecdat')
data_bm = SP500$r500[(1806 - 2*253) : 1805]
plot(1:length(data_bm), data_bm, type='l', lwd=1, main="Valor de Mercat del S&P500", ylab="Valor", xlab="Temps") #dibuix
abline(v=length(data_bm), col='red')


#a)
data_ts = SP500$r500[(1805 - 2*253) : 1804] 
hist(data_ts, breaks=100 , main = "Histograma de la sèrie temporal de SP500", xlab="Dades", ylab="Freqüència") #les putes dates? o canviar el titol

densityplot(data_ts, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
            title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE) 

#b)
a = fitdistr(data_ts, 'normal') 

qnorm(0.01, mean=as.numeric(a[[1]][1]), sd=as.numeric(a[[1]][2]))
quantile(data_ts, 0.01) 


# Exercici 2
path_sample = function(N, t0 , tn , S0 , r , sigma){
  S = replicate(N+1, 0)
  S[1] = S0
  epsilon = rnorm(N, mean = 0, sd = 1)
  T = (tn-t0)/N
  for(i in 1:N){
    S[i+1] = S[i] + r*S[i]*T + sigma*S[i]*sqrt(T)*epsilon[i]
  }
  return(S)
}

monte_carlo = function(M,N, t0, tn, s0, r, sigma, payoff_function, K){
  payoff = replicate(M, 0)
  for(i in 1: M){
    stock = path_sample(N, t0 , tn , s0 , r , sigma)
    S = stock[length(stock)]
    payoff[i] = payoff_function(S, K)
  }
  m = mean(payoff)
  price = m * exp(-r*(tn-t0))
  payoff <- payoff-price
  return(payoff)
}

payoff_function = function(K, S) {
  return(max(K - S, 0))
}

N = 1000
M = 1000
t_0 = 0
t_N = 1
S_0 = 100
r = 0.01
sigma = 0.3
K = 75

payoffs = monte_carlo(N, M, t_0, t_N, S_0, r, sigma, payoff_function, K)

#mirem quines distriibucions s'assemblen
hist(payoffs, breaks=100, main = "Histograma dels payoff", xlab="Dades", ylab="Freqüència")
quantile(payoffs, 0.01)
quantile(payoffs, 0.15)


#Apartat 2 del exercici 2
monte_carlo = function(M,N, t0, tn, s0, r, sigma, payoff_function, K){
  payoff = replicate(M, 0)
  for(i in 1: M){
    stock = path_sample(N, t0 , tn , s0 , r , sigma)
    S = stock[length(stock)]
    payoff[i] = payoff_function(S, K)
  }
  m = mean(payoff)
  price = m * exp(-r*(tn-t0))
  payoff <- payoff-price
  return(price)
}
#a)
L = 700
interest_rate = rnorm(L, mean = 0.05 , sd = sqrt(0.01))

hist(interest_rate)
plot(interest_rate, type = 'l')

vol = rnorm(L, mean = 0.4, sd = sqrt(0.1))
hist(vol)
plot(vol, type = 'l')


# b)
prices <- rep(0,length=L)
for (i in 1:L){
  prices[i] <- monte_carlo(M,N, t_0, t_N, S_0, interest_rate[i], vol[i],payoff_function,K)
}
#com te molts loops esta molta estona a calcular els diferents prices.
plot(prices, type = 'l')
hist(prices, breaks = 100, main = "Histograma dels preus", xlab="Dades", ylab="Freqüència")
mean(prices)
stock = path_sample(N, t0 , tn , S0 , 0.05, 0.01)

plot(seq(from = t0, tn, by= 1/N), stock, type = "l", xlab = "Temps", ylab = "Preu de l'estoc")


