#Practica 5
library(timeSeries)
library(fBasics)
install.packages("normtest")
library(normtest)
#Exercici 1:
  #Apartat a)
coin_toss = function(N){
  v = replicate(N+1, 0)
  profit = 1/sqrt(N)
  v[1]= 0
  WinLoss = sample(c(0,1), replace=TRUE, size=N+1)
  for (i in 2:(N+1)){
    if(WinLoss[i] == 0){
      v[i] = v[i-1] + profit
    }
    else{
      v[i] = v[i-1] - profit
    }
  }
  return(v)
}

#Ara fem el gràfic de les diferents N's proposdes:

N = 5
vec =  coin_toss(N)
plot(seq(from = 0, to = 1, by = 1/N), vec, type = "l")

N = 10
vec =  coin_toss(N)
plot(seq(from = 0, to = 1, by = 1/N), vec, type = "l")

N = 50
vec =  coin_toss(N)
plot(seq(from = 0, to = 1, by = 1/N), vec, type = "l")

N = 100
vec =  coin_toss(N)
plot(seq(from = 0, to = 1, by = 1/N), vec, type = "l")

N = 10000
vec =  coin_toss(N)
plot(seq(from = 0, to = 1, by = 1/N), vec, type = "l")


#Apartat b)
sample_path_distribution = function(N, m){
  v = replicate(m+1, 0)
  for (i in 1:m) { 
    vec = coin_toss(N)
    v[i] = vec[length(vec)]
  }
  return(v)
}

N = m = 100
v = sample_path_distribution(N, m)
vTs = as.timeSeries(v)
densityPlot(vTs, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
            title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE)  
shapiro.test(v)

N = m = 1000
v = sample_path_distribution(N, m)
vTs = as.timeSeries(v)
densityPlot(vTs, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
            title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE ) 
shapiro.test(v)

N = 10000
m = 10000
v = sample_path_distribution(N, m)
vTs = as.timeSeries(v)
densityPlot(vTs, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
            title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE )
shapiro.test(v)#no funciona pq té un maxim de 5000 dades, utilitzem el Jarque-Bera
ajb.norm.test(v) 

#Exercici 2:
#Apartat a)

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
N = 1000 
t0 = 0 
tn = 1
S0 = 100
r = 0.01
sigma = 0.3
plot(seq(from = t0, tn, by= 1/N), path_sample(N, t0 , tn , S0 , r , sigma), type = "l", xlab = "Temps", ylab = "Preu de l'estoc")

#Mirem com canvia la r
for(i in 1: 20){
  stock = path_sample(N, t0 , tn , S0 , 0.1*i , sigma)
  if(i==1){
    plot(seq(from = t0, tn, by= 1/N),stock,ylim=c(0,500), type="l", xlab = "Temps", ylab = "Preu de l'estoc", main="Repercusió de l'interès en el preu de l'estoc")
    
  }
  else{
    lines(seq(from = t0, tn, by= 1/N), stock,col=rgb(0,i/20,i/20))
  }
}
#Mirem com canvia la sigma
for(i in 1: 10){
  stock = path_sample(N, t0 , tn , S0 , r, 0.1*i)
  if(i==1){
    plot(seq(from = t0, tn, by= 1/N),stock,ylim=c(0,250), type="l", xlab = "Temps", ylab = "Preu de l'estoc", main="Repercusió de la volatilitat en el preu de l'estoc")
    
  }
  else{
    lines(seq(from = t0, tn, by= 1/N), stock,col=rgb(i/10,0,i/10))
  }
}

#Apartat b)

payoff_function = function(S, K){
    return(max((S-K), 0))
}

#Apartat c)
monte_carlo = function(M,N, t0, tn, s0, r, sigma, payoff_function, K){
  payoff = replicate(M, 0)
  for(i in 1: M){
    stock = path_sample(N, t0 , tn , s0 , r , sigma)
    if(i==1){
      plot(seq(from = t0, tn, by= 1/N),stock,ylim=c(40,180), type="l", xlab = "Temps", ylab = "Preu de l'estoc")
      
    }
    else{
      lines(seq(from = t0, tn, by= 1/N), stock,col=i)
    }
    S = stock[length(stock)]
    payoff[i] = payoff_function(S, K)
  }
  m = mean(payoff)
  price = m * exp(-r*(tn-t0))
  return(price)
}
#Apartat d)
payoff_function = function(S, K){
  return(max((K-S), 0))
}
monte_carlo(10, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
monte_carlo(50, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
monte_carlo(150, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
monte_carlo(1500, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
monte_carlo(10000, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
monte_carlo(20000, 12, 0, 1, 90, 0.05, 0.4, payoff_function, 75)
