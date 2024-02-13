library(quantmod)
library(purrr)

# Exercici 1
# a)
stocks_tickers = c('ANA.MC', 'ACX.MC', 'ACS.MC', 'AENA.MC', 'AMS.MC', 'MTS.MC', 'SAB.MC', 'BKT.MC', 'BBVA.MC', 'CABK.MC', 'CLNX.MC', 'ENG.MC', 'ELE.MC', 'FER.MC', 'FDR.MC', 'GRF.MC', 'IAG.MC', 'VIS.MC', 'TEF.MC', 'SGRE.MC', 'SAN.MC', 'REP.MC', 'REE.MC', 'NTGY.MC', 'MRL.MC', 'MEL.MC', 'MAP.MC', 'IDR.MC', 'ITX.MC', 'IBE.MC')

getSymbols(
  stocks_tickers,
  from='2016/01/01',
  to='2016/12/17',
  periodicity='daily'
)

getSymbols( '^IBEX', from='2016/01/01', to='2016/12/17', periodicity='daily')

#index IBEX35
index_IBEX = IBEX$IBEX.Adjusted
#components IBEX35
stocks_prices = map(stocks_tickers, function(x) Ad(get(x)))
stocks_prices = reduce(stocks_prices, merge)
colnames(stocks_prices) = stocks_tickers
#stocks_prices

#Calculem els netreturns dels components
nret = data.frame()
for (j in 1:length(stocks_tickers)){
  for( i in 2:length(stocks_prices$ANA.MC)){
    nret[i,j] = (as.numeric((stocks_prices[i,j]))/as.numeric(stocks_prices[(i-1),j])) -1
  }
}
nret = na.omit(nret)#traiem el primer dia

colnames(nret, do.NULL = TRUE, prefix = "col")
colnames(nret) = stocks_tickers
nret

#calculem els log returns
logret=data.frame()
logret = log(1 + nret)

#Calculem els logreturns de l'index
nret_index = replicate(length(index_IBEX),0)
for(i in 2:length(index_IBEX)){
  nret_index[i] = (as.numeric(index_IBEX[i])/as.numeric(index_IBEX[i-1]))-1 
}
nret_index = nret_index[-1] 
logret_index = replicate(length(index_IBEX),0)
logret_index = log(1 + nret_index)

#b)
#regresio x cada u dells

for( i in 1:length(stocks_tickers)){
  m = lm(nret[,i] ~ nret_index -1)
  print(stocks_tickers[i])
  print(coefficients(m) )
}
#com volem risk free rate=0 llevem el primer punt 

#c)
#Agafem CAIXABANK per beta major a 1 i NATURGY com a menor
regresio_MAJOR = lm(nret$CABK.MC ~nret_index -1) 
coef_MAJOR = regresio_MAJOR$coefficients
coef_MAJOR
regresio_MENOR = lm(nret$NTGY.MC ~nret_index -1)
coef_MENOR = regresio_MENOR$coefficients
coef_MENOR
plot()
#d)
#calculem en base 100 les 3 gr?fiques que volem(index, b>1, b<1)
ibex = c(100)
lacaixa = c(100)
naturgy = c(100)
for ( i in 2:(dim(IBEX)[1])){
  ibex = c(ibex, ibex[i-1]*(1+nret_index[i-1]))
  lacaixa = c(lacaixa, lacaixa[i-1]*(1+nret[i-1,10]))
  naturgy = c(naturgy, naturgy[i-1]*(1+nret[i-1,24]))
}

dies = seq(from=as.Date("4/1/2016", format="%d/%m/%Y"),to=as.Date("16/12/2016", format="%d/%m/%Y"), "day")
diaset = weekdays(dies)
dies = dies[!(diaset == "diumenge" | diaset == "dissabte")]
dies = dies[!(dies == "2016-03-25" | dies == "2016-03-28")]

#dibuixem la performance
plot(dies, ibex, type = "l", lwd=1, main="Variaci? valor accions Caixabank i Naturgy durant el 2016", ylab="Preu", xlab="Temps",ylim=c(55,115))
lines(dies, lacaixa, col= "purple" )
lines(dies, naturgy, col="turquoise")
pos= match(as.Date("23/6/2016", format="%d/%m/%Y"),dies)
abline(v=dies[pos], col="pink")
legend("bottomright", legend = c("?ndex IBEX","CaixaBank","Naturgy","Referendum day"), col=c("black","purple","turquoise","pink"), lty=1:1, cex=0.57)
