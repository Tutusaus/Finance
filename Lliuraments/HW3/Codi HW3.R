
# Els building blocs són els següents
# long call = max(S-p,0)
# short call = min(0,p-S)
# long put = max(p-S,0)
# short put = min(0,S-p)
# A més s'ha de tenir en compte que quan tenim l'underlying stock hi ha un factor
# S+k o -S+k que afecta al profit com en el cas de les covered i el collar.

#Bull Spread
S = seq(80, 140,1)#estoc
bslong=pmax(S-100,0)
bsshort=pmin(0,120-S)
bs=rowSums(cbind(bslong,bsshort))
plot(S,bs,type="l", col="red",ylim=c(-10,25),ylab="payoff",xlab="Preu estoc",main="Bull Spread")
lines(S,bs-3,lty=2,col="blue")
abline(h=0,lty=20)
legend(110,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)

#Bear Spread
S = seq(80, 140,1)#estoc
bs1long=pmax(120-S,0)
bs1short=pmin(0,S-100)
bs1=rowSums(cbind(bs1long,bs1short))
plot(S,bs1,type="l", col="red",ylim=c(-10,25),ylab="payoff",xlab="Preu estoc",main="Bear Spread")
lines(S,bs1-3,lty=2,col="blue")
abline(h=0,lty=20)
legend(110,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)

#Covered Call
S = seq(80, 140,1)#estoc
cclong=S-100
ccshort=pmin(0,110-S)
cc=rowSums(cbind(cclong,ccshort))
plot(S,cc,type="l", col="red",ylim=c(-10,15),ylab="payoff",xlab="Preu estoc",main="Covered Call")
lines(S,cc+3,lty=2,col="blue")
abline(h=0,lty=20)
legend(110,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)


#Covered Put
S = seq(80, 140,1)#estoc
cplong=100-S
cpshort=pmin(0,S-90)
cp=rowSums(cbind(cplong,cpshort))
plot(S,cp,type="l", col="red",ylim=c(-10,15),ylab="payoff",xlab="Preu estoc",main="Covered Put")
lines(S,cp+3,lty=2,col="blue")
abline(h=0,lty=20)
legend(115,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)


#Collar
S = seq(80, 140,1)#estoc
c1=S-100
clong=pmax(90-S,0) 
cshort=pmin(0,110-S) 
c=rowSums(cbind(c1,clong,cshort))
plot(S,c,type="l", col="red",ylim=c(-15,15),ylab="payoff",xlab="Preu estoc",main="Collar")
lines(S,c-3,lty=2,col="blue")
abline(h=0,lty=20)
legend(115,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)


#Butterfly
S = seq(80, 140,1)#estoc
b1=pmin(S-100,0)
bpshort=pmin(0,S-90)
bclong=pmax(S-100,0) 
bplong=pmax(100-S,0)
bpshort=pmin(0,110-S)
b2=pmax(S-120,0)
b=rowSums(cbind(b1,bpshort,bclong,bplong,bpshort,b2))
plot(S,b,type="l", col="red",ylim=c(-15,15),ylab="payoff",xlab="Preu estoc",main="Butterfly")
lines(S,b-3,lty=2,col="blue")
abline(h=0,lty=20)
legend(115,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)

#Condor
S = seq(80, 140,1)#estoc
cc100short=pmin(0,100-S)
cc90long=pmax(S-90,0) 
cc120long=pmax(S-120,0)
cc110short=pmin(0,110-S)
cd=rowSums(cbind(cc100short,cc90long,cc120long,cc110short))
plot(S,cd,type="l", col="red",ylim=c(-15,15),ylab="payoff",xlab="Preu estoc",main="Condor")
lines(S,cd-3,lty=2,col="blue")
abline(h=0,lty=20)
legend(115,-5,legend = c("Sense premium","Amb premium"), col=c("red","blue"),lty=1:2, cex=0.8)
