D <- read.csv("C:\\Users\\Juan\\Documents\\FMRTX.csv",sep = ",")
imk = diff(log(D$Close),1,1)
dmk = log(1+imk)
fechas = as.Date(D$Date[-1],format="%Y-%m-%d")
#-----------agregar desde dia a mes
library(highfrequency)
library(xts)
imk = xts(x=imk, order.by = fechas)
# Calculo de las tasas geometricas
ts.month <- apply.monthly(imk,FUN=sum)
dmk = log(1+ts.month)
library(GeneralizedHyperbolic)
# Ajustar el modelo NIG a las tasas geom´etricas
mnig = nigFit(dmk)
nig.est = mnig$param # Par´ametros estimados del modelo NIG
# Calcular la varianza de las tasas geom´etricas bajo el modelo NIG
sigma2 = nigVar(param = nig.est)
d = density(dmk)
plot(d$x, log(d$y),
     type = "l", col='blue', lwd=2,
     xlab="Tasas geom´etricas",
     ylab="Log-Densidad",
     main="Log-Densidades Observadas vs Estimadas",ylim = c(-10, 6.0))
lines(d$x, log(dnig(d$x, param = nig.est)),
      lwd=2, col='red')
legend("topright", legend=c("Observada", "Estimada"),
       col=c("blue", "red"), lwd=2, lty=1)
d = density(dmk)
plot(d$x,(d$y),
     type = "l", col='blue',lwd=2,ylim = c(-10, 40),
     xlab="Tasas geom´etricas",
     ylab="-Densidad",
     main="Densidades Observadas vs Estimadas")
lines(d$x,(dnig(d$x, param = nig.est)),
      lwd=1,type = 'l', col='red')
lines(d$x,(dnorm(d$x, mean = mean(dmk), sd= sd(dmk))),
      lwd=1,type = 'l', col='green')
legend("topright", legend=c("Observada", "Estimada NIG","Disitribucion normal"),
       col=c("blue", "red","green"), lwd=2, lty=1)

# Literal 2
library(GMCM)
ejex.mes <- seq(min(fechas), max(fechas), by = "month")
ejex.a <- seq(min(fechas), max(fechas), by = "year")
plot(fechas,GMCM:::cummean(imk), xaxt="n", panel.first = grid(),
     type='l',ylab='valor unidad fondo FTHRX')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.a, labels = FALSE, tcl = -0.2)
abline(a=0,b=0, col="red")
cimk = GMCM:::cummean(imk)
n1=which(cimk>0)
mimk = mean(imk[n1])
ia = (1+mimk)^(360)-1
iq = 0.02
C = 2.5
m = 12
n = 10
q = 1
t = seq(1,n*m)
cj = rep(C,n*m)*(1+iq)^(floor(t*q/m)/q)
Gavqmn = function(i,m,q,n,iq){
  try(if(iq > i) stop("tasa inflacion invalida"))
  try(if(m%%q != 0) stop("m no es divisible por q"))
  t = seq(1,n*m,1)
  res = (1/m)*sum((1+i)^(-t/m)*(1+iq)^(floor(t*q/m)/q))
  return(res)}
(Cp = C*m*Gavqmn(ia,m,q,n,iq))
## Literal c
ndim = n*m
R = matrix(0,ndim,ndim)
for(i in 1:ndim){
  for(j in 1:ndim){
    R[i,j] = ifelse(i <= j, i,j)}}
R = sigma2*R
Rho = cov2cor(R)
Rho[1:10,1:10]
set.seed(123)
library(mvtnorm)
sim.GC <- function(n, Rho, qnig){
  dat <- rmvnorm(n=n, mean = rep(0,nrow(Rho)), sigma = Rho)
  for(j in 1:nrow(Rho)){
    dat[,j] <- qnig(pnorm(dat[,j]),param = c(j,j,1,1)*nig.est)
  }
  return(dat)
}
Lambda.j = sim.GC(500,Rho,qnig)
Lambda.j[1:8,1:8]
par(mfrow=c(1,2))
hist(Lambda.j[,1],40)
hist(Lambda.j[,60],40)
sesgo_lambda1 = 3*(mean(Lambda.j[,1]) - median(Lambda.j[,1]))/sd(Lambda.j[,1])
sesgo_lambda60 = 3*(mean(Lambda.j[,60]) - median(Lambda.j[,60]))/sd(Lambda.j[,60])
par(mfrow=c(1,2))
plot(Lambda.j[,1], Lambda.j[,60], type='p', main="Gr´afico 2D de Lambda.j",
     xlab="Lambda.j[,1]", ylab="Lambda.j[,60]")
library(scatterplot3d)
scatterplot3d(Lambda.j[,1:3], pch = 16, color="steelblue", main="Gr´afico 3D de Lambda.j")
par(mfrow=c(1,2))
library(moments)
Y = exp(-Lambda.j)%*%cj
S = apply(Y,1,sum)
hist(S,40)
plot(density(S))


# literal d
skewness(S)
kurtosis(S)
library(gamlss)
Sfit1 <- fitDist(y = S, type = "realplus")
Sfit1$fits[1:5]
par(mfrow=c(1,2))
library(gamlss)
library(gamlss.dist)
# Estimaci´on de m´axima verosimilitud con exGAUS
histDist(S, family = "exGAUS", main = "exGAUSS", ylim = c(0,0.015))
H_exGAUSS = gamlssML(S, family = exGAUS)
str(H_exGAUSS)
# Coeficientes estimados para la distribuci´on exGAUSS
coef.exGAUSS = c(mu = H_exGAUSS$mu, sigma = H_exGAUSS$sigma, nu = H_exGAUSS$nu)
# Visualizaci´on del ajuste exGAUSS
hist(S, 50, probability = TRUE,
     col = 'light gray', main = 'Valor presente')
curve(dexGAUS(x, mu = coef.exGAUSS["mu"], sigma = coef.exGAUSS["sigma"], nu = coef.exGAUSS["nu"],
              log = FALSE),
      col = 'red', lwd = 2, lty = 1, add = TRUE)
# Leyenda del gr´afico
legend("topright",
       c("Datos", "M´ax. verosimilitud"),
       col = c("darkgray", "red"),
       lwd = c(2, 2), lty = c(1, 1))
(c(Cp,mean(S)))
q90<- qexGAUS(0.9, mu = coef.exGAUSS[1], sigma = coef.exGAUSS[2], nu = coef.exGAUSS[3])
recargo = (q90 - mean(S)) / mean(S)