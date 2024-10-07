#-------------Ley Perks 1
# Definir la fuerza de mortalidad
muxt.pe1 = function(t,x,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  m=(a1+a2*exp(a3*(x+t)))/(1+a2*exp(a3*(x+t)))
  return(m)}
#---------Definir tpx
tpx.pe1 = function(t,x,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  g = (1-a1)/a3
  v = exp(-a1*t)*((a2*exp(a3*x)+1)/(a2*exp(a3*(x+t))+1))^g
  return(v)}
# parametros
pars=c(0.00025748 , 0.00002553,0.10128397)
n = 30
x= 50
i = 0.06
Ax1n = function(x,i,n,tpx,muxt,pars){
  v = 1/(1+i)
  ft = function(t)v^(t)*tpx(t,x,pars)*muxt(t,x,pars)
  p = integrate(ft,0,n)$value
  return(p)}
C=100
#Sin valor monetario
(Ax1n = Ax1n(x,i,n,tpx.pe1,muxt.pe1,pars))
#con valor monetario
q = C*Ax1n
q
#---------Punto 2---------------
library(flexsurv)
b=function(t){100*(1+iq)^{floor(t)}}
#---------Definir tpx para perk1
tpx.pe1 = function(t,x,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  g = (1-a1)/a3
  v = exp(-a1*t)*((a2*exp(a3*x)+1)/(a2*exp(a3*(x+t))+1))^g
  return(v)}
# parametros
pars=c(0.00025748 , 0.00002553,0.10128397)
iq=0.025
i=0.06
C=100
n = 30
x= 50
#Para hombres
Fx.h= function(t){
  dweibull(x+t,shape=3.75,scale=118.59)/pweibull(x,shape=3.75,scale=118.59,lower.tail = FALSE)
}
Fn.h <- function(t){
  ((1+i)^(-t)) * b(t) * Fx.h(t) * tpx.pe1(t,x,pars)
}
#para mujeres
Fx.m= function(t){
  dweibull(x+t,shape=4.28,scale=131.22)/pweibull(x,shape=4.28,scale=131.22,lower.tail = FALSE)
}
Fn.m <- function(t){
  ((1+i)^(-t)) * b(t) * Fx.m(t) * tpx.pe1(t,x,pars)
}
library(rmutil)
(rh= int(Fn.h,0,n))
(rm =int(Fn.m,0,n))

#Definimos mu y tpx para x1
muxt.pe11 = function(t,x1,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  m=(a1+a2*exp(a3*(x1+t)))/(1+a2*exp(a3*(x1+t)))
  return(m)}
tpx.pe11 = function(t,x1,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  g = (1-a1)/a3
  v = exp(-a1*t)*((a2*exp(a3*x1)+1)/(a2*exp(a3*(x1+t))+1))^g
  return(v)}
#Definimos mu y tpx para x2
muxt.pe12 = function(t,x2,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  m=(a1+a2*exp(a3*(x2+t)))/(1+a2*exp(a3*(x2+t)))
  return(m)}
tpx.pe12 = function(t,x2,pars){
  a1 = pars[1]
  a2 = pars[2]
  a3 = pars[3]
  g = (1-a1)/a3
  v = exp(-a1*t)*((a2*exp(a3*x2)+1)/(a2*exp(a3*(x2+t))+1))^g
  return(v)}
# parametros
pars=c(0.00025748 , 0.00002553,0.10128397)
x1=50
x2=35
iq=0.025
i=0.06
b=function(t){
  100*(1+iq)^{floor(t)}}#b(t)
#Minimo de x1, x2
fx1x2 <- function(t){
  ((1+i)^(-t))*b(t)*tpx.pe11(t,x1,pars)*tpx.pe12(t,x2,pars)*
    (muxt.pe11(t,x1,pars)+muxt.pe12(t,x2,pars))}
library(rmutil)
(rh= int(fx1x2,0,30))
