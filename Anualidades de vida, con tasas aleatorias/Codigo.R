#punto 1
# Par´ametros de la ley de Perks
pars <- c(0.00016509, 0.00004720, 0.09048066)
# Edades de las personas
x1 <- 45 # Persona x
x2 <- 15 # Persona y
# Par´ametros del inter´es
i <- 0.06 # Tasa de inter´es
m <- 12 # Pagos mensuales
v <- 1 / (1 + i) # Factor de descuento
C <- 2.5 # Valor del costo
# Edad l´ımite
w <- 110
# Funci´on de supervivencia para una vida
tpx.pe11 <- function(t, x1, pars) {
  a1 <- pars[1]
  a2 <- pars[2]
  a3 <- pars[3]
  g <- (1 - a1) / a3
  v <- exp(-a1 * t) * ((a2 * exp(a3 * x1) + 1) / (a2 * exp(a3 * (x1 + t)) + 1))^g
  return(v)
}
# Funci´on corregida a_xy_m1
a_xy_m1 <- function(x1, x2, m, i, pars) {
  v = 1 / (1 + i) # Factor de descuento
  k = seq(0, m * (w - max(x1, x2)) - 1, 1) # Secuencia de pagos desde 0 hasta el l´ımite
  sum_v <- 0 # Inicializa la suma
  # Iterar sobre los periodos de tiempo
  for (k_val in k) {
    t <- k_val / m # Tiempo en unidades de a~nos
    surv_x1 <- tpx.pe11(t, x1, pars) # Probabilidad de supervivencia de x1
    surv_x2 <- tpx.pe11(t, x2, pars) # Probabilidad de supervivencia de x2
    sum_v <- sum_v + (v^t) * (surv_x1 * surv_x2) # Agregar al sumatorio
  }
  # Calcula la anualidad conjunta
  a <- sum_v / m
  return(a)
}
(a_xy_m_value = a_xy_m1(x1,x2,m,i,pars))
(costo <- C * m * a_xy_m_value)

# Punto 2

# Par´ametros de la ley de Perks
pars <- c(0.00016509, 0.00004720, 0.09048066)
# Edades de las personas
x1 <- 45 # Persona x
x2 <- 15 # Persona y
# Par´ametros del inter´es
i <- 0.06 # Tasa de inter´es
m <- 12 # Pagos mensuales
v <- 1 / (1 + i) # Factor de descuento
C <- 2.5 # Valor del costo
# Edad l´ımite
w <- 110
# Funci´on de supervivencia para una vida
tpx.pe11 <- function(t, x1, pars) {
  a1 <- pars[1]
  a2 <- pars[2]
  a3 <- pars[3]
  g <- (1 - a1) / a3
  v <- exp(-a1 * t) * ((a2 * exp(a3 * x1) + 1) / (a2 * exp(a3 * (x1 + t)) + 1))^g
  return(v)
}
# C´alculo de la anualidad de vida pura (una vida)
aaxm <- function(x, m, i, pars) {
  v = 1 / (1 + i)
  k = seq(0, m * (w - x) - 1, 1)
  vkm = v^(k / m)
  kmpx = tpx.pe11(k/m, x, pars)
  a = sum(vkm * kmpx)/m
  return(a)
}
# C´alculo de la anualidad pura para y
(a_y_m <- aaxm(x2, m, i, pars))
# C´alculo de la anualidad con reversi´on para x|y
(a_x_given_y_m <- a_y_m - a_xy_m_value)
# 4. C´alculo del costo total
(costo <- C * m * a_x_given_y_m)

# Punto 3
# Par´ametros del modelo Perks
pars <- c(0.00016509, 0.00004720, 0.09048066) # Par´ametros que diste antes
# Edades de las personas
x1 <- 45 # Edad de la persona 1
x2 <- 15 # Edad de la persona 2
w <- 110
C=2.5
# Par´ametros del inter´es
i <- 0.06 # Tasa de inter´es
m <- 12 # Pagos mensuales
v <- 1 / (1 + i) # Factor de descuento
# Funci´on de supervivencia
tpx.pe11 <- function(t, x, pars) {
  a1 <- pars[1]
  a2 <- pars[2]
  a3 <- pars[3]
  g <- (1 - a1) / a3
  v <- exp(-a1 * t) * ((a2 * exp(a3 * x) + 1) / (a2 * exp(a3 * (x + t)) + 1))^g
  return(v)
}
require(GoFKernel)
Zy <- double(5000)
Zxy <- double(5000)
# Funci´on para calcular Z a partir de los tiempos simulados
for (j in 1:5000) {
  v <- 1 / (1 + i) # Factor de descuento
  f1 <- function(t) (1- tpx.pe11(t,x1,pars))/0.9994966
  Tx = random.function(1, f1, lower = 0, upper = 110-x1, kind = "cumulative")
  f2 <- function(t) (1- tpx.pe11(t,x2,pars))/0.9994966
  Ty = random.function(1, f2, lower = 0, upper = 110-x2, kind = "cumulative") # Simula tiempo de muerte para x2
  txy <- seq(0,floor(m*min(Tx,Ty)))
  ty <- seq(0,floor(m*Ty))
  Zy[j] <- C * sum(v^{ty/m})
  Zxy[j] <- C * sum(v^{txy/m})}
hist(Zy-Zxy)
(mediaZ <- mean(Zy)-mean(Zxy))
# Agregar una l´ınea vertical en la posici´on de la media
abline(v = mediaZ, col = "red", lwd = 2)
# Agregar leyenda con el valor de la media
legend("topright", legend = paste("Media =", round(mediaZ, 4)), col = "red", lwd = 2)
