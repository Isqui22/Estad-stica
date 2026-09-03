#### theta_momentos ####
f_mm <- function(muestra) {
  2*mean(muestra) # calculo dos veces el promedio
}
# el return esa implicito en R

# theta_maxima_verosimilitud

f_emv = function(muestra){
  max(muestra)
}

# Probemos como funciona
n <- 50
tita <- 5
X = runif(n,min = 0, max = tita) # me genera la muestra aleatoria de determinado tamaño
mm_tita = f_mm(X)
emv_tita = f_emv(X)

# Pero voy a querer el promedio entre muchas muestras
# Hagamos muchas muestras y veamos que valores da
tita <- 5

N <- 1000 # cantidad de repeticiones del experimento
mm <- c() # voy a guardar los estumadores de momentos
emv <-  c() # idem para EMV
n <- 6

set.seed(42) # seteo una semilla, para que el experimento aleatorio me de el mismo resultado, me tengo q acordar el numero
for (i in 1:N) {
  muestra <- runif(n,min = 0, max = tita)
  mm <- c(mm,f_mm(muestra))
  emv <- c(emv,f_emv(muestra))
}  

par(mfrow = c(2,1))
hist(mm, probability = T)
abline(v = tita, lwd = 2, col = "red")
hist(emv, probability = T)
abline(v = tita, lwd = 2, col = "red")

par(mfrow = c(1,1))
boxplot(mm,emv)
abline(h = tita)

##### Todas las simulaciones ####
set.seed(42)
tita <- 5
N <- 1000
ns <- c(6, 10, 20, 40, 80, 200)
mm <- c()
emv <-  c()
resultados <- data.frame() # guardamos mas info, como en tamaño de muestra
for (n in ns) {
  for (i in 1:N) {
    # Generar muestra
    muestra <- runif(n, min = 0, max = tita)
    # Método de los momentos
    mm <- f_mm(muestra)
    emv <- f_emv(muestra)
    # Guardar resultados
    resultados <- rbind(
      resultados,
      data.frame(
        n = n,
        simulacion = i,
        tita_MM = mm,
        tita_EMV = emv
      )
    )
  }
}

##### ECM muestral ####
ECM_tita_MM <- c()
ECM_tita_EMV <- c()

for (j in 1:length(ns)) {
  # Seleccionar las estimaciones correspondientes a n
  datos_n <- resultados[resultados$n == ns[j], ]
  
  # ECM para tita
  ECM_tita_MM[j] <- mean((datos_n$tita_MM - tita)^2)
  
  ECM_tita_EMV[j] <- mean((datos_n$tita_EMV - tita)^2)
}

plot(ns,ECM_tita_MM, type = "b", col = "red",
     ylab = "ECM",
     xlab = "Tamaño",
     main = "ECM vs Tamaño")
lines(ns,ECM_tita_EMV, type = "b",col = "blue")
legend("topright", legend = c("tita_MM","tita_EMV"), 
       lty = c(1,1),
       pch = c(1,1),
       col = c("red","blue")
)

## ej clase teorica
est1 <- est2 <- 0
n <- 100
set.seed(1001)
mu0<-0
sigma0 <- 1
for(i in 1:10000){
  x <- rnorm(n, mu0, sigma0)
  est1[i] <- mean(x)
  est2[i] <- median(x)
}

plot(est2, ylab="")
points(est1, col=2)
