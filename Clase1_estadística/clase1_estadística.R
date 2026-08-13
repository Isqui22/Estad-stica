tabla <- read.table("dietas.txt", header = TRUE)
tabla

datos_dieta <- function(n){
  media <- mean(tabla[[n]], na.rm = TRUE)
  mediana <- median(tabla[[n]], na.rm = TRUE)
  media_alpha1 <- mean(tabla[[n]], trim = 0.1, na.rm = TRUE)
  media_alpha2 <- mean(tabla[[n]], trim = 0.2, na.rm = TRUE)
  
  # Agrupamos todo en una lista con etiquetas
  resultado <- list(
    Media = media,
    Mediana = mediana,
    Media_Trim_10 = media_alpha1,
    Media_Trim_20 = media_alpha2
  )
  
  return(resultado)
}

# Ejecutar la función
dieta1 <- datos_dieta(1)
print(dieta1)

dieta2 <- datos_dieta(2)
print(dieta2)

dieta3 <- datos_dieta(3)
print(dieta3)

# la dieta c parece ser la única fuera de los valores recomendados

# item 2
desvio_estandar_1 <- sd(tabla[[1]]) 
desvio_estandar_2 <- sd(tabla[[2]])
desvio_estandar_3 <- sd(tabla[[3]]) 

dist_intercuartil_1 <- IQR(tabla[[1]]) 
dist_intercuartil_2 <- IQR(tabla[[2]])
dist_intercuartil_3 <- IQR(tabla[[3]]) 

MAD1 <- mad(tabla[[1]]) 
MAD2 <- mad(tabla[[2]])
MAD3 <- mad(tabla[[3]]) 

cat(desvio_estandar_1, dist_intercuartil_1, MAD1)
cat(desvio_estandar_2, dist_intercuartil_2, MAD2)
cat(desvio_estandar_3, dist_intercuartil_3, MAD3)

# se podria decir que a mayor dispersion, menos establilidad

# item c
percentil_dieta1 <- quantile(tabla[[1]], probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
percentil_dieta2 <- quantile(tabla[[2]], probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
percentil_dieta3 <- quantile(tabla[[3]], probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

# item d
dieta1 <- hist(tabla[[1]], colour(red))


hist(tabla[[1]],
     col = "lightblue",                  # Color claro para ver las líneas
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

hist(tabla[[2]],
     col = "lightblue",                  # Color claro para ver las líneas
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

hist(tabla[[3]],
     col = "lightblue",                  # Color claro para ver las líneas
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

# Ejercicio e
boxplot(tabla[[1]],
        main = "Distribución de Glucosa Total",
        ylab = "Glucosa (mg/dL)",
        col = "springgreen3",
        border = "darkgreen",
        las = 1)

boxplot(tabla[[2]],
        main = "Distribución de Glucosa Total",
        ylab = "Glucosa (mg/dL)",
        col = "springgreen3",
        border = "darkgreen",
        las = 1)

boxplot(tabla[[3]],
        main = "Distribución de Glucosa Total",
        ylab = "Glucosa (mg/dL)",
        col = "springgreen3",
        border = "darkgreen",
        las = 1)

# item f
# 1. Dibuja los puntos (Cuantiles teóricos vs. Cuantiles de tus datos)
qqnorm(tabla[[1]], 
       main = "QQ-Plot de Glucosa", 
       xlab = "Cuantiles Teóricos (Norm)", 
       ylab = "Cuantiles de los Datos")

# 2. Añade la línea recta de referencia que pasa por el primer y tercer cuartil
qqline(tabla[[1]], 
       col = "red")

### 2do grafico
qqnorm(tabla[[2]], 
       main = "QQ-Plot de Glucosa", 
       xlab = "Cuantiles Teóricos (Norm)", 
       ylab = "Cuantiles de los Datos")

qqline(tabla[[2]], 
       col = "red")

### 3er grafico
qqnorm(tabla[[3]], 
       main = "QQ-Plot de Glucosa", 
       xlab = "Cuantiles Teóricos (Norm)", 
       ylab = "Cuantiles de los Datos")

qqline(tabla[[3]], 
       col = "red")

# la primera es la dieta más adecuada