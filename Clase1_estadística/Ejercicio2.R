buffalo <- scan("buffalo.txt")



hist(buffalo,
     col = "lightblue", # Color claro para ver las líneas
     breaks= seq(20, 130, 10),
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

hist(buffalo,
     col = "lightblue", # Color claro para ver las líneas
     breaks= seq(22, 132, 10),
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

hist(buffalo,
     col = "chocolate", # Color claro para ver las líneas
     breaks= seq(24, 134, 10),
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

hist(buffalo,
     col = "lightblue", # Color claro para ver las líneas
     breaks= seq(10, 130, 5),
     border = "white",                   # Bordes limpios
     main = "Distribución de Valores de Glucosa",
     xlab = "Glucosa (mg/dL)")

# item c
estimar_probabilidad <- function(x, h, datos) {
  # Asegurar que no afecten los valores faltantes (NA)
  datos_limpios <- na.omit(datos)
  
  # Usamos sapply para que funcione si 'x' es un vector de varios valores
  proporciones <- sapply(x, function(val_x) {
    # Definir los límites del intervalo para el x actual
    limite_inferior <- val_x - h
    limite_superior <- val_x + h
    
    # Contar cuántos datos caen dentro del intervalo [x - h, x + h]
    conteo <- sum(datos_limpios >= limite_inferior & datos_limpios <= limite_superior)
    
    # Calcular la proporción (casos favorables / casos totales)
    prob_estimada <- conteo / length(datos_limpios)
    
    return(prob_estimada)
  })
  
  return(proporciones)
}

estimar_probabilidad(80, 16, buffalo)

# item d
resultados1 <- estimar_probabilidad(x = buffalo, h = 10, datos = buffalo)
resultados2 <- estimar_probabilidad(x = buffalo, h = 20, datos = buffalo)
resultados3 <- estimar_probabilidad(x = buffalo, h = 30, datos = buffalo)
print(resultados1)
print(resultados2)
print(resultados3)

# item e
rectangular(54)

rectangular <- function(x) {
  if (x < 1 && x > -1) {
    return(1/2)
  } else {
    return(0)
  }
}

# Prueba:
rectangular(0.5) # Devuelve 0.5
rectangular(2)   # Devuelve 0


print(rectangular(50))

kernel_gaussiano <- function(x){
  dnorm(x)
}

kernel_epanechnikov <- function(u) {
  # Devuelve el cálculo si está entre -1 y 1, de lo contrario devuelve 0
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
}

# item f

kde <- function(datos, h, x_s, K) {
  n <- length(datos)
  f_s <- sapply(x_s, function(x){
    mean(k((x-datos)/h))/h
  })
  }

# item i


