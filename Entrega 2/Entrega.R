library(MASS)
library(ggplot2)

# ------------------------------------------------------------------------------
# Parámetros iniciales
# ------------------------------------------------------------------------------
alpha_0 <- 3
lambda_0 <- 4
N <- 1000
tamanios_n <- c(6, 10, 20, 40, 80, 200)

# Fijar semilla para reproducibilidad
set.seed(123)

# Matriz/Dataframe para almacenar los ECM de cada n
ecm_resultados <- data.frame(
  n = integer(),
  ecm_alpha_mom = numeric(),
  ecm_alpha_emv = numeric(),
  ecm_lambda_mom = numeric(),
  ecm_lambda_emv = numeric()
)

# ------------------------------------------------------------------------------
# Bucle principal sobre cada tamaño muestral n
# ------------------------------------------------------------------------------
for (n in tamanios_n) {
  
  # Vectores para guardar las N estimaciones
  alpha_mom <- numeric(N)
  lambda_mom <- numeric(N)
  alpha_emv <- numeric(N)
  lambda_emv <- numeric(N)
  
  for (i in 1:N) {
    # i) Generar muestra aleatoria de tamaño n de Gamma(3, 4)
    muestra <- rgamma(n, shape = alpha_0, rate = lambda_0)
    
    # 1) Estimador de Momentos
    x_bar <- mean(muestra)
    s2 <- var(muestra)
    
    lambda_mom[i] <- x_bar / s2
    alpha_mom[i]  <- (x_bar^2) / s2
    
    # 2) Estimador de Máxima Verosimilitud usando fitdistr
    # Pasamos los estimadores de momentos como valores iniciales (start)
    fit <- fitdistr(muestra, "gamma", start = list(shape = alpha_mom[i], rate = lambda_mom[i]))
    
    alpha_emv[i]  <- fit$estimate["shape"]
    lambda_emv[i] <- fit$estimate["rate"]
  }
  
  # ----------------------------------------------------------------------------
  # i) Visualizaciones (Histogramas y Boxplots paralelos)
  # ----------------------------------------------------------------------------
  par(mfrow = c(2, 2))
  
  # Boxplots paralelos para alpha
  boxplot(list(Momentos = alpha_mom, EMV = alpha_emv), 
          main = paste0("Estimaciones de Alpha (n = ", n, ")"),
          col = c("skyblue", "lightgreen"))
  abline(h = alpha_0, col = "red", lty = 2, lwd = 2)
  
  # Boxplots paralelos para lambda
  boxplot(list(Momentos = lambda_mom, EMV = lambda_emv), 
          main = paste0("Estimaciones de Lambda (n = ", n, ")"),
          col = c("skyblue", "lightgreen"))
  abline(h = lambda_0, col = "red", lty = 2, lwd = 2)
  
  # Histograma de Alpha
  hist(alpha_emv, main = paste0("EMV Alpha (n = ", n, ")"), xlab = "alpha", col = "lightgreen")
  abline(v = alpha_0, col = "red", lwd = 2)
  
  # Histograma de Lambda
  hist(lambda_emv, main = paste0("EMV Lambda (n = ", n, ")"), xlab = "lambda", col = "lightgreen")
  abline(v = lambda_0, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1)) # Restaurar panel gráfico
  
  # ----------------------------------------------------------------------------
  # ii) Cómputo del ECM muestral
  # ----------------------------------------------------------------------------
  ecm_a_mom <- mean((alpha_mom - alpha_0)^2)
  ecm_a_emv <- mean((alpha_emv - alpha_0)^2)
  
  ecm_l_mom <- mean((lambda_mom - lambda_0)^2)
  ecm_l_emv <- mean((lambda_emv - lambda_0)^2)
  
  # Guardar fila de resultados
  ecm_resultados <- rbind(ecm_resultados, data.frame(
    n = n,
    ecm_alpha_mom = ecm_a_mom,
    ecm_alpha_emv = ecm_a_emv,
    ecm_lambda_mom = ecm_l_mom,
    ecm_lambda_emv = ecm_l_emv
  ))
}

# Mostrar la tabla de ECM
print("Resultados de ECM Muestrales:")
print(ecm_resultados)

# ------------------------------------------------------------------------------
# Gráfico final de n vs ECM muestral
# ------------------------------------------------------------------------------
# Gráfico general
ggplot(ecm_resultados, aes(x = n)) +
  geom_line(aes(y = ecm_alpha_mom, color = "Alpha - Momentos"), size = 1) +
  geom_point(aes(y = ecm_alpha_mom, color = "Alpha - Momentos"), size = 2) +
  geom_line(aes(y = ecm_alpha_emv, color = "Alpha - EMV"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ecm_alpha_emv, color = "Alpha - EMV"), size = 2) +
  geom_line(aes(y = ecm_lambda_mom, color = "Lambda - Momentos"), size = 1) +
  geom_point(aes(y = ecm_lambda_mom, color = "Lambda - Momentos"), size = 2) +
  geom_line(aes(y = ecm_lambda_emv, color = "Lambda - EMV"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ecm_lambda_emv, color = "Lambda - EMV"), size = 2) +
  labs(title = "Evolución del ECM según el tamaño muestral (n)",
       x = "Tamaño muestral (n)", y = "ECM Muestral") +
  theme_minimal()

# Zoom para n >= 20 (donde las estimaciones no explotan por variabilidad de n bajo)
ggplot(subset(ecm_resultados, n >= 20), aes(x = n)) +
  geom_line(aes(y = ecm_alpha_mom, color = "Alpha - Momentos"), size = 1) +
  geom_point(aes(y = ecm_alpha_mom, color = "Alpha - Momentos"), size = 2) +
  geom_line(aes(y = ecm_alpha_emv, color = "Alpha - EMV"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ecm_alpha_emv, color = "Alpha - EMV"), size = 2) +
  geom_line(aes(y = ecm_lambda_mom, color = "Lambda - Momentos"), size = 1) +
  geom_point(aes(y = ecm_lambda_mom, color = "Lambda - Momentos"), size = 2) +
  geom_line(aes(y = ecm_lambda_emv, color = "Lambda - EMV"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ecm_lambda_emv, color = "Lambda - EMV"), size = 2) +
  labs(title = "Zoom: ECM según n (n >= 20)",
       x = "Tamaño muestral (n)", y = "ECM Muestral") +
  theme_minimal()