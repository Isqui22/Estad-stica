Debernardi <- read.csv("Debernardi.csv")

# Aca lo escribo como vector
diagnosis <- Debernardi$diagnosis

help(prop.table)

frec_absoluta <- table(diagnosis)
frec_relativa <- prop.table(frec_absoluta)

print(frec_relativa)

# Para combinar ambas tablas
ambas_frecuencias <- cbind(Absoluta = frec_absoluta, Relativa = frec_relativa)

# item b: gráfico de barras
barplot(frec_absoluta)
barplot(frec_relativa)

# más fachero

barplot(
  frec_relativa, 
  main = "Distribución de Diagnósticos",
  xlab = "Tipo de Diagnóstico",
  ylab = "Frecuencia Relativa",
  names.arg = c("Control", "Benigno", "ACPD"), # Nombres claros
  col = c("skyblue", "orange", "firebrick"),
  las = 1,
  ylim = c(0, max(frec_relativa) + 0.1)
)

# veamos otro
install.packages("ggplot2")
library(ggplot2)

# Convertimos la tabla a Data Frame
df_rel <- as.data.frame(frec_relativa)
colnames(df_rel) <- c("Diagnosis", "FrecuenciaRelativa")

# Graficamos con ggplot
ggplot(df_rel, aes(x = Diagnosis, y = FrecuenciaRelativa, fill = Diagnosis)) +
  geom_col(color = "black", width = 0.6) +
  theme_minimal() +
  labs(
    title = "Distribución de Diagnósticos",
    x = "Diagnóstico",
    y = "Frecuencia Relativa"
  ) +
  scale_y_continuous(limits = c(0, max(df_rel$FrecuenciaRelativa) + 0.1)) +
  theme(legend.position = "none") # Ocultamos la leyenda redundant


