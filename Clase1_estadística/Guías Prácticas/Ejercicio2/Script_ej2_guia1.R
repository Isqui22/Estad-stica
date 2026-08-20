# Por la cantidad de datos sabemos que hubo 891 pasajeros no tripulantes

titanic <- read.csv("datos_titanic.csv")

sexo <- titanic$Sex # ES SENSIBLE A LAS MAYUS
print(sexo)

# quiero contar el total de mujeres y las que sobrevivieron
mujeres <- sum(titanic$Sex == "female")
muj_superv <- sum(titanic$Survived == 1 & titanic$Sex == "female", na.rm = TRUE)

# luego la probabilidad de sobrevivir siendo mujer será el numero de mujeres que sobrevivieron sobre las mujeres totales
prob_sobrev_mujer <- muj_superv/mujeres
print(prob_sobrev_mujer)

# calculo la probabilidad de ser mujer
prob_ser_mujer <- mujeres/891
print(prob_ser_mujer)

# calculo la prob de sobrevivir
nro_supervivientes <- sum(titanic$Survived == 1)
prob_sobrevivir <- nro_supervivientes/891

# Luego por Bayes la probabilidad de ser mujer sabiendo que sobrevivió será
(prob_sobrev_mujer*prob_ser_mujer)/prob_sobrevivir
