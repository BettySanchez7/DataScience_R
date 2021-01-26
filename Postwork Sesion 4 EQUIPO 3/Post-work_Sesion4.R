#Post-work Sesión 4 _ Equipo 3
 #FTHG: Full-time Home Goals  FTAG: Full-time Away Goals   FTR: Full-time Result
library(dplyr)
library(ggplot2)
library(rsample)

football <- read.csv("football.csv")

#Probabilidad marginal goles casa
x <- football$FTHG
goles_casa <- as.data.frame(table(x),responseName = "Frec_abs")
goles_casa$Probabilidad <- round(goles_casa$Frec_abs/dim(football)[1],4) #Frec_relativa = Probabilidad

#Probabilidad marginal goles visitante
y <- football$FTAG
goles_visitante <- as.data.frame(table(y),responseName = "Frec_abs")
goles_visitante$Probabilidad <- round(goles_visitante$Frec_abs/dim(football)[1],4) #Frec_relativa = Probabilidad

#Probabilidad conjunta
goles_conjunta <- as.data.frame(table(x,y),responseName = "Frec_abs")
goles_conjunta$Probabilidad <- round(goles_conjunta$Frec_abs/dim(football)[1],4) #Frec_relativa = Probabilidad

#Obteniendo tabla de cocientes
goles_conjunta <- merge(goles_conjunta,goles_casa,by="x", suffixes = c("_conjunta","_casa"))
goles_conjunta <- mutate(goles_conjunta, Frec_abs_conjunta=NULL,Frec_abs_casa=NULL)
goles_conjunta <- merge(goles_conjunta,goles_visitante,by="y")
goles_conjunta <- rename(goles_conjunta,Probabilidad_visitante = Probabilidad)
goles_conjunta <- mutate(goles_conjunta, Cociente=Probabilidad_conjunta/(Probabilidad_casa*Probabilidad_visitante),Frec_abs=NULL)
goles_conjunta <- relocate(goles_conjunta,x) #Reordenando columnas: x primero
goles_conjunta <- arrange(goles_conjunta,y,x) #Ordenando filas por valores de x,y

#Utilzando bootstrap para generar 50 muestras del mismo tamaño que goles_conjunta
muestras_boot <- bootstraps(goles_conjunta,times = 50)
muestras_boot <- lapply(muestras_boot$splits,as.data.frame)
muestras_boot <- lapply(muestras_boot,select,Cociente)
cocientes <- do.call(rbind,muestras_boot) #Guardando todos los cocientes en un df

#Haciendo un histograma para observar la distribución de los cocientes
cocientes %>%
  ggplot(aes(Cociente)) + 
  geom_histogram(binwidth = 0.25, col="black", fill = "#27ae60") + 
  ggtitle("Distribución de cocientes") +
  ylab("Frecuencia") +
  xlab("Cocientes") + 
  theme_light()

# Lo primero que se observa de la distribución es que tiene un máximo para los cocientes
# iguales a cero, que corresponden a los casos en los que la probabilidad conjunta es cero
# (no existió esa combinación de goles) y por lo tanto no es posible comparar con el producto
# de las probabilidades marginales.
# 
# Descartando esos casos, porque no aportan información sobre la independencia de las variables,
# observamos que la distribución tiene un pico para los cocientes iguales y cercanos a uno. Esto
# nos permite afirmar que las variables aleatorias X y Y son independientes.
