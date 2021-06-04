library(plyr)

setwd("C:/Users/Damarlazo/Documents/ALVARO/MasterCICE/TFM/Accidentes")

df <- read.csv("accidentes_estado_meteorologico.csv", sep = ";", stringsAsFactors = T)
summary(df)

# Número de filas
nrow(df)

# Número de accidentes
length(unique(df$NUMERO.PARTE))

# Total de víctimas (tarda mucho en ejecutarse)
victimas = 0
for (i in 1:nrow(df)) {
  if (duplicated(df$NUMERO.PARTE)[i] == FALSE) {
    print(i)
    victimas = victimas + df$NUMERO.VICTIMAS[i]
  }
}
victimas

summary(df)

# TEMPERATURA MEDIA
par(mfrow=c(1,2))
hist(df$TEMPERATURA.MEDIA, main = "Histograma", xlab = "TEMPERATURA MEDIA",
     ylab = "Frecuencia", col = "orange")
boxplot(df$TEMPERATURA.MEDIA, main = "Boxplot", col = "orange")
summary(df$TEMPERATURA.MEDIA)

# PRECIPITACIÓN
par(mfrow=c(1,2))
hist(df$PRECIPITACION, main = "Histograma", xlab = "PRECIPITACION",
     ylab = "Frecuencia", col = "aquamarine")
boxplot(df$PRECIPITACION, main = "Boxplot", col = "aliceblue")
summary(df$PRECIPITACION)

# VELOCIDAD MEDIA VIENTO
par(mfrow=c(1,2))
hist(df$VELOCIDAD.MEDIA.VIENTO, main = "Histograma",
     xlab = "VELOCIDAD MEDIA VIENTO", ylab = "Frecuencia", col = "grey")
boxplot(df$VELOCIDAD.MEDIA.VIENTO, main = "Boxplot")
summary(df$VELOCIDAD.MEDIA.VIENTO)

library(PerformanceAnalytics)
par(mfrow=c(1,1))
chart.Correlation(data_cor, histogram = TRUE, method = "pearson")
