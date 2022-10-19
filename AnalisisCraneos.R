# Ejercicio de Evaluación. Descriptiva e Inferencia.
# Jonás Jiménez Gil
# Ejercicio 1
# Parte A

install.packages(c("readxl", "dplyr", "modeest"))
install.packages(c("quantmod", "tseries", "fImport"))
install.packages("nortest")
install.packages("car")
install.packages("ggplot2")
install.packages("e1071")
install.packages("modeest")
library(readxl)
library(dplyr)
library(modeest)
library(quantmod)
library(tseries)
library(fImport)
library(nortest)
library(car)
library(ggplot2)
library(e1071)
library(modeest)

Libro1 <- read_excel("C:/Users/jjimenez/Desktop/Estadística R/Tarea/Libro1.xlsx")
View(Libro1)

#Creamos dos nuevos dataframes, uno para Predinástico Temprano, y otro para Predinástico Tardío

P2 <- Libro1[Libro1$`Época histórica` == max(Libro1$`Época histórica`),]
P1 <- Libro1[Libro1$`Época histórica` == min(Libro1$`Época histórica`),]

P1.Centralizacion <- P1%>%
  summarise(Mean=mean(P1$'Anchura del cráneo'), Max=max(P1$'Anchura del cráneo'), Min=min(P1$'Anchura del cráneo'), Median=median(P1$'Anchura del cráneo'), Mode=mlv(P1$'Anchura del cráneo', method = "mfv"))
P2.Centralizacion <- P2%>%
  summarise(Mean=mean(P2$'Anchura del cráneo'), Max=max(P2$'Anchura del cráneo'), Min=min(P2$'Anchura del cráneo'), Median=median(P2$'Anchura del cráneo'), Mode=mlv(P2$'Anchura del cráneo', method = "mfv"))

#Realizamos unos gráficos para comparar resultados
barplot(table(P1$`Anchura del cráneo`),
        main = 'Predinástico Temprano',
        sub =  'Anchura del cráneo')

barplot(table(P2$`Anchura del cráneo`),
        main = 'Predinástico Tardío',
        sub =  'Anchura del cráneo')

# Calculamos los cuartiles
P1.Cuartiles <- quantile(P1$'Anchura del cráneo')
print(P1.Cuartiles)
P2.Cuartiles <- quantile(P2$'Anchura del cráneo')
print(P2.Cuartiles)

#Obtenemos unos gráficos Q-Q Plot
qqnorm(P1$`Anchura del cráneo`, pch = 1, frame = FALSE)
qqline(P1$`Anchura del cráneo`, col = "steelblue", lwd = 2)

qqnorm(P2$`Anchura del cráneo`, pch = 1, frame = FALSE)
qqline(P2$`Anchura del cráneo`, col = "steelblue", lwd = 2)

# Calculamos las medidas de dispersión
range(P1$'Anchura del cráneo')
P1.Dispersión <- P1%>%
  summarise(range=max(P1$'Anchura del cráneo')-min(P1$'Anchura del cráneo'), var=var(P1$'Anchura del cráneo'), sd=sd(P1$'Anchura del cráneo'))

print(P1.Dispersión)

range(P2$'Anchura del cráneo')
P2.Dispersión <- P2%>%
  summarise(range=max(P2$'Anchura del cráneo')-min(P2$'Anchura del cráneo'), var=var(P2$'Anchura del cráneo'), sd=sd(P2$'Anchura del cráneo'))

print(P2.Dispersión)

# Asimetría

tapply(Libro1$`Anchura del cráneo`, Libro1$`Época histórica`, 'skewness')

hist(P1$'Anchura del cráneo')
abline(v=131.5333, col="red")
hist(P2$'Anchura del cráneo')
abline(v=132.4666, col="red")

#Calculamos el coeficiente de asimetría de Fisher
skewness(P1$`Anchura del cráneo`, na.rm = TRUE, type = 3)
skewness(P2$`Anchura del cráneo`, na.rm = TRUE, type = 3)

# Curtosis
Curtosis <- tapply(Libro1$`Anchura del cráneo`, Libro1$`Época histórica`, 'kurtosis')
print(Curtosis)

# Diagrama de Cajas y Bigotes

boxplot(Libro1$`Anchura del cráneo` ~ Libro1$`Época histórica`, xlab = "Predinastico temprano, Predinastico tardio",
        ylab = "Anchura", col = c("orange", "blue"))

#Prueba de normalidad de Kolmogorov - Smirnov n>50 y shapiro-wilk n<50

tapply(Libro1$`Anchura del cráneo`, Libro1$`Época histórica`, lillie.test)

tapply(Libro1$`Anchura del cráneo`, Libro1$`Época histórica`, shapiro.test)

#EJERCICIO 2

# Hacemos el test de Levene para la anchura de cráneo

leveneTest(Libro1$`Anchura del cráneo`, Libro1$`Época histórica`)

#Intervalos de confianza 
#90%
t.test(Libro1$`Anchura del cráneo`~ Libro1$`Época histórica`, var.eq = TRUE, conf.int = TRUE, conf.level = 0.90)

#95%
t.test(Libro1$`Anchura del cráneo`~ Libro1$`Época histórica`, var.eq = TRUE, conf.int = TRUE)

#99%
t.test(Libro1$`Anchura del cráneo`~ Libro1$`Época histórica`, var.eq = TRUE, conf.int = TRUE, conf.level = 0.99)

