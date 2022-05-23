library("readxl")
library(moments)

datos = read_excel('./DB nacimientos 2020.xlsx')

# --------------------------------------- muestra sistematica --------------------------------------------------------
poblacion <- nrow(datos)
n_muestra <- poblacion * 0.1

# Funcion para obtener indices de valores de manera sistematica
muestra.sistematica <- function(N,n){
  k <- ceiling(N/n) # intervalo para tomar valores

  r <- sample(1:k, 1) # conteo para hacer el "slide"

  sys.samp <- seq(r, r + k*(n-1), k) # obtencion de indice de muestras
}

# Tomando de 10 en 10 se obtienen 8,315 ; cumpliendo el 10% aprox solicitado
muestra_sistematica <- datos[muestra.sistematica(poblacion,n_muestra),]

# --------------------------------------- muestra sistematica --------------------------------------------------------

# --------------------------------------- medidas tendencia central --------------------------------------------------
#Medidas de posición      peso_nac
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$peso_nac )
#moda
frecuencias <- data.frame(table(muestra_sistematica$peso_nac))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable peso es", moda)

#Medidas de posición    madre_edad
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$madre_edad)
#moda
frecuencias <- data.frame(table(muestra_sistematica$madre_edad))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable edad de madre  es", moda)

#Medidas de posición    semanas_gestacion
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$semana_gestacion)
#moda
frecuencias <- data.frame(table(muestra_sistematica$semana_gestacion))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable semanas de gestación  es", moda)
# --------------------------------------- medidas tendencia central --------------------------------------------------

# --------------------------------------- medidas dispersion ---------------------------------------------------------
# -- varianza
var_peso <- var(muestra_sistematica$peso_nac)
var_edad <- var(muestra_sistematica$madre_edad)
var_gestacion <- var(muestra_sistematica$semana_gestacion)

# -- desviacion estandar
std_peso <- sqrt(var_peso)
std_edad <- sqrt(var_edad)
std_gestacion <- sqrt(var_gestacion)

# -- coeficiente de variación
CV<-function(x){sd(muestra_sistematica$semana_gestacion)*100/mean(muestra_sistematica$semana_gestacion)}
CV(x)

# -- curtosis

# -- asimetria
# --------------------------------------- medidas dispersion ---------------------------------------------------------

# --------------------------------------- graficas --------------------------------------------------------------------
hist(muestra_sistematica$semana_gestacion)

# hist(muestra_sistematica$semana_gestacion, main="Histograma de las semanas de gestacion", xlab="Semanas",
     # ylab ="Frecuencia", freq=F)

# todo: Dar nombres mas descriptivos para que todos entendamos cual es su finalidad
x <- seq(min(muestra_sistematica$semana_gestacion), max(muestra_sistematica$semana_gestacion),
         length = length(muestra_sistematica$semana_gestacion))

f <- dnorm(x, mean <- mean(muestra_sistematica$semana_gestacion), sd = sd(muestra_sistematica$semana_gestacion))
lines(x, f, col <- "red", lwd = 2)
# --------------------------------------- medidas tendencia central --------------------------------------------------
