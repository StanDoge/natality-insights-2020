library("readxl")
library("plyr")

datasource = read_excel('./DB nacimientos 2020.xlsx')
sample (datasource)

poblation = nrow(datasource)
sample_size = 8315

# Funcion para obtener indices de valores de manera sistematica
systematic.sample = function(N,n){
  k = ceiling(N/n) # intervalo para tomar valores

  r = sample(1:k, 1) # conteo para hacer el "slide"

  sys.samp = seq(r, r + k*(n-1), k) # obtencion de indice de muestras
}

# Tomando de 10 en 10 se obtienen 8,315 ; cumpliendo el 10% aprox solicitado
systematic_sample = datasource[systematic.sample(poblation,sample_size),]

#Para mostrar ciertos histogramas
hist(DB_nacimientos_2020$semana_gestacion)

hist(DB_nacimientos_2020$semana_gestacion, main="Histograma de las semanas de gestación", xlab="Semanas", ylab ="Frecuencia", freq=F)

x <- seq(min(DB_nacimientos_2020$semana_gestacion), max(DB_nacimientos_2020$semana_gestacion), length = length(DB_nacimientos_2020$semana_gestacion))

f <- dnorm(x, mean = mean(DB_nacimientos_2020$semana_gestacion), sd = sd(DB_nacimientos_2020$semana_gestacion))
lines(x, f, col = "red", lwd = 2)

#-------------------------------------------------------
#Medidas de posición      peso_nac
#1 Qu. media 3Qu. Mediana 
summary(DB_nacimientos_2020$peso_nac)
#moda
frecuencias <- data.frame(table(DB_nacimientos_2020$peso_nac))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable peso es", moda)

####longitud nac #### variable no encontrada

#Medidas de posición    madre_edad
#1 Qu. media 3Qu. Mediana 
summary(DB_nacimientos_2020$madre_edad)
#moda
frecuencias <- data.frame(table(DB_nacimientos_2020$madre_edad))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable edad de madre  es", moda)

#Medidas de posición    semanas_gestacion
#1 Qu. media 3Qu. Mediana 
summary(DB_nacimientos_2020$semana_gestacion)
#moda
frecuencias <- data.frame(table(DB_nacimientos_2020$semana_gestacion))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable semanas de gestación  es", moda)
#-------------------------------------------------------





