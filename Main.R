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
valores_de_estudio <- list(muestra_sistematica$peso_nac,muestra_sistematica$madre_edad,
                          muestra_sistematica$semana_gestacion,muestra_sistematica$talla_nac)

# -- funcion loop; dado un arreglo y una aplica esa funcion a cada uno de los elementos del arreglo y devuelve uno
#    nuevo con los elementos pasados por la funcion ~ simil a un forEach.
loop <- function (lista,funcion){
  n = 1
  salida = list()
  for (i in lista){
    salida[[n]] = funcion(i)
    n = n + 1
}
  return(salida)
}

# -- varianza
varianza = list()

varianza = loop(lista=valores_de_estudio,funcion=var)

# -- desviacion estandar
desviacion = list()

desviacion = loop(varianza,funcion=sqrt)

# -- coeficiente de variación
CV<-function(x){sd(muestra_sistematica$semana_gestacion)*100/mean(muestra_sistematica$semana_gestacion)}
CV(x)

# -- asimetria
asimetria = list()
asimetria = loop(valores_de_estudio,skewness)

# -- curtosis
curtosis = list()
curtosis = loop(valores_de_estudio,kurtosis)

# -- rango
ra_peso <- max(muestra_sistematica$peso_nac) - min(muestra_sistematica$peso_nac)
ra_edad <- max(muestra_sistematica$madre_edad) - min(muestra_sistematica$madre_edad)
ra_gestacion <- max(muestra_sistematica$semana_gestacion) - min(muestra_sistematica$semana_gestacion)
ra_talla <- max(muestra_sistematica$talla_nac) - min(muestra_sistematica$talla_nac)

rangos <- c(ra_peso,ra_edad,ra_gestacion,ra_talla)

# -- df de valores de dispersion
valores_dispersion <- data.frame( unlist(varianza),unlist(desviacion),unlist(asimetria),unlist(curtosis),
                                  rangos,row.names = c("peso","edad","gestacion","talla"))
# --------------------------------------- medidas dispersion ---------------------------------------------------------

# --------------------------------------- graficas --------------------------------------------------------------------
# hist(muestra_sistematica$semana_gestacion)

# hist(muestra_sistematica$semana_gestacion, main="Histograma de las semanas de gestacion", xlab="Semanas",
     # ylab ="Frecuencia", freq=F)

# todo: Dar nombres mas descriptivos para que todos entendamos cual es su finalidad
# x <- seq(min(muestra_sistematica$semana_gestacion), max(muestra_sistematica$semana_gestacion),
#          length = length(muestra_sistematica$semana_gestacion))
#
# f <- dnorm(x, mean = mean(muestra_sistematica$semana_gestacion), sd = sd(muestra_sistematica$semana_gestacion))
# lines(x, f, col = "red", lwd = 2)
# --------------------------------------- medidas tendencia central --------------------------------------------------
