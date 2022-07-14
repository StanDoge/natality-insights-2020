library("readxl")
library(moments)

# --------------------------------------- muestra sistematica --------------------------------------------------------
muestra_sistematica <- read_excel('./muestra_sistematica.xlsx')
# --------------------------------------- muestra sistematica --------------------------------------------------------

# --------------------------------------- medidas tendencia central --------------------------------------------------

#1 Qu. media 3Qu. Mediana
summary(muestra_sistematica$peso_nac )
#moda
frecuencias <- data.frame(table(muestra_sistematica$peso_nac))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable peso es", moda)

#1 Qu. media 3Qu. Mediana
summary(muestra_sistematica$madre_edad)
#moda
frecuencias <- data.frame(table(muestra_sistematica$madre_edad))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable edad de madre  es", moda)

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
  n<-1
  salida<-list()
  for (i in lista){
    salida[[n]]<-funcion(i)
    n<-n + 1
}
  return(salida)
}

# -- varianza
varianza<-list()

varianza<-loop(lista=valores_de_estudio,funcion=var)

# -- desviacion estandar
desviacion<-list()

desviacion<-loop(varianza,funcion=sqrt)

# -- coeficiente de variación
CV<-function(x){sd(muestra_sistematica$semana_gestacion)*100/mean(muestra_sistematica$semana_gestacion)}
CV(x)

# -- asimetria
asimetria<-list()
asimetria<-loop(valores_de_estudio,skewness)

# -- curtosis
curtosis<-list()
curtosis<-loop(valores_de_estudio,kurtosis)

# -- rango
ra_peso <- max(muestra_sistematica$peso_nac) - min(muestra_sistematica$peso_nac)
ra_edad <- max(muestra_sistematica$madre_edad) - min(muestra_sistematica$madre_edad)
ra_gestacion <- max(muestra_sistematica$semana_gestacion) - min(muestra_sistematica$semana_gestacion)
ra_talla <- max(muestra_sistematica$talla_nac) - min(muestra_sistematica$talla_nac)

rangos <- c(ra_peso,ra_edad,ra_gestacion,ra_talla)

# -- df de valores de dispersion
valores_dispersion <- data.frame( unlist(varianza),unlist(desviacion),unlist(asimetria),unlist(curtosis),
                                  rangos,row.names=c("peso","edad","gestacion","talla"))
# --------------------------------------- medidas dispersion ---------------------------------------------------------

# --------------------------------------- graficas --------------------------------------------------------------------
hist(muestra_sistematica$peso_nac, main="Distribucion de los pesos de los recien nacidos", xlab="Kilogramos"
,ylab="Conteo",breaks = 20)

hist(muestra_sistematica$madre_edad, main="Distribucion de la edad de las madres de los recien nacidos", xlab="Edades"
  ,ylab="Conteo",breaks = 20)

hist(muestra_sistematica$semana_gestacion, main="Distribucion de las semanas de gesticion para los recien nacidos"
  , xlab="Semanas" ,ylab="Conteo",breaks = 20)

hist(muestra_sistematica$talla_nac, main="Distribucion de tallas en los recien nacidos", xlab="Tallas en cm"
  ,ylab="Conteo",breaks = 20)

# --------------------------------------- intervalos de confianza ---------------------------------------------------
madres <- muestra_sistematica$madre_edad
t.test(madres,conf.level = 0.95)

semanas <- muestra_sistematica$semana_gestacion
t.test(semanas,conf.level = 0.95)

peso <- muestra_sistematica$peso_nac
t.test(peso,conf.level = 0.95)

talla <- muestra_sistematica$talla_nac
t.test(talla,conf.level = 0.95)
# --------------------------------------- intervalos de confianza ---------------------------------------------------

# --------------------------------------- probabilidades ---------------------------------------------------

#-- simple featuring scaling
s_normalizacion = function (x){
    return (x/max(x))
}

#-- normalizando datos
variables_estudio_norm = data.frame(lapply(valores_de_estudio,s_normalizacion))[0:4]
variables_estudio_norm = setNames(variables_estudio_norm, c('Peso','Edad','Semanas','Talla'))
# --------------------------------------- probabilidades ---------------------------------------------------