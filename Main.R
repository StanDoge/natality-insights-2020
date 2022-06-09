library("readxl")
library(moments)
install.packages("psych")
library(psych)

# datos<-read_excel('./DB nacimientos 2020.xlsx')

# --------------------------------------- muestra sistematica --------------------------------------------------------
muestra_sistematica <- read_excel('./muestra_sistematica.xlsx')
# --------------------------------------- muestra sistematica --------------------------------------------------------

# --------------------------------------- medidas tendencia central --------------------------------------------------
#Medidas de posición      peso_nac
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$peso_nac)
geometric.mean(muestra_sistematica$peso_nac)
armonic<-1/mean(1/muestra_sistematica$peso_nac)
armonic
#moda
frecuencias <- data.frame(table(muestra_sistematica$peso_nac))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable peso es", moda)

#Medidas de posición    madre_edad
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$madre_edad)
geometric.mean(muestra_sistematica$madre_edad)
armonic<-1/mean(1/muestra_sistematica$madre_edad)
armonic
#moda
frecuencias <- data.frame(table(muestra_sistematica$madre_edad))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable edad de madre  es", moda)

#Medidas de posición    semanas_gestacion
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$semana_gestacion)
geometric.mean(muestra_sistematica$semana_gestacion)
armonic<-1/mean(1/muestra_sistematica$semana_gestacion)
armonic

#moda
frecuencias <- data.frame(table(muestra_sistematica$semana_gestacion))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable semanas de gestación  es", moda)

#Medidas de posición    talla_nac
#1 Qu. media 3Qu. Mediana 
summary(muestra_sistematica$talla_nac)
geometric.mean(muestra_sistematica$talla_nac)
armonic<-1/mean(1/muestra_sistematica$talla_nac)
armonic
#moda
frecuencias <- data.frame(table(muestra_sistematica$talla_nac))
moda <- frecuencias[which.max(frecuencias$Freq),1]
paste("La moda de la variable talla de nacimiento  es", moda)
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

boxplot(muestra_sistematica$peso_nac, main="Peso de recien nacido")
boxplot(muestra_sistematica$madre_edad, main="Edad de la madre del recien nacido")
boxplot(muestra_sistematica$semana_gestacion, main="Semanas de embarazo de la madre del recien nacido ")
boxplot(muestra_sistematica$talla_nac, main="Talla del recien nacido en cm")


#--------------------------------------Distribuciones conjuntas de probabilidad-------------------------------------------------------------------

# local_parto
table(muestra_sistematica$local_parto,muestra_sistematica$sex_nac)
table(muestra_sistematica$local_parto,muestra_sistematica$tipo_parto)
table(muestra_sistematica$local_parto,muestra_sistematica$clase_parto)

#madre_dept
table(muestra_sistematica$madre_dept,muestra_sistematica$sex_nac)
table(muestra_sistematica$madre_dept,muestra_sistematica$madre_area)

#tipo_parto
table(muestra_sistematica$tipo_parto,muestra_sistematica$sex_nac)

#clase_parto
table(muestra_sistematica$clase_parto,muestra_sistematica$sex_nac)


# -------------------------------------Distribuciones conjuntas de probabilidad-----------------------------------------------

# -------------------------------------Inferencia sobre medias de dos muestras-----------------------------------------------
table(muestra_sistematica$sex_nac)
mean((x))



# -------------------------------------Inferencia sobre medias de dos muestras-----------------------------------------------


