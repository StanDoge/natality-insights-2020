library("readxl")

datos <- read_excel('./DB nacimientos 2020.xlsx')

# --------------------------------------- muestra sistematica --------------------------------------------------------
poblacion <- nrow(datos)
n_muestra <- 8315

# Funcion para obtener indices de valores de manera sistematica
systematic.sample <- function(N,n){
  k <- ceiling(N/n) # intervalo para tomar valores

  r <- sample(1:k, 1) # conteo para hacer el "slide"

  sys.samp <- seq(r, r + k*(n-1), k) # obtencion de indice de muestras
}

# Tomando de 10 en 10 se obtienen 8,315 ; cumpliendo el 10% aprox solicitado
muestra_sistematica <- datos[systematic.sample(poblacion,n_muestra),]

# --------------------------------------- muestra sistematica --------------------------------------------------------


# --------------------------------------- medidas tendencia central --------------------------------------------------

# Medidas de tendencia central

# Para mostrar ciertos histogramas
hist(muestra_sistematica$semana_gestacion)

hist(muestra_sistematica$semana_gestacion, main="Histograma de las semanas de gestacion", xlab="Semanas",
     ylab ="Frecuencia", freq=F)

x <- seq(min(muestra_sistematica$semana_gestacion), max(muestra_sistematica$semana_gestacion),
         length = length(muestra_sistematica$semana_gestacion))

f <- dnorm(x, mean <- mean(muestra_sistematica$semana_gestacion), sd = sd(muestra_sistematica$semana_gestacion))
lines(x, f, col <- "red", lwd = 2)

# --------------------------------------- medidas tendencia central --------------------------------------------------


# --------------------------------------- medidas dispersion ---------------------------------------------------------
# --------------------------------------- medidas dispersion ---------------------------------------------------------


# --------------------------------------- graficas --------------------------------------------------------------------
# --------------------------------------- graficas---------------------------------------------------------------------

