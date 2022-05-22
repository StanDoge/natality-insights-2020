library("readxl")

datos <- read_excel('./DB nacimientos 2020.xlsx')
sample (datos)

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

#Para mostrar ciertos histogramas
hist(DB_nacimientos_2020$semana_gestacion)

hist(DB_nacimientos_2020$semana_gestacion, main = "Histograma de las semanas de gestación", xlab="Semanas", ylab ="Frecuencia", freq=F)

x <- seq(min(DB_nacimientos_2020$semana_gestacion), max(DB_nacimientos_2020$semana_gestacion), length = length(DB_nacimientos_2020$semana_gestacion))

f <- dnorm(x, mean <- mean(DB_nacimientos_2020$semana_gestacion), sd = sd(DB_nacimientos_2020$semana_gestacion))
lines(x, f, col <- "red", lwd = 2)



