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

