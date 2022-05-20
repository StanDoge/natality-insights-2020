library("readxl")
library("plyr")

datasource = read_excel('./DB nacimientos 2020.xlsx')
sample (datasource)

sampling_interval = 2
poblation = nrow(datasource)

# Funcion para obtener indices de valores de manera sistematica
systematic.sample = function(N,n){
  k = ceiling(N/n) # intervalo para tomar valores

  r = sample(1:k, 1) # conteo para hacer el "slide"

  sys.samp = seq(r, r + k*(n-1), k) # obtencion de indice de muestras
}

# Tomando de 3 en 3 se obtienen 8,400 ; cumpliendo el 10% aprox solicitado
sample_size = 28000

sys.samp = seq(sample_index, sample_index + k*(sample_size - 1), k)

systematic_sample = systematic.sample(poblation,sample_size)



