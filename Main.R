library("readxl")
library("plyr")

datasource = read_excel('./DB nacimientos 2020.xlsx')
sample (datasource)

poblation = nrow(datasource)
sample_size = 28000

# Funcion para obtener indices de valores de manera sistematica
systematic.sample = function(N,n){
  k = ceiling(N/n) # intervalo para tomar valores

  r = sample(1:k, 1) # conteo para hacer el "slide"

  sys.samp = seq(r, r + k*(n-1), k) # obtencion de indice de muestras

  # cat("The selected systematic sample is: \"", sys.samp, "\"\n")

}

# Tomando de 3 en 3 se obtienen 8,400 ; cumpliendo el 10% aprox solicitado
systematic_sample = systematic.sample(poblation,sample_size)

# df.sample = datasource[datasource$id==systematic_sample] todo: delimitar el df original de 80,000 -> 8,
