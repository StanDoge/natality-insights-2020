library("readxl")
library("plyr")
datasource = read_excel('./DB nacimientos 2020.xlsx')
sample (datasource)

sampling_interval = 2
poblation = data.frame(table(datasource))

# Tomando de 2 en 2 se obtienen 8,000 ; cumpliendo el 10% aprox solicitado
sample_size = 20000

# k = ceiling(poblation/sample_size)