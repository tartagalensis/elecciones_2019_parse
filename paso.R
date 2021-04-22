# PARSE PASO 2019 - electorAR
# Author: Franco Galeano
# Date: 22/04/2020

# Paquetes ####
library(tidyverse)


# Read Data ####
descripcion_postulaciones <- read_delim("data/escrutinio-definitivo-PASO2019/descripcion_postulaciones.csv", delim = "|") %>% 
  print()

distritos <- read_csv2("data/escrutinio-definitivo-PASO2019/distritos.csv") %>% 
  print()

agrupaciones <- read_csv2("data/escrutinio-definitivo-PASO2019/mesas-agrupaciones.csv") %>% 
  print()

blancos_nulos <- read_csv2("data/escrutinio-definitivo-PASO2019/mesas-blancosnulos.csv") %>% 
  print()

mesas_listas <- read_csv2("data/escrutinio-definitivo-PASO2019/mesas-listas.csv") %>% 
  print()

secciones <- read_csv2("data/escrutinio-definitivo-PASO2019/secciones.csv") %>% 
  print()

# Data Wrangling ####

# Export ####
