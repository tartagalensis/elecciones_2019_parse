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

electores_gral <- read_csv2("data/escrutinio-definitivo-Generales2019/mesas-electores.csv") %>% 
  print()


# Filtering - Presidencial ####

votos_agrupaciones_presidenciales <- agrupaciones %>% 
  filter(categoria == "000100000000000") %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  spread(agrupacion, votos) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  ungroup() %>% 
  print()

votos_blancos_nulos_presidenciales <- blancos_nulos %>% 
  filter(categoria == "000100000000000") %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  spread(contador, votos) %>% 
  rename("blancos" = `VOTOS EN BLANCO`,
         "nulos" = `VOTOS NULOS`,
         "codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  select(-categoria) %>% 
  ungroup() %>% 
  print()
  
electores_presidenciales <- electores_gral %>% 
  filter(categoria == "000100000000000") %>% 
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()


modelo <- read_csv("data/arg_presi_paso2019 copy.csv") %>% 
  print()

paso_2019 <- electores_presidenciales %>% 
  left_join(votos_blancos_nulos_presidenciales) %>% 
  drop_na(nulos) %>% 
  left_join(votos_agrupaciones_presidenciales) %>% 
  select(-categoria) %>% 
  rename( "00002" = `133`,
          "00010" = `131`,
          "00011" = `132`,
          "00008" = `13`,
          "00009" = `135`,
          "00005" = `136`,
          "00001" = `137`,
          "00050" = `36`,
          "00051" = `57`,
          "00004" = `87`,
          "electores" = "votos") %>% 
  print()
  

#cf -> 00001 / 137
#FIT -> 00008 / 133
#FDT -> 00005 / 136
# NOS -> 00010 / 131
# frente patriota -> 00011 / 132
# jxc -> 00009 / 135
# mas -> 00002 / 13
# mav -> 00051 / 57
# pan -> 00050 / 36
# unite -> 00004 / 87


paso_2019 %>% 
  filter(codprov == 24) %>% 
  select(-circuito, -mesa) %>% 
  #group_by(codprov, coddepto) %>% 
  summarise(across(votos:`00004`, sum))

nom_deptos <- modelo %>%
  select(codprov, coddepto, depto) %>% 
  unique() %>% 
  print()

paso_2019 %>% 
  left_join(nom_deptos) %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  write_csv('data/definitivos/arg_presi_paso2019.csv')

            