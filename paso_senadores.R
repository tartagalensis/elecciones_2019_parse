# PARSE PASO Senadores 2019 - electorAR
# Author: Franco Galeano
# Date: 26/04/2020

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
  rename("codprov" = "distrito",
         "coddepto" = "seccion",
         "depto" = "nombre_seccion") %>% 
  print()

electores_gral <- read_csv2("data/escrutinio-definitivo-Generales2019/mesas-electores.csv") %>% 
  print()


# CATEGORIAS ####
# CABA -> CAT 000201000000000
# CHACO -> CAT 000206000000000
# ER -> CAT 000208000000000
# NEUQUEN -> 	CAT 000215000000000
# RIO NEGRO -> 	CAT 000216000000000
# SALTA -> CAT 000217000000000
# SANTIAGO -> CAT 000222000000000
# TDF -> 000224000000000

# CABA ####
# Agrupaciones
votos_agrupaciones_sencaba <- agrupaciones %>% 
  filter(categoria == "000201000000000") %>% 
  group_by(distrito, seccion, circuito, mesa, categoria, agrupacion) %>% 
  distinct(distrito, seccion, circuito, mesa, categoria, agrupacion,.keep_all = TRUE) %>% 
  ungroup() %>% 
  group_by(distrito, seccion, circuito, mesa, categoria) %>% 
  spread(agrupacion, votos) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  ungroup() %>% 
  select(-categoria) %>% 
  print()

# Blancos Nulos
votos_blancos_nulos_sencaba <- blancos_nulos %>% 
  filter(categoria == "000201000000000") %>% 
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

# Electores
electores_sencaba <- electores_gral %>% 
  filter(categoria == "000201000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000201000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "01")

# Join
sen_caba <- electores_sencaba %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_sencaba) %>% 
  left_join(votos_agrupaciones_sencaba) %>% 
  rename( "00001" = `01-504`,
          "00002" = `13`,
          "00055" = `01-262`,
          "00003" = `5`,
          "00004" = `87`,
          "00005" = `01-502`,
          "00056" = `01-343`,
          "00006" = `88`,
          "00007" = `01-187`,
          "00008" = `01-501`,
          "00009" = `01-503`,
          "00057" = `86`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  print()

sen_caba %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00006`, sum))

sen_caba %>% write_csv("data/definitivos/caba_sen_paso2019.csv")


# CHACO ####
#-> CAT 000206000000000

votos_agrupaciones_senchaco <- agrupaciones %>% 
  filter(categoria == "000206000000000") %>% 
  group_by(distrito, seccion, circuito, mesa, categoria, agrupacion) %>% 
  distinct(distrito, seccion, circuito, mesa, categoria, agrupacion,.keep_all = TRUE) %>% 
  ungroup() %>% 
  group_by(distrito, seccion, circuito, mesa, categoria) %>% 
  spread(agrupacion, votos) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  ungroup() %>% 
  select(-categoria) %>% 
  print()

# Blancos Nulos
votos_blancos_nulos_senchaco <- blancos_nulos %>% 
  filter(categoria == "000206000000000") %>% 
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

# Electores
electores_senchaco <- electores_gral %>% 
  filter(categoria == "000206000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000206000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "06")

# Join
sen_chaco <- electores_senchaco %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_senchaco) %>% 
  left_join(votos_agrupaciones_senchaco) %>% 
  rename( "00068" = `06-156`,
          "00069" = `06-502`,
          "00001" = `06-504`,
          "00005" = `06-501`,
          "00009" = `06-503`,
          "00066" = `06-179`,
          "00072" = `71`,
          "00070" = `06-180`,
          "00071" = `33`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  print()

sen_chaco %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00072`, sum))

sen_chaco %>% write_csv("data/definitivos/chaco_sen_paso2019.csv")


# ENTRE RIOS ####
#-> CAT 000208000000000

votos_agrupaciones_sener <- agrupaciones %>% 
  filter(categoria == "000208000000000") %>% 
  group_by(distrito, seccion, circuito, mesa, categoria, agrupacion) %>% 
  distinct(distrito, seccion, circuito, mesa, categoria, agrupacion,.keep_all = TRUE) %>% 
  ungroup() %>% 
  group_by(distrito, seccion, circuito, mesa, categoria) %>% 
  spread(agrupacion, votos) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  ungroup() %>% 
  select(-categoria) %>% 
  print()

# Blancos Nulos
votos_blancos_nulos_sener <- blancos_nulos %>% 
  filter(categoria == "000208000000000") %>% 
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

# Electores
electores_sener <- electores_gral %>% 
  filter(categoria == "000208000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000208000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "08")

# Join
sen_er <- electores_sener %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_sener) %>% 
  left_join(votos_agrupaciones_sener) %>% 
  rename( "00076" = `50`,
          "00077" = `68`,
          "00002" = `13`,
          "00005" = `08-501`,
          "00009" = `08-502`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  print()

sen_er %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00076`, sum))

sen_er %>% write_csv("data/definitivos/erios_sen_paso2019.csv")


# NEUQUEN -> 	CAT 000215000000000


# RIO NEGRO -> 	CAT 000216000000000
# SALTA -> CAT 000217000000000
# SANTIAGO -> CAT 000222000000000
# TDF -> 000224000000000

