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

# NEUQUEN ####
# -> 	CAT 000215000000000

votos_agrupaciones_nqn <- agrupaciones %>% 
  filter(categoria == "000215000000000") %>% 
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
votos_blancos_nulos_nqn <- blancos_nulos %>% 
  filter(categoria == "000215000000000") %>% 
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
electores_nqn <- electores_gral %>% 
  filter(categoria == "000215000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000215000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "15")

# Join
sen_nqn <- electores_nqn %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_nqn) %>% 
  left_join(votos_agrupaciones_nqn) %>% 
  rename( "00001" = `15-504`,
          "00008" = `15-502`,
          "00005" = `15-503`,
          "00009" = `15-501`,
          "00002" = `13`,
          "00085" = `15-151`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  print()

sen_nqn %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00001`, sum))

sen_nqn %>% write_csv("data/definitivos/neuquen_sen_paso2019.csv")

# RIO NEGRO ####
#-> 	CAT 000216000000000


votos_agrupaciones_rn <- agrupaciones %>% 
  filter(categoria == "000216000000000") %>% 
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
votos_blancos_nulos_rn <- blancos_nulos %>% 
  filter(categoria == "000216000000000") %>% 
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
electores_rn <- electores_gral %>% 
  filter(categoria == "000216000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000216000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "16")

# Join
sen_rn <- electores_rn %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_rn) %>% 
  left_join(votos_agrupaciones_rn) %>% 
  rename( "00008" = `16-503`,
          "00005" = `16-502`,
          "00086" = `16-169`,
          "00002" = `13`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  print()

sen_rn %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00008`, sum))

sen_rn %>% write_csv("data/definitivos/rnegro_sen_paso2019.csv")

# SALTA ####
# -> CAT 000217000000000


votos_agrupaciones_salta <- agrupaciones %>% 
  filter(categoria == "000217000000000") %>% 
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
votos_blancos_nulos_salta <- blancos_nulos %>% 
  filter(categoria == "000217000000000") %>% 
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
electores_salta <- electores_gral %>% 
  filter(categoria == "000217000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000217000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "17")

# Join
sen_salta <- electores_salta %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_salta) %>% 
  left_join(votos_agrupaciones_salta) %>% 
  rename( "00088" = `19`,
          "00008" = `17-504`,
          "00005" = `17-501`,
          "00009" = `17-503`,
          "00087" = `17-502`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  drop_na() %>% 
  print()

sen_salta %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00088`, sum))

sen_salta %>% write_csv("data/definitivos/salta_sen_paso2019.csv")

# SANTIAGO ####
#-> CAT 000222000000000



votos_agrupaciones_sgo <- agrupaciones %>% 
  filter(categoria == "000222000000000") %>% 
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
votos_blancos_nulos_sgo <- blancos_nulos %>% 
  filter(categoria == "000222000000000") %>% 
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
electores_sgo <- electores_gral %>% 
  filter(categoria == "000222000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000222000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "22")

# Join
sen_sgo <- electores_sgo %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_sgo) %>% 
  left_join(votos_agrupaciones_sgo) %>% 
  rename( "00095" = `22-501`,
          "00005" = `22-502`,
          "00009" = `22-503`,
          "00094" = `40`,
          "00079" = `38`,
          "00006" = `22-179`,
          "00061" = `22-178`,
          "00004" = `87`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  drop_na() %>% 
  print()

sen_sgo %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00004`, sum))

sen_sgo %>% write_csv("data/definitivos/santiago_sen_paso2019.csv")

# TDF ####
# -> 000224000000000


votos_agrupaciones_tdf <- agrupaciones %>% 
  filter(categoria == "000224000000000") %>% 
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
votos_blancos_nulos_tdf <- blancos_nulos %>% 
  filter(categoria == "000224000000000") %>% 
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
electores_tdf <- electores_gral %>% 
  filter(categoria == "000224000000000") %>%  
  select(-contador, -categoria) %>% 
  group_by(distrito, seccion, circuito, mesa) %>% 
  mutate(mesa = str_pad(mesa, width = 6, pad = "0", side = "left"),
         circuito = str_pad(circuito, width = 5, pad = "0", side = "left")) %>% 
  rename("codprov" = "distrito",
         "coddepto" = "seccion") %>% 
  ungroup() %>% 
  print()

descripcion_postulaciones %>% 
  filter(CODIGO_CATEGORIA == "000224000000000") %>% 
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION)

read_csv("data/listas_sen_paso2019 copy.csv") %>% 
  filter(vot_proCodigoProvincia == "24")

# Join
sen_tdf <- electores_tdf %>% 
  left_join(secciones) %>% 
  left_join(votos_blancos_nulos_tdf) %>% 
  left_join(votos_agrupaciones_tdf) %>% 
  rename( "00005" = `24-501`,
          "00100" = `69`,
          "00009" = `24-502`,
          "00097" = `24-503`,
          "00099" = `24-187`,
          "00098" = `24-503`,
          "electores" = "votos") %>% 
  select(codprov, coddepto, depto, circuito, mesa, everything()) %>% 
  drop_na() %>% 
  print()

sen_tdf %>%  
  select(-circuito, -mesa) %>% 
  summarise(across(electores:`00100`, sum))

sen_tdf %>% write_csv("data/definitivos/tdf_sen_paso2019.csv")

