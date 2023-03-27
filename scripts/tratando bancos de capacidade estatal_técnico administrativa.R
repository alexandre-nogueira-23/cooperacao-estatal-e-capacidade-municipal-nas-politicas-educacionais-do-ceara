###########################################################################
###########################################################################
###           Dados Prêmio escola nota 10                               ###
###########################################################################
###########################################################################

#   (1) Carregando pacotes:
library("pacman")
p_load(tidyverse, data.table,
       here, fs,
       lubridate, googledrive,
       openxlsx, readxl, abjutils, sidrar,
       deflateBR, brazilmaps, knitr,
       basedosdados)

#   (2) Formatando espaço de trabalho:
options(scipen=999) # Evita aparecerem notações científicas nas tabelas
#drive_auth("") # Colocando auth para pegar dados do google drive

#   (3) Criando subpastas:
setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara")
dir_create(c("data",
             "documents",
             "scripts",
             "outputs"))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Censo escolar - docentes          |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Defina o seu projeto no Google Cloud
set_billing_id("rais-dashboard")

# Baixando dicionário:
query <- bdplyr("br_inep_censo_escolar.dicionario")
df <- bd_collect(query) 

# Variáveis de interesse:
# formacao_especif_anos_iniciais
# formacao_especif_anos_finais
# tipo_contratacao

# aqui definimos a nossa query
query <- "SELECT
formacao_especif_anos_iniciais,
formacao_especif_anos_finais,
tipo_contratacao,
ano
FROM `basedosdados.br_inep_censo_escolar.docente` as censo_docentes"

# aqui carregamos o arquivo para o R
censo_docentes <- read_sql(query)


# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Censo escolar - gestores          |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Abrindo censo escolar:
setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara/data/microdados_ed_basica_2012/dados")
censo2012 <- fread("microdados_ed_basica_2012.csv")


#        |#|
#       \###/
#        \#/
#         *




# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|          Salvando                    |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Salvando:
setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara/outputs")
write.xlsx(cooperacao, paste("Banco com variáveis de cooperação_",  Sys.Date(), ".xlsx"))

#                                      ~~~~~ Fim ~~~~~