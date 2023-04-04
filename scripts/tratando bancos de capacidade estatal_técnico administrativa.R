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
etapa_ensino, rede,
ano, sigla_uf, id_municipio
FROM `basedosdados.br_inep_censo_escolar.docente` as censo_docentes
WHERE rede = 'municipal' AND sigla_uf = 'CE'"

# aqui carregamos o arquivo para o R
censo_docentes <- read_sql(query)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|   Tratando variáveis                 |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Tratando var sobre etapa de ensino:
df_docentes_etapa <- df %>%
  filter(id_tabela=="docente") %>%
  filter(nome_coluna=="etapa_ensino") %>%
  mutate(chave = as.character(chave))
  
# Categorias relativas a ensino fundamental: 
# c(4, 41, 5, 56, 6, 65, 69, 7, 70, 72, 8, 9, 10, 11, 12,
#   13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

# Dúvidas: essas duas categorias se encaixam como ensino fundamental? 
# 60 = EJA - Presencial - Integrada à Ed. Profissional de Nível Fundamental - FIC
# 61 = EJA - Semipresencial - Integrada à Ed. Profissional de Nível Fundamental - FIC
# 73 = Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)

censo_docentes.2 <- censo_docentes %>%
  filter(etapa_ensino %in% c(4, 41, 5, 56, 6, 60, 61, 65, 69, 7, 70, 72, 73, 8, 9, 10, 11, 12,
                             13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)) %>%
  group_by(id_municipio, ano) %>%
  summarise(n_docentes_ens.fundamental = n(),
            n_docentes_ens.fundamental_formacao.continuada = sum(formacao_especif_anos_iniciais==1 |
                                                                   formacao_especif_anos_finais==1, na.rm = TRUE),
            perc.docentes.formacao.continuada = n_docentes_ens.fundamental_formacao.continuada/n_docentes_ens.fundamental,
            n_docentes.concursados = sum(tipo_contratacao==1, na.rm = TRUE),
            perc.docentes.concursados = n_docentes.concursados/n_docentes_ens.fundamental)

# Atenção: as variáveis tipo de contratação, formação continuada para anos finais e 
# para anos iniciais só existem de 2011 em diante.
# Deve-se colocar NA nos casos antes de 2011.

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|          Salvando                    |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Salvando:
setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara/outputs")
write.xlsx(censo_docentes.2, paste("Banco com variáveis de capacidade_técnico administrativa_",  Sys.Date(), ".xlsx"))

#                                      ~~~~~ Fim ~~~~~