###########################################################################
###########################################################################
###           Dados Prêmio escola nota 10                               ###
###########################################################################
###########################################################################

#   (1) Carregando pacotes:
library("pacman")

p_load(tidyverse, data.table,
       here, fs,
       lubridate, sf, geobr, googledrive,
       openxlsx, readxl, abjutils, sidrar,
       deflateBR, brazilmaps, knitr)

#   (2) Formatando espaço de trabalho:
options(scipen=999) # Evita aparecerem notações científicas nas tabelas
drive_auth("") # Colocando auth para pegar dados do google drive

#   (3) Criando subpastas:
setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara")

dir_create(c("data",
             "documents",
             "scripts",
             "outputs"))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Tratando dados Prêmio escola n10  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

setwd("~/Documentos/consultorias/cooperacao-estatal-e-capacidade-municipal-nas-politicas-educacionais-do-ceara/data")
pend.2ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 2) 
pend.2ano.2 <- pend.2ano %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2008.0`:`2017parcela2`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=`2008.0`,
            `2009`=`2009.0`+`2009parcela2`,
            `2010`=`2010.0`+`2010parcela2`,
            `2011`=`2011.0`+`2011parcela2`,
            `2012`=`2012.0`,
            `2013`=`2013.0`+`2013parcela2`,
            `2014`=`2014.0`+`2014parcela2`,
            `2015`=`2015.0`+`2015parcela2`,
            `2016`=`2016.0`+`2016parcela2`,
            `2017`=`2017.0`+`2017parcela2`) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor")

pend.5ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 3) 
pend.5ano.2 <- pend.5ano %>%
  mutate(`2011.0` = ifelse(ESCOLAS.PREMIADAS=="MARIA NUBIA VIEIRA NOVAIS E E F PROF", NA_real_, `2011.0`),
         `2011.0` = as.numeric(`2011.0`)) %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2010.0`:`2017parcela2`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=0,
            `2009`=0,
            `2010`=`2010.0`,
            `2011`=`2011.0`+`2011parcela2`,
            `2012`=`2012.0`,
            `2013`=`2013.0`+`2013parcela2`,
            `2014`=`2014.0`+`2014parcela2`,
            `2015`=`2015.0`+`2015parcela2`,
            `2016`=`2016.0`+`2016parcela2`,
            `2017`=`2017.0`+`2017parcela2`) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor")

pend.9ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 4) 
pend.9ano.2 <- pend.9ano %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2015.0`:`2017.0`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=0,
            `2009`=0,
            `2010`=0,
            `2011`=0,
            `2012`=0,
            `2013`=0,
            `2014`=0,
            `2015`=`2015.0`,
            `2016`=`2016.0`,
            `2017`=`2017.0`) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor")

#        |#|
#       \###/
#        \#/
#         *

# Dados de escolas apoiadas:
pend.apo.2ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 6) 
pend.apo.2ano.2 <- pend.apo.2ano %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2008.0`:`2016parcela2`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=`2008.0`,
            `2009`=`2009.0`+`2009parcela2`,
            `2010`=`2010.0`+`2010parcela2`,
            `2011`=`2011.0`+`2011parcela2`,
            `2012`=`2012.0`,
            `2013`=`2013.0`+`2013parcela2`,
            `2014`=`2014.0`+`2014parcela2`,
            `2015`=`2015parcela2`,
            `2016`=`2016parcela2`,
            `2017`=0) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor")

pend.apo.5ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 7) 
pend.apo.5ano.2 <- pend.apo.5ano %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2011.0`:`2017parcela2`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=0,
            `2009`=0,
            `2010`=0,
            `2011`=`2011.0`,
            `2012`=`2012.0`,
            `2013`=`2013.0`+`2013parcela2`,
            `2014`=`2014.0`+`2014parcela2`,
            `2015`=`2015.0`+`2015parcela2`,
            `2016`=`2016.0`+`2016parcela2`,
            `2017`=`2017.0`+`2017parcela2`) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor")

pend.apo.9ano <- read.xlsx("Premio Escola Nota 10 - parcelasSEDUC.xlsx", sheet = 8) 
pend.apo.9ano.2 <- pend.apo.9ano %>%
  group_by(MUNICÍPIO) %>%
  summarise(across(c(`2015.0`:`2017.0`), \(x) sum(x, na.rm = TRUE))) %>%
  transmute(MUNICÍPIO,
            `2008`=0,
            `2009`=0,
            `2010`=0,
            `2011`=0,
            `2012`=0,
            `2013`=0,
            `2014`=0,
            `2015`=`2015.0`,
            `2016`=`2016.0`,
            `2017`=`2017.0`) %>%
  pivot_longer(`2008`:`2017`,
               names_to = "ano",
               values_to = "valor") 

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|          Cruzando bases              |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

pend.2 <- pend.2ano.2 %>%
  full_join(pend.5ano.2, by=c("MUNICÍPIO", "ano")) %>%
  full_join(pend.9ano.2, by=c("MUNICÍPIO", "ano")) 

pend.apo <- pend.apo.2ano.2 %>%
  full_join(pend.apo.5ano.2, by=c("MUNICÍPIO", "ano")) %>%
  full_join(pend.apo.9ano.2, by=c("MUNICÍPIO", "ano")) %>%
  filter(MUNICÍPIO!="TOTAL")


# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|             Salvando                 |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Salvando:
setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\trajeto-renda-lista-completa-dos-beneficiarios-de-todas-as-fases\\outputs")
write.xlsx(lista.12, "trajeto renda_lista completa dos beneficiários de todas as fases_20230310.xlsx")
write.xlsx(score, "trajeto renda_lista completa dos beneficiários de todas as fases_score atualização por município_20230310.xlsx")

#lista.12 <- read.xlsx("trajeto renda_lista completa dos beneficiários de todas as fases_20230310.xlsx")

#                                      ~~~~~ Fim ~~~~~