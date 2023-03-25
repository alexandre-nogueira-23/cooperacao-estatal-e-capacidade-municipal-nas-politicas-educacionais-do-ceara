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
       deflateBR, brazilmaps, knitr)

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

pend.3 <- pend.2 %>%
  full_join(pend.apo, by=c("MUNICÍPIO", "ano")) 

pend.4 <- pend.3 %>%
  mutate(valor_pen10_normal = valor.x.x+valor.y.x+valor.x.x.x,
         valor_pen10_apoiadas = valor.x.y+valor.y.y+valor.y.y.y) %>%
  select(-c(valor.x.x:valor.y.y.y)) %>%
  mutate(valor_pen10_normal = ifelse(is.na(valor_pen10_normal), 0, valor_pen10_normal),
         valor_pen10_apoiadas = ifelse(is.na(valor_pen10_apoiadas), 0, valor_pen10_apoiadas),
         valor_pen10_total = valor_pen10_normal+valor_pen10_apoiadas)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|       Tratando banco ICMS            |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

icms.2009 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 1) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2009=X22)
icms.2010 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 2) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2))  %>%
  rename(ano2010=X22)
icms.2011 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 3) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2011=X22)
icms.2012 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 4) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2012=X22)
icms.2013 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 5) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2013=X22)
icms.2014 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 6) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2014=X22)
icms.2015 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 7) %>%
  select(X4, X24) %>%
  slice(3:190) %>%
  filter(!is.na(X4)) %>%
  rename(ano2015=X24)
icms.2016 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 8) %>%
  select(X1, X21) %>%
  slice(3:190) %>%
  filter(!is.na(X1)) %>%
  rename(ano2016=X21)
icms.2017 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 9) %>%
  select(X1, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X1)) %>%
  rename(ano2017=X22)
icms.2018 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 10) %>%
  select(X2, X22) %>%
  slice(3:190) %>%
  filter(!is.na(X2)) %>%
  rename(ano2018=X22)
icms.2019 <- read.xlsx("Repasses ICMS - 2009 a 2019 (1) (1).xlsx", sheet = 11) %>%
  select(X3, X23) %>%
  slice(3:190) %>%
  filter(!is.na(X3)) %>%
  rename(ano2019=X23)

#        |#|
#       \###/
#        \#/
#         *

icms <- icms.2009 %>%
  full_join(icms.2010, by="X2") %>%
  full_join(icms.2011, by="X2") %>%
  full_join(icms.2012, by="X2") %>%
  full_join(icms.2013, by="X2") %>%
  full_join(icms.2014, by="X2") %>%
  full_join(icms.2015, by=c("X2"="X4")) %>%
  full_join(icms.2016, by=c("X2"="X1")) %>%
  full_join(icms.2017, by=c("X2"="X1")) %>%
  full_join(icms.2018, by="X2") %>%
  full_join(icms.2019, by=c("X2"="X3")) %>%
  filter(ano2009!="IQE")

icms.2 <- icms %>%
  pivot_longer(ano2009:ano2019,
               names_to = "ano",
               values_to = "valor") %>%
  mutate(ano = gsub("ano", "", ano))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    juntando bases icms e pena0       |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

cooperacao <- pend.4 %>%
  full_join(icms.2, c("MUNICÍPIO"="X2", "ano"="ano")) %>%
  rename(valor_icms = valor) %>%
  mutate(across(c(valor_pen10_normal:valor_icms), ~ifelse(is.na(.x), 0, .x)))

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