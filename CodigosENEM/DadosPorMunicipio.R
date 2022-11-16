rm(list=ls())
cat("\014")
if(!is.null(dev.list())) dev.off()
library("tidyverse")
library("data.table")
memory.limit(24576)
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2018")
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=1000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE)
#Retire todas as colunas do Banco de Dados que Deseja 
#Trabalhar
#Nao interessa se o candidato eh treineiro ou nao
#Nao interessa se ele ja terminou ou nao o ensino medio
#So consideramos escola publica
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=10000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c(2,4,6:7,18,20,83:86,91:94,104:110)) %>% 
                     filter(TP_ESCOLA=="2",
                            TP_PRESENCA_CN=="1",
                            TP_PRESENCA_CH=="1",
                            TP_PRESENCA_LC=="1",
                            TP_PRESENCA_MT=="1",
                            TP_STATUS_REDACAO=="1")
#Apos conferir os dados e as variaveis de interesse
#fazemos o filtro
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=-1, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c(2,4,6:7,18,20,83:86,91:94,104:110)) %>% 
                      filter(TP_ESCOLA=="2",
                             TP_PRESENCA_CN=="1",
                             TP_PRESENCA_CH=="1",
                             TP_PRESENCA_LC=="1",
                             TP_PRESENCA_MT=="1",
                             TP_STATUS_REDACAO=="1")
dados$NO_MUNICIPIO_RESIDENCIA <- str_to_upper(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- str_to_upper(dados$SG_UF_RESIDENCIA)
dados$NO_MUNICIPIO_RESIDENCIA <- abjutils::rm_accent(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- abjutils::rm_accent(dados$SG_UF_RESIDENCIA)
dados$NO_MUNICIPIO_RESIDENCIA <- str_trim(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- str_trim(dados$SG_UF_RESIDENCIA)
dados <- dados %>% select(2:4,11:14,16:21,1)
names(dados) <- c("MUNICIPIO", "UF", "IDADE","NOTA_CN","NOTA_CH","NOTA_LC","NOTA_MT","COMP1","COMP2","COMP3","COMP4","COMP5", "REDACAO", "ANO")
dados$nt_final <- (rowSums(dados[,c("NOTA_CN", "NOTA_CH", "NOTA_LC", "NOTA_MT", "REDACAO")]))/5
dados <- dados[!is.na(dados$nt_final),]
dados <- dados %>% 
  group_by(MUNICIPIO, UF) %>% 
  summarise(ID=mean(IDADE), 
            NOTA_CN=round(mean(NOTA_CN), digits = 2),
            NOTA_CH=round(mean(NOTA_CH), digits = 2),
            NOTA_LC=round(mean(NOTA_LC), digits = 2),
            NOTA_MT=round(mean(NOTA_MT), digits = 2),
            COMP1=round(mean(COMP1), digits = 2),
            COMP2=round(mean(COMP2), digits = 2),
            COMP3=round(mean(COMP3), digits = 2),
            COMP4=round(mean(COMP4), digits = 2),
            COMP5=round(mean(COMP5), digits = 2),
            REDACAO=round(mean(REDACAO), digits = 2),
            NT=round(mean(nt_final), digits = 2),
            NCand=n(),
            ANO=round(mean(ANO), digits = 0),
            .groups = 'drop')
dados$ID <- round(dados$ID, digits = 2)
saveRDS(dados, file="Enem2018.Rds")
 


rm(list=ls())
cat("\014")
if(!is.null(dev.list())) dev.off()
library("tidyverse")
library("data.table")
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2019")
memory.limit(24576)
#Visualize todas as colunas do Banco de Dados
dados <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=1000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE)
#Retire todas as colunas do Banco de Dados que Deseja 
#Trabalhar
#Nao interessa se o candidato eh treineiro ou nao
#Nao interessa se ele ja terminou ou nao o ensino medio
#So consideramos escola publica
dados <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=10000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c(2,4,6:7,18,20,84:87,92:95,105:111)) %>% 
  filter(TP_ESCOLA=="2",
         TP_PRESENCA_CN=="1",
         TP_PRESENCA_CH=="1",
         TP_PRESENCA_LC=="1",
         TP_PRESENCA_MT=="1",
         TP_STATUS_REDACAO=="1")
#Apos conferir os dados e as variaveis de interesse
#fazemos o filtro
dados <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=-1, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c(2,4,6:7,18,20,84:87,92:95,105:111)) %>% 
  filter(TP_ESCOLA=="2",
         TP_PRESENCA_CN=="1",
         TP_PRESENCA_CH=="1",
         TP_PRESENCA_LC=="1",
         TP_PRESENCA_MT=="1",
         TP_STATUS_REDACAO=="1")
dados$NO_MUNICIPIO_RESIDENCIA <- str_to_upper(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- str_to_upper(dados$SG_UF_RESIDENCIA)
dados$NO_MUNICIPIO_RESIDENCIA <- abjutils::rm_accent(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- abjutils::rm_accent(dados$SG_UF_RESIDENCIA)
dados$NO_MUNICIPIO_RESIDENCIA <- str_trim(dados$NO_MUNICIPIO_RESIDENCIA)
dados$SG_UF_RESIDENCIA        <- str_trim(dados$SG_UF_RESIDENCIA)
dados <- dados %>% select(2:4,11:14,16:21,1)
names(dados) <- c("MUNICIPIO", "UF", "IDADE","NOTA_CN","NOTA_CH","NOTA_LC","NOTA_MT","COMP1","COMP2","COMP3","COMP4","COMP5", "REDACAO", "ANO")
dados$nt_final <- (rowSums(dados[,c("NOTA_CN", "NOTA_CH", "NOTA_LC", "NOTA_MT", "REDACAO")]))/5
dados <- dados[!is.na(dados$nt_final),]
dados <- dados %>% 
  group_by(MUNICIPIO, UF) %>% 
  summarise(ID=mean(IDADE), 
            NOTA_CN=round(mean(NOTA_CN), digits = 2),
            NOTA_CH=round(mean(NOTA_CH), digits = 2),
            NOTA_LC=round(mean(NOTA_LC), digits = 2),
            NOTA_MT=round(mean(NOTA_MT), digits = 2),
            COMP1=round(mean(COMP1), digits = 2),
            COMP2=round(mean(COMP2), digits = 2),
            COMP3=round(mean(COMP3), digits = 2),
            COMP4=round(mean(COMP4), digits = 2),
            COMP5=round(mean(COMP5), digits = 2),
            REDACAO=round(mean(REDACAO), digits = 2),
            NT=round(mean(nt_final), digits = 2),
            NCand=n(),
            ANO=round(mean(ANO), digits = 0),
            .groups = 'drop')
dados$ID <- round(dados$ID, digits = 2)
saveRDS(dados, file="Enem2019.Rds")
rm(list = ls())
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2018")
ENEM2018 <- readRDS("Enem2018.Rds")
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2019")
ENEM2019 <- readRDS("Enem2019.Rds")
ENEM <- rbind(ENEM2018,ENEM2019)
saveRDS(ENEM, file="~/GitHub/PortalTransparencia/Transferencias/BancoDados/Enem.Rds")  

setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2018")
ENEM2018 <- readRDS("Enem2018.Rds") 
xlsx::write.xlsx(ENEM2018, "ENEM2018.xlsx", sheetName="ENEM2018")
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2019")
ENEM2019 <- readRDS("Enem2019.Rds") 
xlsx::write.xlsx(ENEM2019, "ENEM2019.xlsx", sheetName="ENEM2019")
