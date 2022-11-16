rm(list = ls())
cat("\014")
library(data.table)
library(tidyverse)
setwd("~/GitHub/PortalTransparencia/Transferencias/Saeb")
df2017 <- data.table::fread("2017/TS_MUNICIPIO.csv",
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=-1, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE)

df2017 <- df2017 %>% select(c("NO_UF", 
                              "NO_MUNICIPIO", 
                              "DEPENDENCIA_ADM", 
                              "LOCALIZACAO", 
                              "MEDIA_5_LP", 
                              "MEDIA_5_MT", 
                              "MEDIA_9_LP", 
                              "MEDIA_9_MT"))
df2017$Ano <- 2017
df2019 <- data.table::fread("2019/TS_MUNICIPIO.csv",
                            integer64='character',
                            skip=0, #Ler do inicio
                            nrow=-1, #Quantidade de registros a serem lidos
                            na.strings = "",
                            showProgress = TRUE, encoding = "Latin-1",
                            header = TRUE)
df2019 <- df2019 %>% select(c("NO_UF", 
                              "NO_MUNICIPIO", 
                              "DEPENDENCIA_ADM", 
                              "LOCALIZACAO", 
                              "MEDIA_5_LP", 
                              "MEDIA_5_MT", 
                              "MEDIA_9_LP", 
                              "MEDIA_9_MT"))
df2019$Ano <- 2019
saeb <- rbind(df2017, df2019)
setwd("~/GitHub/PortalTransparencia/Transferencias/BancoDados")
dfMun <- saeb %>% filter(DEPENDENCIA_ADM=="Municipal")
dfMun <- dfMun %>% filter(LOCALIZACAO=="Total")
dfMun <- dfMun %>% mutate_all(na_if,"")
dfMun[is.na(dfMun)] <- 0
dfMun <- dfMun %>% select(2,1,5:9)
names(dfMun) <- c("MUNICIPIO", 
               "UF",
               "MEDIA_5_LP",
               "MEDIA_5_MT",
               "MEDIA_9_LP",
               "MEDIA_9_MT",
               "ANO")

UF <- fread("UF.csv", encoding = "Latin-1")
names(UF) <- c("UF", "Sigla")
UF$UF <- str_trim(UF$UF)
UF$UF <- str_to_upper(UF$UF)
UF$UF <- abjutils::rm_accent(UF$UF)
dfMun$UF <- str_trim(dfMun$UF)
dfMun$UF <- str_to_upper(dfMun$UF)
dfMun$UF <- abjutils::rm_accent(dfMun$UF)
dfMun <- inner_join(dfMun, UF, by = "UF")
dfMun <- dfMun %>% select(1,8, 3:7)
names(dfMun) <- c("MUNICIPIO",
                "UF",
                "MEDIA_5_LP",
                "MEDIA_5_MT",
                "MEDIA_9_LP",
                "MEDIA_9_MT",
                "Ano")
dfMun$UF <- str_trim(dfMun$UF)
dfMun$MUNICIPIO <- str_trim(dfMun$MUNICIPIO)
dfMun$MUNICIPIO <- str_to_upper(dfMun$MUNICIPIO)
dfMun$MUNICIPIO <- abjutils::rm_accent(dfMun$MUNICIPIO)
#unique(dfMun$UF)
saveRDS(dfMun, "saeb.Rds")
xlsx::write.xlsx(dfMun, "SAEB.xlsx", sheetName="SAEB")
#df1 <- readODS::read.ods("TS_MUNICIPIO.ods")

