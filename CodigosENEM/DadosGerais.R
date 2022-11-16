rm(list=ls())
cat("\014")
if(!is.null(dev.list())) dev.off()
library("tidyverse")
library("data.table")
#memory.limit(24576)
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2018")
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=1000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE)

#Escolhendo as colunas de interesse
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=1000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, 
                           select = c("NU_ANO", "NO_MUNICIPIO_RESIDENCIA", "SG_UF_RESIDENCIA", 
                                      "NU_IDADE", "TP_SEXO", "TP_ESTADO_CIVIL", 
                                      "TP_COR_RACA", "TP_NACIONALIDADE", "TP_ST_CONCLUSAO", 
                                      "TP_ANO_CONCLUIU", "TP_ESCOLA", "TP_ENSINO", 
                                      "IN_TREINEIRO", "TP_DEPENDENCIA_ADM_ESC", "TP_LOCALIZACAO_ESC", 
                                      "IN_BAIXA_VISAO", "IN_CEGUEIRA", "IN_SURDEZ", 
                                      "IN_DEFICIENCIA_AUDITIVA", "IN_SURDO_CEGUEIRA", "IN_DEFICIENCIA_FISICA", 
                                      "IN_DEFICIENCIA_MENTAL", "IN_DEFICIT_ATENCAO", "IN_DISLEXIA", 
                                      "IN_DISCALCULIA", "IN_AUTISMO", "IN_VISAO_MONOCULAR", 
                                      "IN_OUTRA_DEF", "IN_GESTANTE", "IN_LACTANTE", 
                                      "IN_IDOSO", "IN_ESTUDA_CLASSE_HOSPITALAR", "IN_SEM_RECURSO", 
                                      "IN_BRAILLE", "IN_AMPLIADA_24", "IN_AMPLIADA_18", 
                                      "IN_LEDOR", "IN_ACESSO", "IN_TRANSCRICAO", 
                                      "IN_LIBRAS", "IN_LEITURA_LABIAL", "IN_MESA_CADEIRA_RODAS", 
                                      "IN_MESA_CADEIRA_SEPARADA", "IN_APOIO_PERNA", "IN_GUIA_INTERPRETE", 
                                      "IN_COMPUTADOR", "IN_CADEIRA_ESPECIAL", "IN_CADEIRA_CANHOTO", 
                                      "IN_CADEIRA_ACOLCHOADA", "IN_PROVA_DEITADO", "IN_MOBILIARIO_OBESO", 
                                      "IN_LAMINA_OVERLAY", "IN_PROTETOR_AURICULAR", "IN_MEDIDOR_GLICOSE", 
                                      "IN_MAQUINA_BRAILE", "IN_SOROBAN", "IN_MARCA_PASSO", 
                                      "IN_SONDA", "IN_MEDICAMENTOS", "IN_SALA_INDIVIDUAL", 
                                      "IN_SALA_ESPECIAL", "IN_SALA_ACOMPANHANTE", "IN_MOBILIARIO_ESPECIFICO", 
                                      "IN_MATERIAL_ESPECIFICO", "IN_NOME_SOCIAL", "NO_MUNICIPIO_PROVA", 
                                      "SG_UF_PROVA", "TP_PRESENCA_CN", "TP_PRESENCA_CH", 
                                      "TP_PRESENCA_LC", "TP_PRESENCA_MT", "NU_NOTA_CN", 
                                      "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", 
                                      "TP_STATUS_REDACAO", "NU_NOTA_COMP1", "NU_NOTA_COMP2", 
                                      "NU_NOTA_COMP3", "NU_NOTA_COMP4", "NU_NOTA_COMP5", 
                                      "NU_NOTA_REDACAO", "Q001", "Q002", 
                                      "Q003", "Q004", "Q005", 
                                      "Q006", "Q007", "Q008", 
                                      "Q009", "Q010", "Q011", 
                                      "Q012", "Q013", "Q014", 
                                      "Q015", "Q016", "Q017", 
                                      "Q018", "Q019", "Q020", 
                                      "Q021", "Q022", "Q023", 
                                      "Q024", "Q025"))

#Retire todas as colunas do Banco de Dados que Deseja 
#Trabalhar
#Nao interessa se o candidato eh treineiro ou nao
#Nao interessa se ele ja terminou ou nao o ensino medio
dados <- data.table::fread(input='MICRODADOS_ENEM_2018.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=10000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c("NU_ANO", "NO_MUNICIPIO_RESIDENCIA", "SG_UF_RESIDENCIA", 
                                                     "NU_IDADE", "TP_SEXO", "TP_ESTADO_CIVIL", 
                                                     "TP_COR_RACA", "TP_NACIONALIDADE", "TP_ST_CONCLUSAO", 
                                                     "TP_ANO_CONCLUIU", "TP_ESCOLA", "TP_ENSINO", 
                                                     "IN_TREINEIRO", "TP_DEPENDENCIA_ADM_ESC", "TP_LOCALIZACAO_ESC", 
                                                     "IN_BAIXA_VISAO", "IN_CEGUEIRA", "IN_SURDEZ", 
                                                     "IN_DEFICIENCIA_AUDITIVA", "IN_SURDO_CEGUEIRA", "IN_DEFICIENCIA_FISICA", 
                                                     "IN_DEFICIENCIA_MENTAL", "IN_DEFICIT_ATENCAO", "IN_DISLEXIA", 
                                                     "IN_DISCALCULIA", "IN_AUTISMO", "IN_VISAO_MONOCULAR", 
                                                     "IN_OUTRA_DEF", "IN_GESTANTE", "IN_LACTANTE", 
                                                     "IN_IDOSO", "IN_ESTUDA_CLASSE_HOSPITALAR", "IN_SEM_RECURSO", 
                                                     "IN_BRAILLE", "IN_AMPLIADA_24", "IN_AMPLIADA_18", 
                                                     "IN_LEDOR", "IN_ACESSO", "IN_TRANSCRICAO", 
                                                     "IN_LIBRAS", "IN_LEITURA_LABIAL", "IN_MESA_CADEIRA_RODAS", 
                                                     "IN_MESA_CADEIRA_SEPARADA", "IN_APOIO_PERNA", "IN_GUIA_INTERPRETE", 
                                                     "IN_COMPUTADOR", "IN_CADEIRA_ESPECIAL", "IN_CADEIRA_CANHOTO", 
                                                     "IN_CADEIRA_ACOLCHOADA", "IN_PROVA_DEITADO", "IN_MOBILIARIO_OBESO", 
                                                     "IN_LAMINA_OVERLAY", "IN_PROTETOR_AURICULAR", "IN_MEDIDOR_GLICOSE", 
                                                     "IN_MAQUINA_BRAILE", "IN_SOROBAN", "IN_MARCA_PASSO", 
                                                     "IN_SONDA", "IN_MEDICAMENTOS", "IN_SALA_INDIVIDUAL", 
                                                     "IN_SALA_ESPECIAL", "IN_SALA_ACOMPANHANTE", "IN_MOBILIARIO_ESPECIFICO", 
                                                     "IN_MATERIAL_ESPECIFICO", "IN_NOME_SOCIAL", "NO_MUNICIPIO_PROVA", 
                                                     "SG_UF_PROVA", "TP_PRESENCA_CN", "TP_PRESENCA_CH", 
                                                     "TP_PRESENCA_LC", "TP_PRESENCA_MT", "NU_NOTA_CN", 
                                                     "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", 
                                                     "TP_STATUS_REDACAO", "NU_NOTA_COMP1", "NU_NOTA_COMP2", 
                                                     "NU_NOTA_COMP3", "NU_NOTA_COMP4", "NU_NOTA_COMP5", 
                                                     "NU_NOTA_REDACAO", "Q001", "Q002", 
                                                     "Q003", "Q004", "Q005", 
                                                     "Q006", "Q007", "Q008", 
                                                     "Q009", "Q010", "Q011", 
                                                     "Q012", "Q013", "Q014", 
                                                     "Q015", "Q016", "Q017", 
                                                     "Q018", "Q019", "Q020", 
                                                     "Q021", "Q022", "Q023", 
                                                     "Q024", "Q025")) %>% 
  filter(TP_PRESENCA_CN=="1",
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
dados$nt_final <- (rowSums(dados[,c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]))/5
dados <- dados[!is.na(dados$nt_final),]
saveRDS(dados, file="Enem2018T.Rds")
#ENEM2018 <- readRDS("Enem2018.Rds") 

rm(list=ls())
cat("\014")
if(!is.null(dev.list())) dev.off()
library("tidyverse")
library("data.table")
setwd("~/GitHub/PortalTransparencia/Transferencias/Enem/2019")
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
dados <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                           integer64='character',
                           skip=0, #Ler do inicio
                           nrow=10000, #Quantidade de registros a serem lidos
                           na.strings = "",
                           showProgress = TRUE, encoding = "Latin-1",
                           header = TRUE, select = c("NU_ANO", "NO_MUNICIPIO_RESIDENCIA", "SG_UF_RESIDENCIA", 
                                                     "NU_IDADE", "TP_SEXO", "TP_ESTADO_CIVIL", 
                                                     "TP_COR_RACA", "TP_NACIONALIDADE", "TP_ST_CONCLUSAO", 
                                                     "TP_ANO_CONCLUIU", "TP_ESCOLA", "TP_ENSINO", 
                                                     "IN_TREINEIRO", "TP_DEPENDENCIA_ADM_ESC", "TP_LOCALIZACAO_ESC", 
                                                     "IN_BAIXA_VISAO", "IN_CEGUEIRA", "IN_SURDEZ", 
                                                     "IN_DEFICIENCIA_AUDITIVA", "IN_SURDO_CEGUEIRA", "IN_DEFICIENCIA_FISICA", 
                                                     "IN_DEFICIENCIA_MENTAL", "IN_DEFICIT_ATENCAO", "IN_DISLEXIA", 
                                                     "IN_DISCALCULIA", "IN_AUTISMO", "IN_VISAO_MONOCULAR", 
                                                     "IN_OUTRA_DEF", "IN_GESTANTE", "IN_LACTANTE", 
                                                     "IN_IDOSO", "IN_ESTUDA_CLASSE_HOSPITALAR", "IN_SEM_RECURSO", 
                                                     "IN_BRAILLE", "IN_AMPLIADA_24", "IN_AMPLIADA_18", 
                                                     "IN_LEDOR", "IN_ACESSO", "IN_TRANSCRICAO", 
                                                     "IN_LIBRAS", "IN_LEITURA_LABIAL", "IN_MESA_CADEIRA_RODAS", 
                                                     "IN_MESA_CADEIRA_SEPARADA", "IN_APOIO_PERNA", "IN_GUIA_INTERPRETE", 
                                                     "IN_COMPUTADOR", "IN_CADEIRA_ESPECIAL", "IN_CADEIRA_CANHOTO", 
                                                     "IN_CADEIRA_ACOLCHOADA", "IN_PROVA_DEITADO", "IN_MOBILIARIO_OBESO", 
                                                     "IN_LAMINA_OVERLAY", "IN_PROTETOR_AURICULAR", "IN_MEDIDOR_GLICOSE", 
                                                     "IN_MAQUINA_BRAILE", "IN_SOROBAN", "IN_MARCA_PASSO", 
                                                     "IN_SONDA", "IN_MEDICAMENTOS", "IN_SALA_INDIVIDUAL", 
                                                     "IN_SALA_ESPECIAL", "IN_SALA_ACOMPANHANTE", "IN_MOBILIARIO_ESPECIFICO", 
                                                     "IN_MATERIAL_ESPECIFICO", "IN_NOME_SOCIAL", "NO_MUNICIPIO_PROVA", 
                                                     "SG_UF_PROVA", "TP_PRESENCA_CN", "TP_PRESENCA_CH", 
                                                     "TP_PRESENCA_LC", "TP_PRESENCA_MT", "NU_NOTA_CN", 
                                                     "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", 
                                                     "TP_STATUS_REDACAO", "NU_NOTA_COMP1", "NU_NOTA_COMP2", 
                                                     "NU_NOTA_COMP3", "NU_NOTA_COMP4", "NU_NOTA_COMP5", 
                                                     "NU_NOTA_REDACAO", "Q001", "Q002", 
                                                     "Q003", "Q004", "Q005", 
                                                     "Q006", "Q007", "Q008", 
                                                     "Q009", "Q010", "Q011", 
                                                     "Q012", "Q013", "Q014", 
                                                     "Q015", "Q016", "Q017", 
                                                     "Q018", "Q019", "Q020", 
                                                     "Q021", "Q022", "Q023", 
                                                     "Q024", "Q025")) %>% 
  filter(TP_PRESENCA_CN=="1",
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
dados$nt_final <- (rowSums(dados[,c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]))/5
dados <- dados[!is.na(dados$nt_final),]
saveRDS(dados, file="Enem2019T.Rds")
ENEM2018 <- readRDS("~/GitHub/PortalTransparencia/Transferencias/Enem/2018/Enem2018T.Rds")
ENEM2019 <- readRDS("~/GitHub/PortalTransparencia/Transferencias/Enem/2019/Enem2019T.Rds")
ENEM <- rbind(ENEM2018,ENEM2019)

#Atendimentos Especializados

ENEM$ATEN_ESPECIALIZADO <- ENEM %>% select("IN_BAIXA_VISAO", "IN_CEGUEIRA", "IN_SURDEZ", 
                                  "IN_DEFICIENCIA_AUDITIVA", "IN_SURDO_CEGUEIRA", "IN_DEFICIENCIA_FISICA", 
                                  "IN_DEFICIENCIA_MENTAL", "IN_DEFICIT_ATENCAO", "IN_DISLEXIA", 
                                  "IN_DISCALCULIA", "IN_AUTISMO", "IN_VISAO_MONOCULAR", 
                                  "IN_OUTRA_DEF") %>% summarise(rowSums(.))
#Atendimentos Especificos
ENEM$ATEN_ESPECIFICO <- ENEM %>% select("IN_GESTANTE", "IN_LACTANTE", 
                                        "IN_IDOSO", "IN_ESTUDA_CLASSE_HOSPITALAR") %>% summarise(rowSums(.)) 
#Recursos Especializados e especificos
ENEM$RECUR_ESPECIALIZADO <- ENEM %>% select("IN_SEM_RECURSO", 
                                            "IN_BRAILLE", "IN_AMPLIADA_24", "IN_AMPLIADA_18", 
                                            "IN_LEDOR", "IN_ACESSO", "IN_TRANSCRICAO", 
                                            "IN_LIBRAS", "IN_LEITURA_LABIAL", "IN_MESA_CADEIRA_RODAS", 
                                            "IN_MESA_CADEIRA_SEPARADA", "IN_APOIO_PERNA", "IN_GUIA_INTERPRETE", 
                                            "IN_COMPUTADOR", "IN_CADEIRA_ESPECIAL", "IN_CADEIRA_CANHOTO", 
                                            "IN_CADEIRA_ACOLCHOADA", "IN_PROVA_DEITADO", "IN_MOBILIARIO_OBESO", 
                                            "IN_LAMINA_OVERLAY", "IN_PROTETOR_AURICULAR", "IN_MEDIDOR_GLICOSE", 
                                            "IN_MAQUINA_BRAILE", "IN_SOROBAN", "IN_MARCA_PASSO", 
                                            "IN_SONDA", "IN_MEDICAMENTOS", "IN_SALA_INDIVIDUAL", 
                                            "IN_SALA_ESPECIAL", "IN_SALA_ACOMPANHANTE", "IN_MOBILIARIO_ESPECIFICO", 
                                            "IN_MATERIAL_ESPECIFICO", "IN_NOME_SOCIAL") %>% summarise(rowSums(.))  
saveRDS(ENEM, file="~/GitHub/PortalTransparencia/Transferencias/BancoDados/EnemTeste.Rds")  

#names(dados) <- c("ANO", "MUNICIPIO_RESIDENCIA", "UF_RESIDENCIA", 
#                  "IDADE", "SEXO", "ESTADO_CIVIL", 
#                  "COR_RACA", "NACIONALIDADE", "ST_CONCLUSAO", 
#                  "ANO_CONCLUIU", "ESCOLA", "ENSINO", 
#                  "TREINEIRO", "DEPENDENCIA_ADM_ESC", "LOCALIZACAO_ESC", 
#                  "BAIXA_VISAO", "CEGUEIRA", "SURDEZ", 
#                  "DEFICIENCIA_AUDITIVA", "SURDO_CEGUEIRA", "DEFICIENCIA_FISICA", 
#                  "DEFICIENCIA_MENTAL", "DEFICIT_ATENCAO", "DISLEXIA", 
#                  "DISCALCULIA", "AUTISMO", "VISAO_MONOCULAR", 
#                  "OUTRA_DEF", "GESTANTE", "LACTANTE", 
#                  "IDOSO", "ESTUDA_CLASSE_HOSPITALAR", "SEM_RECURSO", 
#                  "BRAILLE", "AMPLIADA_24", "AMPLIADA_18", 
#                  "LEDOR", "ACESSO", "TRANSCRICAO", 
#                  "LIBRAS", "LEITURA_LABIAL", "MESA_CADEIRA_RODAS", 
#                  "MESA_CADEIRA_SEPARADA", "APOIO_PERNA", "GUIA_INTERPRETE", 
#                  "COMPUTADOR", "CADEIRA_ESPECIAL", "CADEIRA_CANHOTO", 
#                  "CADEIRA_ACOLCHOADA", "PROVA_DEITADO", "MOBILIARIO_OBESO", 
#                  "LAMINA_OVERLAY", "PROTETOR_AURICULAR", "MEDIDOR_GLICOSE", 
#                  "MAQUINA_BRAILE", "SOROBAN", "MARCA_PASSO", 
#                  "SONDA", "MEDICAMENTOS", "SALA_INDIVIDUAL", 
#                  "SALA_ESPECIAL", "SALA_ACOMPANHANTE", "MOBILIARIO_ESPECIFICO", 
#                  "MATERIAL_ESPECIFICO", "NOME_SOCIAL", "NO_MUNICIPIO_PROVA", 
#                  "UF_PROVA", "PRESENCA_CN", "PRESENCA_CH", 
#                  "PRESENCA_LC", "PRESENCA_MT", "NOTA_CN", 
#                  "NOTA_CH", "NOTA_LC", "NOTA_MT", 
#                  "STATUS_REDACAO", "NOTA_COMP1", "NOTA_COMP2", 
#                  "NOTA_COMP3", "NOTA_COMP4", "NOTA_COMP5", 
#                  "REDACAO", "Q001", "Q002", 
#                  "Q003", "Q004", "Q005", 
#                  "Q006", "Q007", "Q008", 
#                  "Q009", "Q010", "Q011", 
#                  "Q012", "Q013", "Q014", 
#                  "Q015", "Q016", "Q017", 
#                  "Q018", "Q019", "Q020", 
#                  "Q021", "Q022", "Q023", 
#                  "Q024", "Q025")