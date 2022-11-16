rm(list = ls())
cat("\014")
library(tidyverse)
###############################################################
df1 <- readRDS("DadosTransferencias/transferencias.Rds")
df2 <- readRDS("DadosENEM/ENEM.Rds")
df3 <- readRDS("DadosSAEB/SAEB.Rds")
Pop <- readRDS("DadosPopulacionais/PopEstadoMUN.RDS")
##############################################################
############Pagina Ensino Fundamental 1
#######################################################

#########Grafico de Colunas

teste <- df1 %>%
  filter(Ano==2020) %>%
  group_by(UF) %>%
  summarise(Valor=round((sum(VALOR)/1000000000), digits = 5),
            .groups = 'drop')
plot1 <- ggplot(teste,
                aes(reorder(factor(UF), desc(Valor)),
                    y = Valor,
                    fill = UF,
                    text = paste("UF",UF,
                                 "<br>",
                                 "Valor",paste0("R$",formatC(1000000000*Valor, digits = 2, big.mark=',', format = 'f'))))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Valor),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  xlab("Unidade Federativa") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos do Governo Federal no ano de ", 2020, " em bilhões de reais"))

plotly::ggplotly(plot1, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE) #%>%
#plotly::style(textposition = "top")

############Total por Municipio

MunEF1 <- df1 %>% filter(UF=="MG")
MunEF2 <- MunEF1 %>%
  filter(MUNICIPIO=="VICOSA") %>%
  group_by(MUNICIPIO, Ano) %>%
  summarise(Valor=round((sum(VALOR)), digits = 5),
            .groups = 'drop') %>%
  as.data.frame()
minimo <- (min(unique(MunEF2$Ano)))
maximo <- (max(unique(MunEF2$Ano)))

plot1 <- ggplot(MunEF2,
                aes(factor(Ano),
                    Valor,
                    fill = factor(Ano),
                    text = paste("Ano:",Ano,
                                 "<br>",
                                 "Valor:",paste0("R$",formatC(Valor, digits = 2, big.mark=',', format = 'f')),
                                 "<br>",
                                 "Município:", "vICOSA"))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=sprintf("R$ %.2f", Valor)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Ano") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos de ",minimo," a ", maximo, " por ", "Vicosa"))

plotly::ggplotly(plot1, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE) #%>%
#plotly::style(textposition = "top")


#####Fonte de Recurso Por Municipio

MunEF3 <- df1 %>% filter(Ano==2020)
MunEF4 <- MunEF3 %>% filter(UF=="MG")
MunEF5 <- MunEF4 %>%
  filter(MUNICIPIO=="VICOSA") %>%
  group_by(MUNICIPIO, LINGUAGEM_CIDADA) %>%
  summarise(Valor=round((sum(VALOR)), digits = 5),
            .groups = 'drop') %>%
  as.data.frame()
plot1 <- ggplot(MunEF5,
                aes(x=factor(LINGUAGEM_CIDADA),
                    y = Valor,
                    fill = factor(LINGUAGEM_CIDADA),
                    text = paste("Programa:",LINGUAGEM_CIDADA,
                                 "<br>",
                                 "Valor:",paste0("R$",formatC(Valor, digits = 2, big.mark=',', format = 'f')),
                                 "<br>",
                                 "Município:", "Vicosa"))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=sprintf("R$ %.2f", Valor)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("LINGUAGEM_CIDADA") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos pelo Município de ", "Viçosa", " no ano de ", "2020"))

plotly::ggplotly(plot1, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE) #%>%
#plotly::style(textposition = "top")

#############Grafico de Linha


#############Tabela de DUpla Entrada

###Por Estados

teste <- df1 %>%
  filter(Ano=="2020") %>%
  ungroup() %>%
  select(UF, LINGUAGEM_CIDADA, VALOR) %>%
  group_by(UF, LINGUAGEM_CIDADA) %>%
  summarise(VALOR=sum(VALOR), .groups = 'drop')

teste <- teste %>%
  pivot_wider(names_from = LINGUAGEM_CIDADA,
              values_from = VALOR,
              values_fill = 0) %>%
  mutate(Total=rowSums(.[-1]))
teste$UF <- as.character(teste$UF)
teste <- teste %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))


DT::datatable(teste,
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))


########Por Municipio

table1 <- df1 %>% filter(UF=="MG")
table2 <- table1 %>%
  filter(MUNICIPIO=="VICOSA") %>%
  ungroup() %>%
  select(Ano, LINGUAGEM_CIDADA, VALOR) %>%
  group_by(Ano, LINGUAGEM_CIDADA) %>%
  summarise(VALOR=sum(VALOR), .groups = 'drop')

table2 <- table2 %>%
  pivot_wider(names_from = LINGUAGEM_CIDADA,
              values_from = VALOR,
              values_fill = 0) %>%
  mutate(Total=rowSums(.[-1]))
table2$Ano <- as.character(table2$Ano)
table2 <- table2 %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
DT::datatable(table2,
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))


##############################################################
############Pagina Ensino Fundamental 2
#######################################################

########Graficos de Colunas ou Barras

df1 <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  filter(MEDIA_5_LP!=0) %>%
  summarise(Nota=round(mean(MEDIA_5_LP), digits = 2),
            .groups = 'drop')

plot2 <- ggplot(df1,
                aes(x=reorder(factor(UF), Nota),
                    y = Nota,
                    fill = UF,
                    text = paste('<br>UF:', UF,
                                 '<br>Nota:', Nota))) +
  geom_bar(stat='identity',
           position = 'dodge',
           show.legend = FALSE
  ) +
  geom_text(
    aes(label = Nota,
        y = Nota+0.1*min(Nota)),
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  coord_flip() +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("LP - 5° Ano"," no SAEB do ano de ", '2019'))

plotly::ggplotly(plot2, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
#plotly::style(textposition = 'center')

######################################
#############Graficos de Colunas######
#############Estados (Percentuais)######
######################################

teste <- df3 %>%
  filter(Ano==2019 &
           MEDIA_5_LP!=0&
           MEDIA_5_MT!=0&
           MEDIA_9_LP!=0&
           MEDIA_9_MT!=0) %>%
  group_by(UF) %>% summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP),digits = 2),
                             MEDIA_5_MT=round(mean(MEDIA_5_MT),digits = 2),
                             MEDIA_9_LP=round(mean(MEDIA_9_LP),digits = 2),
                             MEDIA_9_MT=round(mean(MEDIA_9_MT),digits = 2), .groups = 'drop')

Max5LP <-  max(teste$MEDIA_5_LP)
UF5LP <- teste[teste$MEDIA_5_LP==max(teste$MEDIA_5_LP),1]
Max5MT <-  max(teste$MEDIA_5_MT)
UF5MT <- teste[teste$MEDIA_5_MT==max(teste$MEDIA_5_MT),1]
Max9LP <-  max(teste$MEDIA_9_LP)
UF9LP <- teste[teste$MEDIA_9_LP==max(teste$MEDIA_9_LP),1]
Max9MT <-  max(teste$MEDIA_9_MT)
UF9MT <- teste[teste$MEDIA_9_MT==max(teste$MEDIA_9_MT),1]


teste <- df3 %>%
  filter(Ano=="2019" &
           MEDIA_5_LP!=0&
           MEDIA_5_MT!=0&
           MEDIA_9_LP!=0&
           MEDIA_9_MT!=0) %>%
  group_by(UF) %>% summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP),digits = 2),
                             MEDIA_5_MT=round(mean(MEDIA_5_MT),digits = 2),
                             MEDIA_9_LP=round(mean(MEDIA_9_LP),digits = 2),
                             MEDIA_9_MT=round(mean(MEDIA_9_MT),digits = 2), .groups = 'drop')

teste <- teste %>%
  mutate(MEDIA_5_LP=round((MEDIA_5_LP/max(MEDIA_5_LP))*100,digits = 2),
         MEDIA_5_MT=round((MEDIA_5_MT/max(MEDIA_5_MT))*100,digits = 2),
         MEDIA_9_LP=round((MEDIA_9_LP/max(MEDIA_9_LP))*100,digits = 2),
         MEDIA_9_MT=round((MEDIA_9_MT/max(MEDIA_9_MT))*100,digits = 2))


df1 <- teste %>%
  select(UF, MEDIA_5_LP)

names(df1) <- c("UF", "Nota")
plot2 <- ggplot(df1, aes(x=reorder(factor(UF), Nota),
                         y = Nota,
                         fill = UF,
                         text = paste('<br>UF:', UF,
                                      '<br>Nota:', Nota))) +
  geom_bar(stat='identity',
           position = 'dodge',
           show.legend = FALSE
  ) +
  geom_text(
    aes(label = paste0(Nota, "%"),
        y = Nota+0.1*min(Nota)),
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  coord_flip() +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Percentual de ","LP - 5 Ano"," do SAEB do Ano de ", "2019", " Comparada ao Estado com Maior Nota"))

plotly::ggplotly(plot2, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
#plotly::style(textposition = 'center')

######################################
#############Graficos de Colunas######
#############Municipios######
######################################

table1EF2 <- df3 %>% filter(Ano=="2019")
table2EF2 <- table1EF2 %>% filter(UF=="MG")
table3EF2 <- table2EF2 %>%
  filter(MUNICIPIO == "VICOSA") %>%
  as.data.frame() %>%
  select(-Ano, -UF)
names(table3EF2) <- c("Municipio",
                      "LP - 5° Ano",
                      "MT - 5° Ano",
                      "LP - 9° Ano",
                      "MT - 9° Ano")

table4EF2 <- table3EF2 %>%
  gather(key = "Provas", value = "Notas", -Municipio) %>%
  filter(Notas!=0)
plot2 <- ggplot(table4EF2,
                aes(x=reorder(factor(Provas), Notas),
                    y = Notas,
                    fill = factor(Provas),
                    text = paste('<br>Provas:', Provas,
                                 '<br>Nota:', Notas))) +
  geom_bar(stat='identity',
           position = 'dodge',
           show.legend = FALSE
  ) +
  geom_text(
    aes(label = Notas,
        y = Notas+0.1*min(Notas)),
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  coord_flip() +
  xlab("Provas") +
  ylab("Notas") +
  theme_light() +
  ggtitle(paste0("Nota da cidade de ", "VIÇOSA"," no Saeb do ano de ", 2019))

plotly::ggplotly(plot2, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
#plotly::style(textposition = 'center')

########Tabela de Dupla Entrada

tb1 <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP), digits = 2),
            MEDIA_5_MT=round(mean(MEDIA_5_MT), digits = 2),
            MEDIA_9_LP=round(mean(MEDIA_9_LP), digits = 2),
            MEDIA_9_MT=round(mean(MEDIA_9_MT), digits = 2))



DT::datatable(tb1,
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))


#####################################
############Ramos e Folhas###########
#####################################

tb <- df3 %>% filter(UF=="MG" &
                       Ano==2019)

print(stem(tb$MEDIA_5_LP))


#####################################
############Gráficos de Pizza########
#####################################

EF1 <- df3 %>% filter(Ano==2019)

EF2 <- EF1 %>% filter(UF=="MG")
EF3 <- EF2 %>%
  filter(MUNICIPIO == "VICOSA") %>%
  as.data.frame() %>% select(c(1,3:6))
names(EF3) <- c("Municipio",
                "LP - 5° Ano",
                "MT - 5° Ano",
                "LP - 9° Ano",
                "MT - 9° Ano")
EF4 <- EF3 %>%
  gather(key = "Provas", value = "Notas", -Municipio)


EF5 <- EF4 %>%
  arrange(desc(Notas)) %>%
  mutate(prop = Notas / sum(EF4$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
EF5 <- EF5[EF5$Notas>0,]

plotly::plot_ly(EF5,
        values=~Notas,
        marker = list(line = list(color = '#FFFFFF',
                                  width = 1)),
        type="pie",
        textposition = "outside",
        textinfo = 'text',
        hoverinfo = 'text',
        source = "subset",
        text=~paste0("<br>",Provas,
                     "<br>",Notas),
        insidetextfont = list(color = '#FFFFFF'),
        textfont = list(color = "black", size = 20)) %>%
  plotly::layout(showlegend = FALSE,separators = ',.') %>%
  plotly::config(displayModeBar = F)





##############################################################
############Pagina Ensino Medio
#######################################################
df1 <- readRDS("DadosTransferencias/transferencias.Rds")
df2 <- readRDS("DadosENEM/ENEM.Rds")
df3 <- readRDS("DadosSAEB/SAEB.Rds")
Pop <- readRDS("DadosPopulacionais/PopEstadoMUN.RDS")


#####Grafico de colunas ou barras

EM1 <- df1 %>% filter(Ano=="2020")
EM2 <- EM1 %>% filter(UF=="MG")
EM3 <- EM2 %>%
  filter(MUNICIPIO == "VICOSA") %>%
  group_by(LINGUAGEM_CIDADA) %>%
  summarise(VALOR = sum(VALOR))
plot3 <- ggplot(EM3,
                aes(x=factor(LINGUAGEM_CIDADA),
                    y = VALOR,
                    fill = LINGUAGEM_CIDADA,
                    text = paste("Programa:",LINGUAGEM_CIDADA,
                                 "<br>",
                                 "Valor:",paste0("R$",formatC(VALOR, digits = 2, big.mark=',', format = 'f')),
                                 "<br>",
                                 "Município:", "Viçosa"))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=VALOR),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("LINGUAGEM CIDADA") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valor Total Recebido = R$ ", sum(EM3$VALOR)))


plotly::ggplotly(plot3, tooltip = "text") %>%
  plotly::layout(showlegend = FALSE) #%>%
#plotly::style(textposition = "top")



######Tabela de Dupla Entrada

DT::datatable(EM3,
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))


DP1 <- df2 %>% filter(ANO=="2019")
DP2 <- DP1 %>%
  filter(UF == "MG") %>%
  select(-c(UF, ANO))
DT::datatable(DP2,
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))


##########Diagrama de Ramos e Folhas
tb <- df2 %>% filter(UF=="MG" &
                       ANO=="2019")
print(stem(tb$NOTA_CN))


#####################################
############Gráficos de Pizza########
#####################################


tb1 <- df2 %>% filter(ANO=="2019")
tb2 <- tb1 %>% filter(UF=="MG")

tb3 <- tb2 %>%
  filter(MUNICIPIO == "VICOSA") %>%
  as.data.frame() %>% select(c(1,4:8))

names(tb3) <- c("Municipio",
                "Ciências da Natureza",
                "Ciências Humanas",
                "Linguagens",
                "Matemática",
                "Redação")

tb4 <- tb3 %>%
  gather(key = "Provas", value = "Notas", -Municipio)

tb5 <- tb4 %>%
  arrange(desc(Notas)) %>%
  mutate(prop = Notas / sum(tb4$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
tb5 <- tb5[tb5$Notas>0,]

plotly::plot_ly(tb5,
        values=~Notas,
        marker = list(line = list(color = '#FFFFFF',
                                  width = 1)),
        type="pie",
        textposition = "outside",
        textinfo = 'text',
        hoverinfo = 'text',
        source = "subset",
        text=~paste0("<br>",Provas,
                     "<br>",Notas),
        insidetextfont = list(color = '#FFFFFF'),
        textfont = list(color = "black", size = 15)) %>%
  plotly::layout(showlegend = FALSE,separators = ',.') %>%
  plotly::config(displayModeBar = F)


############histograma

teste <- df2 %>%
  filter(ANO=="2019" & UF == "MG") %>%
  select(-c(MUNICIPIO, UF, ID, NCand, ANO,COMP1,COMP2,COMP3,COMP4,COMP5))
names(teste) <- c("Ciências da Natureza",
                  "Ciências Humanas",
                  "Linguagens",
                  "Matemática",
                  "Redação",
                  "Média Geral")
teste2 <- teste %>%
  gather(key = "Provas", value = "Notas")

bx <- teste2 %>% ggplot(aes(Provas, Notas, fill = Provas)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("Boxplot")

plotly::ggplotly(bx) %>%
  plotly::layout(showlegend = FALSE)



###########################################################################
########  Outros Graficos
###########################################################################

rm(list = ls())
cat("\014")
###############################################################
df1 <- readRDS("DadosTransferencias/transferencias.Rds")
df2 <- readRDS("DadosENEM/ENEM.Rds")
df3 <- readRDS("DadosSAEB/SAEB.Rds")
Pop <- readRDS("DadosPopulacionais/PopEstadoMUN.RDS")
##############################################################



#Recurso por Estado

teste <- df1 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Valor=round((sum(VALOR))/1000000000, digits = 5),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Valor,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Valor),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos do Governo Federal \n no ano de 2019"),
          subtitle = paste0("Valores Recebidos em Bilhões de Reais (x", 10^9,")")) #+
# geom_label(
#   label="Valores em Bilhões de Reais",
#   x="RN",
#   y=4.5,
#   label.padding = unit(0.55, "lines"), # Rectangle size around label
#   label.size = 0.35,
#   color = "black",
#   fill="#69b3a2"
# )

## Boxplot

teste <- df1 %>% filter(VALOR<=1000000 & Ano == 2019)

teste %>% ggplot(aes(UF, VALOR, fill = UF)) +
  geom_boxplot(show.legend = FALSE)

## Boxplot

teste <- df2 %>% filter(ANO == 2019)

teste %>% ggplot(aes(UF, NOTA_CN, fill = UF)) +
  geom_boxplot(show.legend = FALSE)

###Treemap

Recursos <-df1 %>%
  filter(UF == "RJ" & Ano == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Valor = sum(VALOR))

Enem <-df2 %>%
  filter(UF == "RJ" & ANO == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Nota = mean(NT))

df <- inner_join(Recursos, Enem, by = "MUNICIPIO")

df$escala <- scale(df$Nota) #necessário para criar valores negativos para deixar as disparidades mais evidentes
library("treemap")
x <- treemap(df, index = "MUNICIPIO", vSize = "Valor", vColor = "escala",
             type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
             title  =  "Treemap de Recursos Recebidos e Notas do ENEM",
             overlap.labels=0.2)


#Nota Media ENEM por Estado

teste <- df2 %>%
  filter(ANO==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(NT), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média do ENEM no ano de 2019"),
          subtitle = "Soma de Todas as Notas dividido por 5")

#### Buble Chart

Pop$POPULACAO <- as.numeric(gsub(".", "", Pop$POPULACAO, fixed = TRUE))
Pop$MUNICIPIO <- str_trim(Pop$MUNICIPIO)
Pop$MUNICIPIO <- str_to_upper(Pop$MUNICIPIO)
Pop$MUNICIPIO <- abjutils::rm_accent(Pop$MUNICIPIO)
names(df1) <- str_to_upper(names(df1))
Recursos <-df1 %>%
  filter(UF == "MG" & ANO == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Valor = round(sum(VALOR), digits = 2))

Enem <-df2 %>%
  filter(UF == "MG" & ANO == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Nota = mean(NT))
Pop2 <- Pop %>%
  filter(UF == "MG")
df <- inner_join(Recursos, Enem, by = "MUNICIPIO")
df <- inner_join(df, Pop2, by = "MUNICIPIO")

df$PerCapita <- round(df$Valor/df$POPULACAO, digits = 2)

df <- df %>% filter(MUNICIPIO!= "RIO DE JANEIRO")
radius <- sqrt(df$POPULACAO/pi)
symbols(df$Valor,
        df$Nota,
        circles=radius,
        inches=0.35,
        fg="white",
        bg="red",
        xlab="Valor Recebido",
        ylab="Nota") +
  text(df$Valor, df$Nota, df$MUNICIPIO, cex=0.5)

colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

# Using Plotly
#library(plotly)
plotly::plot_ly(
  df, x = ~`Nota`, y = ~`PerCapita`,
  color = ~`MUNICIPIO`, type = "scatter",
  mode="markers", size=~`radius`,
  marker = list(symbol = 'circle', sizemode = 'diameter',
                line = list(width = 2, color = '#FFFFFF'),
                opacity=0.4),
  text = ~paste('Município:', `MUNICIPIO`,
                '<br>UF:', `UF`,
                '<br>Nota:', round(`Nota`,2),
                '<br>Valor Total Recebido:', round(`Valor`,2),
                '<br>Population:',POPULACAO,
                '<br>Valor Per-Capita:',PerCapita)) %>%
  plotly::layout(showlegend = FALSE)

#Nota_CN ENEM por Estado

teste <- df2 %>%
  filter(ANO==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(NOTA_CN), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Ciências da Natureza no ano de 2019"))

#Candidatos ENEM por Estado

teste <- df2 %>%
  filter(ANO==2019) %>%
  group_by(UF) %>%
  summarise(Cand=round(sum(NCand), digits = 0),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Cand,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Cand),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Número de Candidatos") +
  theme_light() +
  ggtitle(paste0("Número de Candidatos por Estado no ano de 2019"))

#Media de Idade por Estado

teste <- df2 %>%
  filter(ANO==2019) %>%
  group_by(UF) %>%
  summarise(Idade=round(mean(ID), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Idade,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Idade),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Idade Média dos Candidatos") +
  theme_light() +
  ggtitle(paste0("Idade Média dos Candidatos por Estado no ano de 2019"))

#################################
# UF <- fread("UF.csv")
# names(UF) <- c("UF", "Sigla")
# UF$UF <- str_trim(UF$UF)
# UF$UF <- str_to_upper(UF$UF)
# UF$UF <- abjutils::rm_accent(UF$UF)
# df3$UF <- str_trim(df3$UF)
# df3$UF <- str_to_upper(df3$UF)
# df3$UF <- abjutils::rm_accent(df3$UF)
# df3 <- inner_join(df3, UF, by = "UF")
# df3 <- df3 %>% select(1,8, 3:7)
# names(df3) <- c("MUNICIPIO",
#                 "UF",
#                 "MEDIA_5_LP",
#                 "MEDIA_5_MT",
#                 "MEDIA_9_LP",
#                 "MEDIA_9_MT",
#                 "ANO")
# saveRDS(df3, "saeb.Rds")


#Nota Media LP 5 SAEB por Estado

str(df3$MEDIA_5_LP)

teste <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(MEDIA_5_LP), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Lingua Portuguesa do Quinto Ano \n do Saeb no ano de 2019"))


#Nota Media LP 9 SAEB por Estado (Cidades com Nota diferente de zero)

teste <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  filter(MEDIA_9_LP!= 0) %>%
  summarise(Nota=round(mean(MEDIA_9_LP), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Lingua Portuguesa do Nono Ano \n do Saeb no ano de 2019"))


# Num. de Cidades que nao responderam LP 9 SAEB por Estado (Cidades com Nota igual a zero)

teste <- df3 %>%
  filter(Ano==2017) %>%
  group_by(UF) %>%
  filter(MEDIA_9_LP== 0) %>%
  summarise(Count=n(),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Count,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Count),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Quantidade") +
  theme_light() +
  ggtitle(paste0("Número de Cidades que Não Tiveram Avaliação de Lingua Portuguesa no ano de 2019"))

#########################################################

#Valores Recebidos por Municipio

teste <- df1 %>%
  filter(MUNICIPIO == "LAJINHA" & ANO == 2019)

ggplot(teste,
       aes(x=factor(LINGUAGEM_CIDADA),
           y = VALOR,
           fill = LINGUAGEM_CIDADA)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=VALOR),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("LINGUAGEM CIDADA") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valor Total Recebido = R$ ", sum(teste$VALOR)))

#Nota do Enem por Municipio

teste <- df2 %>%
  filter(MUNICIPIO == "LAJINHA" & ANO == 2019)

teste <- reshape2::melt(teste[c(1,4:8)])

ggplot(teste,
       aes(x=factor(variable),
           y = value,
           fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Provas do ENEM") +
  ylab("Nota") +
  theme_light() +
  ggtitle(paste0("A Nota Média no ENEM foi de ", mean(teste$value)))

#Nota do Enem por Municipio

teste <- df2 %>%
  filter(ANO == 2019)

teste <- teste %>%
  filter(UF == "MG")

teste <- teste %>%
  filter(MUNICIPIO == "LAJINHA") %>%
  as.data.frame() %>% select(c(1,4:8))

names(teste) <- c("Municipio",
                  "Ciências da Natureza",
                  "Ciências Humanas",
                  "Linguagens",
                  "Matemática",
                  "Redação")

#teste <- reshape2::melt(teste[c(1,4:8)])
#teste <- teste[c(1,4:8)]
teste <- teste %>%
  gather(key = "Provas", value = "Notas", -Municipio)

# Compute the position of labels
teste <- teste %>%
  arrange(desc(Notas)) %>%
  mutate(prop = Notas / sum(teste$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(teste, aes(x="", y=prop, fill=Provas)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos,
                label = paste0(Provas, "\n value: ", Notas)), nudge_x = 0.2, color = "black", size=6) +
  scale_fill_brewer(palette="Set1")

plotly::plot_ly(teste,
        values=~Notas,
        marker = list(line = list(color = '#FFFFFF',
                                  width = 1)),
        type="pie",
        textposition = "outside",
        textinfo = 'text',
        hoverinfo = 'text',
        source = "subset",
        text=~paste0("<br>",Provas,
                     "<br>",Notas),
        insidetextfont = list(color = '#FFFFFF'),
        textfont = list(color = "black", size = 20)) %>%
  plotly::layout(showlegend = FALSE,separators = ',.') %>%
  plotly::config(displayModeBar = F)

#plotly::ggplotly(pie)

#Nota do Saeb por Municipio

teste <- df3 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019) %>%
  as.data.frame()

teste <- reshape2::melt(teste[c(1,3:6)])

ggplot(teste,
       aes(x=factor(variable),
           y = value,
           fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Provas do ENEM") +
  ylab("Nota") +
  theme_light() +
  ggtitle(paste0("A Nota Média no ENEM foi de ", mean(teste$value)))

###

# Create test data.

teste <- df3 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019) %>%
  as.data.frame()

teste <- reshape2::melt(teste[c(1,3:4)])

# Compute percentages
teste$fraction = teste$value / sum(teste$value)

# Compute the cumulative percentages (top of each rectangle)
teste$ymax = cumsum(teste$fraction)

# Compute the bottom of each rectangle
teste$ymin = c(0, head(teste$ymax, n=-1))

# Compute label position
teste$labelPosition <- (teste$ymax + teste$ymin) / 2

# Compute a good label
teste$label <- paste0(teste$variable, "\n value: ", teste$value)

# Make the plot
ggplot(teste, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


#Nota do Saeb por Municipio

teste <- df3 %>%
  filter(Ano == 2017)

teste <- teste %>%
  filter(UF == "AC")

teste <- teste %>%
  filter(MUNICIPIO == "ASSIS BRASIL") %>%
  as.data.frame() %>% select(c(1,3:6))

names(teste) <- c("Municipio",
                  "LP - 5° Ano",
                  "MT - 5° Ano",
                  "LP - 9° Ano",
                  "MT - 9° Ano")

#teste <- reshape2::melt(teste[c(1,4:8)])
#teste <- teste[c(1,4:8)]
teste <- teste %>%
  gather(key = "Provas", value = "Notas", -Municipio)

teste <- teste[teste$Notas>0,]


# Compute the position of labels
teste <- teste %>%
  arrange(desc(Notas)) %>%
  mutate(end_angle = 2*pi*cumsum(Notas)/sum(teste$Notas),      # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle))   %>% # middle of each pie slice, for the text label
  mutate(prop = Notas / sum(teste$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>%
  mutate(hjust = ifelse(mid_angle>pi, 1, 0),
         vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))

rpie = 1 # pie radius
rlabel = 0.6 * rpie # radius of the labels; a number slightly larger than 0.5 seems to work better,
# but 0.5 would place it exactly in the middle as the question asks for.

library(ggforce)
# Basic piechart
rlabel = 1.05 * rpie # now we place labels outside of the pies

ggplot(teste) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = Provas)) +
  geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = paste0(Provas, "\n value: ", Notas),
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL)




###########

teste <- df2 %>%
  filter(ANO==2019 & UF == "MG") %>%
  select(-c(MUNICIPIO, UF, ID, NCand, ANO,COMP1,COMP2,COMP3,COMP4,COMP5))
names(teste) <- c("Ciências da Natureza",
                  "Ciências Humanas",
                  "Linguagens",
                  "Matemática",
                  "Redação",
                  "Média Geral")


teste2 <- teste %>%
  gather(key = "Provas", value = "Notas")

bx <- teste2 %>% ggplot(aes(Provas, Notas, fill = Provas)) +
  geom_boxplot(show.legend = FALSE)

plotly::ggplotly(bx)
##################################

teste <- df1 %>% filter(ANO==2019 &
                          MUNICIPIO=="ABAIARA") %>%
  ungroup() %>%
  select(c(MUNICIPIO, LINGUAGEM_CIDADA, VALOR)) %>%
  group_by(MUNICIPIO, LINGUAGEM_CIDADA) %>%
  summarise(VALOR=sum(VALOR),
            .groups = 'drop')

teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  plotly::plot_ly(x = ~variable,
          y = ~VALOR,
          type = 'bar',
          name = ~value,
          hovertemplate = "Valor Recebido: R$ %{y}",
          text = ~VALOR,
          textfont = list(size = 15),
          textposition = "outside") %>%
  plotly::layout(yaxis = list(title = 'Valor Recebido'),
         xaxis = list(title = 'Variável'),
         barmode = 'stack')

teste %>%
  plotly::plot_ly() %>%
  plotly::add_bars(x = ~LINGUAGEM_CIDADA,
           y = ~VALOR,
           type = 'bar',
           name = ~LINGUAGEM_CIDADA,
           hovertemplate = "Valor Recebido: %{y}"
  ) %>%
  ## Not working
  #    add_text(x = ~Animals, y = ~value,
  #             text = "gotcha", textposition = "top center") %>%
  plotly::layout(yaxis = list(title = 'Valor Recebido'), barmode = 'stack')

df <- read.table(text=
                   "Rank F1     F2     F3
1    500    250    50
2    400    100    30
3    300    155    100
4    200    90     10", header=TRUE)

df %>%
  gather(variable, value, F1:F3) %>%
  ggplot(aes(x = Rank, y = value, fill = variable)) +
  geom_bar(stat = "identity")

teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  ggplot(aes(x = variable, y = VALOR, fill = value)) +
  geom_bar(stat = "identity")


teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  plotly::plot_ly(x = ~variable,
          y = ~VALOR,
          type = 'bar',
          name = ~value,
          hovertemplate = "Valor Recebido: R$ %{y}",
          text = ~VALOR,
          textfont = list(size = 15),
          textposition = "outside") %>%
  plotly::layout(yaxis = list(title = 'Valor Recebido'),
         xaxis = list(title = 'Variável'),
         barmode = 'stack') #%>%
# add_annotations(
#   x = ~variable,
#   y = ~VALOR,
#   text = ~value,
#   showarrow = F,
#   textfont = list(size = 10)
# )
###################################

City<-c("X","Y","Z","X","Z","X","Y")
House_Unit_Id<-c("H1","H2","H3","H4","H5","H6","H7")
Adult<-c(50,100,60,40,50,80,60)
Child<-c(40,0,40,20,50,20,30)
Baby<-c(10,0,0,40,0,0,10)
data<-data.frame(City,House_Unit_Id,Adult,Child,Baby)

library(plyr)
# Changing the data frame before plotting ... there is propably an easier way to do this!
newdata <- ldply(3:5,function(n){tempdata <- data[,c(1,n)]
colnames(tempdata)[2] <- "Number"
tempdata$type <- colnames(data[n])
return(tempdata)})
newdata <- ddply(newdata,.(City,type),summarize,Number=sum(Number))
# Total for each city
datatotal <- ddply(newdata,~City,summarize,n=sum(Number))
# Merge the data frames together
newdata <- merge(newdata,datatotal)
# Calc the percentages
newdata$perc <- newdata$Number/newdata$n

plotly::plot_ly(newdata,x = ~City, y = ~perc*100, type = 'bar',color = ~type,text=~Number,hoverinfo = 'text') %>%
  plotly::layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack")

teste <- df1 %>% filter(UF=="MG")
teste <- teste %>%
  filter(MUNICIPIO=="LAJINHA") %>%
  group_by(MUNICIPIO, ANO) 



teste <- teste %>% 
  group_by(MUNICIPIO, ANO) %>% 
  dplyr::summarise(Valor=round((sum(VALOR)), digits = 5),
            .groups = 'drop')

minimo <- min(unique(teste$ANO))
maximo <- max(unique(teste$ANO))

plot1 <- ggplot(teste,
                aes(x=factor(ANO),
                    y = Valor,
                    fill = factor(ANO))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=paste0("R$ ",Valor)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("ANO") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos de ",minimo," a ", maximo, " por ",unique(teste$MUNICIPIO)))

plotly::ggplotly(plot1) %>%
  plotly::layout(showlegend = FALSE) %>%
  plotly::style(textposition = "top")

df <- df1
dfPM <- df %>% filter(MUNICIPIO %in% c("PARA DE MINAS", "FLORESTAL", "ITAUNA", "CONCEICAO DO PARA"))

#names(df) <- c("AnoMes","TIPO","Uf","MUNICIPIO","Orgao","Programa","PlanoOrcamentario","Favorecido","ValorTransferido")
dfPM <- df %>% filter(MUNICIPIO %in% c("PARA DE MINAS", "FLORESTAL", "CONCEICAO DO PARA", "ITAUNA"))
table(dfPM$MUNICIPIO, dfPM$VALOR)

tot <- sum(dfPM$VALOR)

Tot <- dfPM %>% group_by(MUNICIPIO) %>% summarise(Total=sum(VALOR), .groups = 'drop')
#criar gráficos comparando os valores recebidos no mês de janeiro de 5 cidades da região
#usar gráficos do ggplot2

options(scipen = 999)
Tot <- dfPM %>% group_by(MUNICIPIO) %>% dplyr::summarise(Total=sum(VALOR))
plot1 <- ggplot(data = Tot, aes(x = MUNICIPIO,
                                y = Total,
                                fill = MUNICIPIO,
                                label = Total)) +
  geom_col(show.legend = FALSE, position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_classic()
plotly::ggplotly(plot1)
################################################################################
plot2 <- ggplot(data = Tot, aes(x = MUNICIPIO,
                                y = Total,
                                fill = MUNICIPIO,
                                label = Total,
                                text = paste("MUNICIPIO:", MUNICIPIO, "<br>",
                                             "Total:", Total))) +
  geom_bar(stat = "identity", show.legend = FALSE, position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_classic()+
  coord_flip()
plotly::ggplotly(plot2, tooltip = "text") %>% plotly::layout (showlegend = FALSE)
################################################################################
plot3 <- ggplot(data = Tot, aes(x = MUNICIPIO,
                                y = Total,
                                fill = MUNICIPIO,
                                label = Total,
                                text = paste("MUNICIPIO:", MUNICIPIO, "<br>",
                                             "Total:", Total))) +
  geom_point(stat = "identity", show.legend = FALSE, position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_classic()+
  coord_flip()
plotly::ggplotly(plot3, tooltip = "text") %>% plotly::layout (showlegend = FALSE)

################################################################################

#options(scipen = 999)
Tot %>% ggplot(aes(MUNICIPIO,
                   Total,
                   fill = MUNICIPIO,
                   label = Total)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5)+
  theme_minimal()



ggplot(data=Tot, aes(x=MUNICIPIO, y=Total)) +
  geom_bar(stat="identity")
