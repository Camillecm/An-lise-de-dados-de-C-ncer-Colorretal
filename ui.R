library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(survival)
library(flexsurv)
library(plotly)
library(zoo)
library(patchwork)
library(knitr)
library(forecast)
library(LaplacesDemon)
library(datasets)
library(tidyverse)
library(modeest)
library(gridExtra)
library(ggpubr)
library(forecast)
library(HelpersMG)
library(survminer)
library(qqplotr)
library(EstimationTools)

#setwd("G:/Meu Drive/TCC/App")

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "Câncer colorretal", titleWidth = 258,
                                    dropdownMenu(type = "notifications", 
                                                 headerText = strong("INFO"), 
                                                 icon = icon("info"), 
                                                 badgeStatus = NULL,
                                                 notificationItem(
                                                   text = "Pacientes diagnosticados entre 1996 e 1998.",
                                                   tags$i(class = "fa-solid fa-user-group")
                                                 ),
                                                 notificationItem(
                                                   text = "Hospital de Bellvitge",
                                                   icon = icon("map")
                                                 ),
                                                 notificationItem(
                                                   text = "Universidade Federal da Bahia",
                                                   icon = icon("map")
                                                 ),
                                                 notificationItem(
                                                   text = "por Camille Menezes P dos Santos",
                                                   icon = icon("address-card"))
                                    ),
                                    tags$li(a(img(src = "search.png", height = 20),
                                              height = 20, href = "https://drive.google.com/file/d/1cKA_97WjX7tdiJ4U8pbHEuojIk2xkwkt/view?usp=sharing",
                                              title = "", target = "_blank"),
                                            class = "dropdown"),
                                    tags$li(
                                      a(img(src = "ufba.png", height = 20),
                                        height = 20, title = "",
                                        target = "_blank"),
                                      class = "dropdown")),
                    
                    dashboardSidebar(width = 258,
                                     sidebarMenu(menuItem(icon=icon("info"),"Informações", tabName="info"),
                                                 menuItem(tags$div(tags$i(class="fa-solid fa-chart-pie"),tags$span("Análise descritiva")),tabName = "descritiva"),
                                                 menuItem(tags$div(tags$i(class="fa-solid fa-chart-line"),tags$span("Modelos de fragilidade")),
                                                          menuItem("Abordagem frequentista", 
                                                                   menuItem("Estimativas", tabName = "freqreg"),
                                                                   menuItem("Resíduos e comparação", tabName = "freqreg1")),
                                                          menuItem("Abordagem Bayesiana",
                                                                   menuItem("Estimativas", tabName = "bayesreg"),
                                                                   menuItem("Diagnóstico MCMC", tabName = "bayesreg1"),
                                                                   menuItem("Resíduos e comparação", tabName = "bayesreg2")))
                                                 )),
                    
                    dashboardBody(
                      
#########info###################################################################

tabItems(tabItem(tabName = "info",
                 box(width=12,
                     h1("Modelo de Sobrevivência Bayesiano para Dados de Câncer Colorretal")),
                 box(width=12,
                     h4("Painel construído durante o Trabalho de Conclusão de Curso II, 
       sob a orientação da profa. dra. Maristela Dias de Oliveira, realizado no Instituto
       de Matemática e Estatística da Universidade Federal da Bahia acerca 
       de um Modelo de Sobrevivência Bayesiano para Dados de Câncer Colorretal.", align="justify"),
                     
                     h4("O câncer colorretal é uma doença grave, que pode ser fatal. 
                     Por isso, este estudo tem como objetivo analisar o tempo até as reinternações 
                     de pacientes diagnosticados com câncer colorretal e submetidos à cirurgia de remoção do tumor. 
                     Para isso, foram empregadas técnicas de Análise de Sobrevivência que incorporam covariáveis na 
                     presença de eventos recorrentes: o modelo de fragilidade compartilhada gama.
                     Neste painel, são apresentados os resultados dessa modelagem.", align="justify"),
                     
                     h4("Não é razoável supor que os tempos de reinternação de um mesmo paciente sejam independentes. 
                        Por esse motivo, neste modelo, um efeito aleatório (fragilidade) é introduzido na função da taxa 
                        de falha para descrever a possível associação entre os tempos de um mesmo paciente. Além disso, 
                        ao contrário de estudos anteriores em análise de sobrevivência sobre câncer colorretal, o modelo 
                        foi ajustado utilizando as abordagens frequentista e Bayesiana.", align="justify"),

                     h4("Os dados são de 403 pacientes acompanhados no hospital de Bellvitge, localizado 
na região metropolitana de Barcelona, na Espanha. Para esses esses pacientes, 
foram registradas 861 reinternações devido à recorrência do câncer colorretal.
As covariáveis incluem o sexo do paciente, se o paciente recebeu tratamento 
quimioterápico ou não, estágio tumoral de Dukes e índice de comorbidade de Charlson.", align="justify"),

                     h4("Inicialmente, foi realizada uma análise descritiva, que revelou que há diferenças 
significativas entre as curvas de sobrevivência estimadas das covariáveis 
quimioterapia, estágio tumoral de Dukes e índice de comorbidade de Charlson. 
Em seguida, foram aplicadas as abordagens frequentista e Bayesiana para modelar 
o tempo até a primeira reinternação. O estudo do tempo até a primeira reinternação desses pacientes foi realizado 
                        no TCC I, os resultados dessa modelagem podem ser encontrados em: https://camille-menezes.shinyapps.io/Cancer_Colorretal_TCC1/.", align="justify"),
                     
                     h4("No modelo de fragilidade compartilhada gama, foram estimados modelos com todas as covariáveis 
                     (completo) e sem a covariável quimioterapia (reduzido). Os modelos reduzidos apresentaram melhores ajustes e resultados
                     semelhantes foram obtidos para as duas abordagens: o risco de reinternação é 
                        maior para pacientes do sexo masculino, com estágios tumorais mais avançados e índices de 
                        comorbidade mais elevados. O modelo também indicou que os tempos de um mesmo indivíduo não 
                        podem ser considerados independentes e há heterogeneidade entre os tempos de diferentes pacientes.", align="justify"),
 
                     h4("Na abordagem Bayesiana, utilizando o algoritmo Adaptative Metropolis-Within-Gibbs, 
                     as amostras da distribuição a posteriori mostraram sinais de convergência. 
                     Para as duas abordagens, os resíduos quantílicos evidenciaram a adequabilidade dos
                     modelos e as conclusões obtidas neste trabalho estão em 
concordância com as conclusões obtidas de pesquisas que utilizaram o mesmo conjunto de dados.", align="justify")
                 ),
                 h4("Elaborado por: Camille Menezes P. dos Santos"),
                 h5("Email p/ contato: camillemennezes@gmail.com")
),

#############descritiva#########################################################                               

tabItem(tabName = "descritiva",
        fluidRow(
          box(width=12,
              valueBoxOutput(width = 3, outputId = "quimio"),
              valueBoxOutput(width = 3, outputId = "sexo"),
              valueBoxOutput(width = 3, outputId = "tumoral"),
              valueBoxOutput(width = 3, outputId = "comorbidade")
          )),
        fluidRow(
          tabBox(tabPanel("Quimioterapia", h4("Gráfico de pizza para a covariável 
                 quimioterapia do conjunto de dados de pacientes diagnosticados com câncer colorretal no Hospital de Bellvitge 
                 entre 1996 e 1998.",align="center"),plotlyOutput("Plot39")),
                 tabPanel("Sexo",h4("Gráfico de pizza para a covariável 
                 sexo do conjunto de dados de pacientes diagnosticados com câncer colorretal no Hospital de Bellvitge 
                 entre 1996 e 1998.",align="center"),plotlyOutput("Plot1")),
                 tabPanel("Est. tumoral", h4("Gráfico de pizza para a covariável 
                 estágio tumoral de Dukes do conjunto de dados de pacientes diagnosticados com câncer colorretal no Hospital de Bellvitge 
                 entre 1996 e 1998.",align="center"),plotlyOutput("Plot40")),
                 tabPanel("Índ. de comorbidade",h4("Gráfico de pizza para a covariável 
                 índice de comorbidade de Charlson, conforme os tempos até as reinternações, do conjunto de dados de pacientes diagnosticados com câncer colorretal no Hospital de Bellvitge 
                 entre 1996 e 1998.",align="center"),plotlyOutput("Plot41"),
                          radioGroupButtons(inputId = "Wid13", label = "", choices = c("1º reint.","2º reint.","3º reint.","4º reint.","5º reint.","6º reint."), justified = TRUE)),
          ),
          tabBox(
            tabPanel("Quimioterapia", h4("Curvas de sobrevivência estimadas pelo método Kaplan-Meier para 
                   a covariável quimioterapia, conforme os tempos até as reinternações, do conjunto de dados de pacientes diagnosticados 
                   com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot2"),
                     radioGroupButtons(inputId = "Wid14", label = "", choices = c("1º reint.","2º reint.","3º reint."), justified = TRUE)),
            tabPanel("Sexo", h4("Curvas de sobrevivência estimadas pelo método Kaplan-Meier para 
                   a covariável sexo, conforme os tempos até as reinternações, do conjunto de dados de pacientes diagnosticados 
                   com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot36"),
                     radioGroupButtons(inputId = "Wid15", label = "", choices = c("1º reint.","2º reint.","3º reint."), justified = TRUE)),
            tabPanel("Est. tumoral", h4("Curvas de sobrevivência estimadas pelo método Kaplan-Meier para 
                   a covariável estágio tumoral de Dukes, conforme os tempos até as reinternações, do conjunto de dados de pacientes diagnosticados 
                   com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot37"),
                     radioGroupButtons(inputId = "Wid16", label = "", choices = c("1º reint.","2º reint.","3º reint."), justified = TRUE)),
            tabPanel("Índ. de comorbidade", h4("Curvas de sobrevivência estimadas pelo método Kaplan-Meier para 
                   a covariável índice de comorbidade de Charlson, conforme os tempos até as reinternações, do conjunto de dados de pacientes diagnosticados 
                   com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot38"),
                     radioGroupButtons(inputId = "Wid17", label = "", choices = c("1º reint.","2º reint.","3º reint."), justified = TRUE)),
          ),
          box(title = h4("Tempos até as reinternações ou censura dos pacientes dos dados de pacientes diagnosticados com 
                         câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot42")),
          box(title = h4("Gráfico TTT para os tempos até as reinternações dos pacientes diagnosticados 
                         com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot47"))
        )),

#############freqreg############################################################

tabItem(tabName = "freqreg",
        fluidRow(
          box(width=12,
              valueBoxOutput(width = 3, outputId = "box11"),
              valueBoxOutput(width = 3, outputId = "box12"),
              valueBoxOutput(width = 3, outputId = "box13"),
              valueBoxOutput(width = 3, outputId = "box14")
          )),
        fluidRow(
          tabBox(width=12,
                 tabPanel("Completo", h4("Estimativas dos parâmetros do modelo de fragilidade gama completo, 
                                         ajustado a partir da abordagem frequentista, para os dados de pacientes 
                                         diagnosticados com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot27")),
                 tabPanel("Reduzido", h4("Estimativas dos parâmetros do modelo de fragilidade gama reduzido, 
                                         ajustado a partir da abordagem frequentista, para os dados de pacientes 
                                         diagnosticados com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot28")))
        )),

#############freqreg1###########################################################

tabItem(tabName = "freqreg1",
        fluidRow(box(width=12,
                     valueBoxOutput(width = 4, outputId = "box24"),
                     valueBoxOutput(width = 4, outputId = "box22"),
                     valueBoxOutput(width = 4, outputId = "box23")
        )),
        fluidRow(
          tabBox(tabPanel("Completo", h4("Gráfico de dispersão dos resíduos quantílicos do modelo de regressão 
                                         completo de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         frequentista, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot31")),
                 tabPanel("Reduzido", h4("Worm plot dos resíduos quantílicos do modelo de regressão 
                                         completo de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         frequentista, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot32"))
          ),
          tabBox(tabPanel("Completo", h4("Gráfico de dispersão dos resíduos quantílicos do modelo de regressão 
                                         reduzido de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         frequentista, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot33")),
                 tabPanel("Reduzido", h4("Worm plot dos resíduos quantílicos do modelo de regressão 
                                         reduzido de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         frequentista, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot32.1"))
          ))),

#############bayesreg###########################################################

tabItem(tabName = "bayesreg",
        fluidRow(
          box(width=12,
              valueBoxOutput(width = 3, outputId = "box15"),
              valueBoxOutput(width = 3, outputId = "box16"),
              valueBoxOutput(width = 3, outputId = "box17"),
              valueBoxOutput(width = 3, outputId = "box18")
          )),
        fluidRow(
          tabBox(width=12,
                 tabPanel("Completo", h4("Estimativas dos parâmetros do modelo de fragilidade gama completo, 
                                         ajustado a partir da abordagem Bayesiana, para os dados de pacientes 
                                         diagnosticados com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot29")),
                 tabPanel("Reduzido", h4("Estimativas dos parâmetros do modelo de fragilidade gama reduzido, 
                                         ajustado a partir da abordagem Bayesiana, para os dados de pacientes 
                                         diagnosticados com câncer colorretal no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot30")))
        )),

#############bayesreg1##########################################################

tabItem(tabName = "bayesreg1",
        fluidRow(box(width=12,
                     valueBoxOutput(width = 2, outputId = "box6"),
                     valueBoxOutput(width = 2, outputId = "box7"),
                     valueBoxOutput(width = 2, outputId = "box8"),
                     valueBoxOutput(width = 3, outputId = "box9"),
                     valueBoxOutput(width = 3, outputId = "box10")
        )),
        fluidRow(
          tabBox(tabPanel("Completo", h4("Erro padrão de monte carlo (MCSE) e tamanho efetivo da amostra
                 (ESS) das distribuições a posteriori geradas a partir do algoritmo 
                 Adaptative Metropolis-within-Gibbs para o modelo de regressão completo de fragilidade compartilhada gama
                 para os dados de pacientes diagnosticados com câncer colorretal 
                 no Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot34")),
                 tabPanel("Reduzido", h4("Erro padrão de monte carlo (MCSE) e tamanho efetivo da amostra
                 (ESS) das distribuições a posteriori geradas a partir do algoritmo 
                 Adaptative Metropolis-within-Gibbs para o modelo de regressão reduzido de fragilidade compartilhada gama
                 para os dados de pacientes diagnosticados com câncer colorretal 
                 no Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotlyOutput("Plot35"))
          ),
          tabBox(tabPanel("β0", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β0 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot14"),radioGroupButtons(
                   inputId = "Wid4", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β1", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β1 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot15"),radioGroupButtons(
                   inputId = "Wid5", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β2", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β2 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot16"),radioGroupButtons(
                   inputId = "Wid6", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β3", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β3 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot17"),radioGroupButtons(
                   inputId = "Wid7", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β4", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β4 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot18"),radioGroupButtons(
                   inputId = "Wid8", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β5", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β5 a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot19"),radioGroupButtons(
                   inputId = "Wid9", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("β6", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de β6 a partir do algoritmo Adaptative Metropolis-within-Gibbs para o
                 modelo de regressão completo de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot20")),
                 tabPanel("γ", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de γ a partir do algoritmo Adaptative Metropolis-within-Gibbs para o 
                 modelo de regressão completo de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotOutput("Plot21"),radioGroupButtons(
                   inputId = "Wid10", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("ξ", h4("Gráficos de linha, densidade e correlação das amostras geradas da distribuição 
                 a posteriori de ξ a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotOutput("Plot21.1"),radioGroupButtons(
                   inputId = "Wid10.1", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("Desvio", h4("Gráficos de linha, densidade e correlação do desvio calculado nas amostras geradas da distribuição 
                 a posteriori a partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotOutput("Plot22"),radioGroupButtons(
                   inputId = "Wid11", label = "", choices = c("Completo","Reduzido"), justified = TRUE)),
                 tabPanel("LP", h4("Gráficos de linha, densidade e correlação do logaritmo da distribuição a posteriori das amostras 
                 geradas partir do algoritmo Adaptative Metropolis-within-Gibbs para os 
                 modelos de regressão de fragilidade compartilhada gama para os dados de pacientes diagnosticados com câncer colorretal no 
                 Hospital de Bellvitge entre 1996 e 1998.",align="center"), plotOutput("Plot23"),radioGroupButtons(
                   inputId = "Wid12", label = "", choices = c("Completo","Reduzido"), justified = TRUE))
          ))),

#############bayesreg2##########################################################

tabItem(tabName = "bayesreg2",
        fluidRow(box(width=12,
                     valueBoxOutput(width = 4, outputId = "box21"),
                     valueBoxOutput(width = 4, outputId = "box19"),
                     valueBoxOutput(width = 4, outputId = "box20")
        )),
        fluidRow(
          tabBox(tabPanel("Completo", h4("Gráfico de dispersão dos resíduos quantílicos do modelo de regressão 
                                         completo de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         Bayesiana, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot43")),
                 tabPanel("Reduzido", h4("Gráfico de dispersão dos resíduos quantílicos do modelo de regressão 
                                         reduzido de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         Bayesiana, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotlyOutput("Plot44"))
          ),
          tabBox(tabPanel("Completo", h4("Worm plot dos resíduos quantílicos do modelo de regressão 
                                         completo de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         Bayesiana, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot45")),
                 tabPanel("Reduzido", h4("Worm plot dos resíduos quantílicos do modelo de regressão 
                                         reduzido de fragilidade compartilhada gama, estimado a partir da abordagem 
                                         Bayesiana, para dados de pacientes diagnosticados com câncer colorretal no 
                                         Hospital de Bellvitge entre 1996 e 1998.",align="center"),plotOutput("Plot46"))
          )))
)))




