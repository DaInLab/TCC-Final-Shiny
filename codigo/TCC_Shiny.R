# ------- Libraries ------- #
library(readr)
library(shiny)
library(shinydashboard)
library(graphics)
library(plotly)
library(tidyverse)
library(ggplot2)

# ------- Libraries ------- #


# ------------------------ Reading CSV file with data ------------------------ #
#pathVar <- "C:\\TCC_FINAL_Shiny\\dados\\pesquisa.csv"
pathVar <- "/Users/jpalbino/Downloads/TCC_FINAL_Shiny/dados/pesquisa.csv"
encondingVar <- "latin1"
pesquisa <- read.csv(pathVar, header=TRUE, stringsAsFactors=FALSE, fileEncoding=encondingVar)
view(pesquisa)

#pathVar2 <- "C:\\TCC_FINAL_Shiny\\dados\\despesas.csv"
pathVar2 <- "/Users/jpalbino/Downloads/TCC_FINAL_Shiny/dados/despesas.csv"
encondingVar2 <- "UTF-8"
despesas <- read.csv(pathVar2, header=TRUE, stringsAsFactors=FALSE, fileEncoding=encondingVar2)
view(despesas)
# ------------------------ Reading CSV file with data ------------------------ #


# ----------------------------- Shiny Implementation ----------------------------- #
ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "TCC II - Análise Estatística Utilizando R e Shiny", 
                                    titleWidth = 500),
                    
                    dashboardSidebar(
                      width = 500,
                      sidebarMenu(
                        menuItem("Parte 1 - Dados gerais dos participantes da pesquisa", tabName = "pt1", icon = icon("chart-bar")),
                        menuItem("Parte 2 - Como a pandemia afetou o estudante", tabName = "pt2", icon = icon("chart-bar")),
                        menuItem("Parte 3 - Como o estudante se sentiu diante da pandemia", tabName = "pt3", icon = icon("chart-bar")),
                        menuItem("Parte 4 - Mudanças na universidade e desempenho escolar do estudante", tabName = "pt4", icon = icon("chart-bar")),
                        menuItem("Parte 5 - Renda X Despesas", tabName = "pt5", icon = icon("chart-bar")),
                        menuItem("Parte 6 - Ansiedade com relação a eventos futuros", tabName = "pt6", icon = icon("chart-bar"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # ------------------------ Part 1 ------------------------ #
                        tabItem("pt1",
                                fluidRow(
                                  box(valueBoxOutput(width = 5.5, outputId = "value_observations")),
                                  box(plotOutput("dadosIdade"), width=7),
                                  box(sliderInput("inputIdade", "Deslize para mudar o número de caixas", 
                                                  2,10,5), width=5)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosGenero"), width=5),
                                  box(plotlyOutput("dadosSitConj"), width=7)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosSitFinan"), width=5.5)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosUniversidade"), width=5.5)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosPrivPubli"), width=5.5)
                                )
                        ),
                        # ------------------------ Part 1 ------------------------ #
                        
                        # ------------------------ Part 2 ------------------------ #
                        tabItem("pt2",
                                fluidRow(
                                  box(plotlyOutput("dadosMoraCom"), width=6),
                                  box(plotlyOutput("dadosGrupoRisco"), width=6),
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosDificuldades"), width=12),
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosAcessoSaude"), width=12),
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosAcessoInternet"), width=12),
                                ),
                        ),
                        # ------------------------ Part 2 ------------------------ #
                        
                        # ------------------------ Part 3 ------------------------ #
                        tabItem("pt3",
                                fluidRow(
                                  box(plotlyOutput("dadosGraduacao"), width=5.5),
                                  box(plotlyOutput("dadosSocializacao"), width=5.5)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosPsicologico"), width=5.5)
                                )
                        ),
                        # ------------------------ Part 3 ------------------------ #
                        
                        # ------------------------ Part 4 ------------------------ #
                        tabItem("pt4",
                                fluidRow(
                                  box(plotlyOutput("dadosAulasVirtuais"), width=6),
                                  box(plotlyOutput("dadosDesempenho"), width=6),
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosAcessoProfessores"), width=5.5)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosLugarEstudo"), width=12),
                                ),
                        ),
                        # ------------------------ Part 4 ------------------------ #
                        
                        # ------------------------ Part 5 ------------------------ #
                        tabItem("pt5",
                                fluidRow(
                                  box(plotlyOutput("dadosAlteracaoRenda"), width=12)
                                ),
                                fluidRow(
                                  box(plotlyOutput("dadosGastos"), width=6),
                                  box(plotlyOutput("dadosDespesas"), width=6)
                                )
                        ),
                        # ------------------------ Part 5 ------------------------ #
                        
                        # ------------------------ Part 6 ------------------------ #
                        tabItem("pt6",
                                fluidRow(
                                  box(plotlyOutput("dadosAnsiedade"), width=5.5),
                                ),
                        )
                        # ------------------------ Part 6 ------------------------ #
                      ),
                    ),
)

server <- function(input, output) {
  
  # ------------------------ Part 1 ------------------------ #
  
  output$value_observations <- renderValueBox({
    valueBox(
      nrow(pesquisa), "Respostas ao questionário", icon = icon("list"),
      color = "purple"
    )
  })
    
  output$dadosIdade <- renderPlot({
    # Using - Hist
    hist(pesquisa$idade, breaks = input$inputIdade, col = 'dark blue', border = 'white', 
         main = 'Idade dos estudantes', xlab="Idade", ylab = "Quantidade")
  })
  
  output$dadosGenero <- renderPlotly({
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
    
    dataGenero <- pesquisa %>%
      group_by(genero) %>%
      summarize(countGenero =  n(),
                percentageGenero = n() / nrow(pesquisa))
    
    pieGenero <- plot_ly(data = dataGenero, labels = ~genero, values = ~percentageGenero,
                         type = 'pie', 
                         textposition = 'inside', 
                         textinfo = 'percent', 
                         insidetextfont = list(color = "#FFFFFF"),
                         hoverinfo = 'text',
                         text = ~paste(genero),
                         sort= FALSE,
                         marker = list(colors=colors, line = list(genero="#FFFFFF", width = 1)),
                         showlegend = FALSE ) %>%
      layout(title= "Gênero" )
  })
  
  output$dadosSitConj <- renderPlotly({
    
    dataSitConj <- pesquisa %>%
      group_by(situacao_conjugal) %>%
      summarize(countSitConj =  n(),
                percentageSitConj = n()/nrow(pesquisa))
    
    pSitConj <- ggplot(data=dataSitConj, aes(x = situacao_conjugal , y = countSitConj,
                                             text = paste("Qtde: ", round(countSitConj), "(",
                                                          round(percentageSitConj * 100), "%)"))) +                                       
      geom_bar(stat="identity")+
      ggtitle("Situação Conjugal")+
      labs(y = "Número de pessoas", x=" ")
    
    ggplotly(pSitConj, tooltip="text")
    
  })
  
  output$dadosSitFinan <- renderPlotly({
    
    colors <- c('rgb(127,255,212)', 'rgb(188,143,143)', 'rgb(95,158,160)')
    
    dataSitFinan <- pesquisa %>%
      group_by(situacao_financeira) %>%
      summarize(countSitFinan =  n(),
                percentageSitFinan = n() / nrow(pesquisa))
    
    pieSitFinan <- plot_ly(data = dataSitFinan, labels = ~situacao_financeira, values = ~percentageSitFinan,
                           type = 'pie', 
                           textposition = 'inside', 
                           textinfo = 'percent', 
                           insidetextfont = list(color = "#FFFFFF"),
                           hoverinfo = 'text',
                           text = ~paste(situacao_financeira),
                           sort= FALSE,
                           marker = list(colors=colors, line = list(situacao_financeira="#FFFFFF", width = 1)),
                           showlegend = FALSE ) %>%
      layout(title= "Situação Financeira" )
  })
  
  output$dadosUniversidade <- renderPlotly({
    
    dataUniversidade <- pesquisa %>%
      group_by(universidade) %>%
      summarize(countUniversidade =  n(),
                percentageUniversidade = n() / nrow(pesquisa))
    
    pUniversidade <- ggplot(data=dataUniversidade, aes(x = universidade , y = countUniversidade, fill = universidade,
                                                       text = paste("Qtde: ", round(countUniversidade), "(",
                                                                    round(percentageUniversidade * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Universidades")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#B22222", "#F4A460", "#BC8F8F", "#A020F0", "#FF00FF", "#FFB6C1", "#DC143C", "#87CEFA"))
    
    ggplotly(pUniversidade, tooltip="text")
    
  })
  
  output$dadosPrivPubli <- renderPlotly({
    
    colorsPrivPubli <- c('rgb(211,94,96)', 'rgb(128,133,133)')
    
    dataPrivPubli <- pesquisa %>%
      group_by(tipo_priv_publi) %>%
      summarize(countPrivPubli =  n(),
                percentagePrivPubli = n() / nrow(pesquisa))
    
    piePrivPubli <- plot_ly(data = dataPrivPubli, labels = ~tipo_priv_publi, values = ~percentagePrivPubli,
                            type = 'pie', 
                            textposition = 'inside', 
                            textinfo = 'percent', 
                            insidetextfont = list(color = "#FFFFFF"),
                            hoverinfo = 'text',
                            text = ~paste(tipo_priv_publi),
                            sort= FALSE,
                            marker = list(colors=colors, line = list(genero="#FFFFFF", width = 1)),
                            showlegend = FALSE ) %>%
      layout(title= "Universidade Privada ou Pública" )
  })
  # ------------------------ Part 1 ------------------------ #
  
  
  # ------------------------ Part 2 ------------------------ #
  output$dadosMoraCom <- renderPlotly({
    
    colorsMoraCom <- c('rgb34,139,34)', 'rgb(139,69,19)')
    
    dataMoraCom <- pesquisa %>%
      group_by(mora_com) %>%
      summarize(countMoraCom =  n(),
                percentageMoraCom = n() / nrow(pesquisa))
    
    pieMoraCom <- plot_ly(data = dataMoraCom, labels = ~mora_com, values = ~percentageMoraCom,
                          type = 'pie', 
                          textposition = 'inside', 
                          textinfo = 'percent', 
                          insidetextfont = list(color = "#FFFFFF"),
                          hoverinfo = 'text',
                          text = ~paste(mora_com),
                          sort= FALSE,
                          marker = list(colors=colorsMoraCom, line = list(mora_com="#FFFFFF", width = 1)),
                          showlegend = FALSE ) %>%
      layout(title= "Estudante mora com" )
  })
  
  output$dadosGrupoRisco <- renderPlotly({
    
    colorsGrupoRisco <- c('rgb(0,0,255)', 'rgb(105,105,105)')
    
    dataGrupoRisco <- pesquisa %>%
      group_by(reside_com_grupo_de_risco) %>%
      summarize(countGrupoRisco =  n(),
                percentageGrupoRisco = n() / nrow(pesquisa))
    
    pieGrupoRisco <- plot_ly(data = dataGrupoRisco, labels = ~reside_com_grupo_de_risco, values = ~percentageGrupoRisco,
                             type = 'pie', 
                             textposition = 'inside', 
                             textinfo = 'percent', 
                             insidetextfont = list(color = "#FFFFFF"),
                             hoverinfo = 'text',
                             text = ~paste(reside_com_grupo_de_risco),
                             sort= FALSE,
                             marker = list(colors=colorsGrupoRisco, line = list(reside_com_grupo_de_risco="#FFFFFF", width = 1)),
                             showlegend = FALSE ) %>%
      layout(title= "Estudante mora com grupo de risco" )
  })
  
  output$dadosDificuldades <- renderPlotly({
    
    colorsDificuldades <- c('rgb(0,128,0)', 'rgb(135,206,235)', 
                            'rgb(138,43,226)','rgb(50,205,50)', 'rgb(218,165,32)' )
    
    dataDificuldades <- pesquisa %>%
      group_by(dificuldades_pandemia) %>%
      summarize(countDificuldades =  n(),
                percentageDificuldades = n() / nrow(pesquisa))
    
    pieDificuldades <- plot_ly(data = dataDificuldades, labels = ~dificuldades_pandemia, values = ~percentageDificuldades,
                               type = 'pie', 
                               textposition = 'inside', 
                               textinfo = 'percent', 
                               insidetextfont = list(color = "#FFFFFF"),
                               hoverinfo = 'text',
                               text = ~paste(dificuldades_pandemia),
                               sort= FALSE,
                               marker = list(colors=colorsDificuldades, line = list(dificuldades_pandemia="#FFFFFF", width = 1)),
                               showlegend = FALSE ) %>%
      layout(title= "Dificuldades dos estudantes" )
  })
  
  output$dadosAcessoSaude <- renderPlotly({
    
    dataAcessoSaude <- pesquisa %>%
      group_by(acesso_saude_pandemia) %>%
      summarize(countsAcessoSaude =  n(),
                percentageAcessoSaude = n() / nrow(pesquisa))
    
    pAcessoSaude <- ggplot(data=dataAcessoSaude, aes(x = acesso_saude_pandemia , y = countsAcessoSaude, fill = acesso_saude_pandemia,
                                                     text = paste("Qtde: ", round(countsAcessoSaude), "(",
                                                                  round(percentageAcessoSaude * 100), "%)"))) +                                               
      geom_bar(stat="identity")+
      ggtitle("Acesso a saúde na pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#00FF00", "#0000FF", "#FF0000", "#A020F0", "#C0C0C0", "#FF00FF", "#00FFFF"))
    
    ggplotly(pAcessoSaude, tooltip="text")
    
  })
  
  output$dadosAcessoInternet <- renderPlotly({
    
    dataAcessoInternet <- pesquisa %>%
      group_by(acesso_internet_pandemia) %>%
      summarize(countsAcessoInternet =  n(),
                percentageAcessoInternet = n() / nrow(pesquisa))
    
    pAcessoInternet <- ggplot(data=dataAcessoInternet, aes(x = acesso_internet_pandemia , y = countsAcessoInternet, fill = acesso_internet_pandemia,
                                                           text = paste("Qtde: ", round(countsAcessoInternet), "(",
                                                                        round(percentageAcessoInternet * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Acesso a internet na pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#A52A2A", "#228B22", "#5F9EA0", "#6495ED", "#FF00FF", "#FFB6C1", "#DC143C"))
    
    ggplotly(pAcessoInternet, tooltip="text")
    
  })
  # ------------------------ Part 2 ------------------------ #
  
  
  # ------------------------ Part 3 ------------------------ #
  output$dadosGraduacao <- renderPlotly({
    
    colorGraduacao <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(95,158,160)', 
                        'rgb(255,127,80)', 'rgb(176,224,230)')
    
    dataGraduacao <- pesquisa %>%
      group_by(estudo_graduacao) %>%
      summarize(countGraduacao =  n(),
                percentageGraduacao = n() / nrow(pesquisa))
    
    pieGraduacao <- plot_ly(data = dataGraduacao, labels = ~estudo_graduacao, values = ~percentageGraduacao,
                            type = 'pie', 
                            textposition = 'inside', 
                            textinfo = 'percent', 
                            insidetextfont = list(color = "#FFFFFF"),
                            hoverinfo = 'text',
                            text = ~paste(estudo_graduacao),
                            sort= FALSE,
                            marker = list(colors=colorGraduacao, line = list(estudo_graduacao="#FFFFFF", width = 1)),
                            showlegend = FALSE ) %>%
      layout(title= "Capacidade de prosseguir com os estudos na pandemia" )
    
  })
  
  output$dadosSocializacao <- renderPlotly({
    
    dataSocializacao <- pesquisa %>%
      group_by(socializacao) %>%
      summarize(countSocializacao =  n(),
                percentageSocializacao = n() / nrow(pesquisa))
    
    pSocializacao <- ggplot(data=dataSocializacao, aes(x = socializacao , y = countSocializacao,
                                                       text = paste("Qtde: ", round(countSocializacao), "(",
                                                                    round(percentageSocializacao * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Capacidade de socialização na pandemia")+
      labs(y = "Número de pessoas", x=" ")
    
    ggplotly(pSocializacao, tooltip="text")
    
  })
  
  output$dadosPsicologico <- renderPlotly({
    
    dataPsicologico <- pesquisa %>%
      group_by(psicologico) %>%
      summarize(countPsicologico =  n(),
                percentagePsicologico = n() / nrow(pesquisa))
    
    pPsicologico <- ggplot(data=dataPsicologico, aes(x = psicologico , y = countPsicologico, fill = psicologico,
                                                     text = paste("Qtde: ", round(countPsicologico), "(",
                                                                  round(percentagePsicologico * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Bem-estar psicológico em geral na pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#B22222", "#F4A460", "#BC8F8F", "#A020F0"))
    
    
    ggplotly(pPsicologico, tooltip="text")
    
  })
  # ------------------------ Part 3 ------------------------ #
  
  
  # ------------------------ Part 4 ------------------------ #
  output$dadosAulasVirtuais <- renderPlotly({
    
    colorsAulasVirtuais <- c('rgb(0,0,255)', 'rgb(128,133,133)')
    
    dataAulasVirtuais <- pesquisa %>%
      group_by(aulas_virtuais_pandemia) %>%
      summarize(countAulasVirtuais =  n(),
                percentageAulasVirtuais = n() / nrow(pesquisa))
    
    pieAulasVirtuais <- plot_ly(data = dataAulasVirtuais, labels = ~aulas_virtuais_pandemia, values = ~percentageAulasVirtuais,
                                type = 'pie', 
                                textposition = 'inside', 
                                textinfo = 'percent', 
                                insidetextfont = list(color = "#FFFFFF"),
                                hoverinfo = 'text',
                                text = ~paste(aulas_virtuais_pandemia),
                                sort= FALSE,
                                marker = list(colors=colorsAulasVirtuais, line = list(aulas_virtuais_pandemia="#FFFFFF", width = 1)),
                                showlegend = FALSE ) %>%
      layout(title= "Aulas Virtuais na Pandemia" )
  })
  
  output$dadosDesempenho <- renderPlotly({
    
    colorsDesempenho <- c('rgb(0,0,255)', 'rgb(255,0,0)', 'rgb(0,128,0)')
    
    dataDesempenho <- pesquisa %>%
      group_by(desempenho) %>%
      summarize(countDesempenho =  n(),
                percentageDesempenho = n() / nrow(pesquisa))
    
    pieAulasVirtuais <- plot_ly(data = dataDesempenho, labels = ~desempenho, values = ~percentageDesempenho,
                                type = 'pie', 
                                textposition = 'inside', 
                                textinfo = 'percent', 
                                insidetextfont = list(color = "#FFFFFF"),
                                hoverinfo = 'text',
                                text = ~paste(desempenho),
                                sort= FALSE,
                                marker = list(colors=colorsDesempenho, line = list(desempenho="#FFFFFF", width = 1)),
                                showlegend = FALSE ) %>%
      layout(title= "Desempenho escolar na pandemia" )
  })
  
  output$dadosAcessoProfessores <- renderPlotly({
    
    data <- pesquisa %>%
      group_by(acesso_professores) %>%
      summarize(countsAcessoProfessores =  n(),
                percentageAcessoProfessores = n() / nrow(pesquisa))
    
    pAcessoProfessores <- ggplot(data=data, aes(x = acesso_professores , y = countsAcessoProfessores, fill = acesso_professores,
                                                text = paste("Qtde: ", round(countsAcessoProfessores), "(",
                                                             round(percentageAcessoProfessores * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Acesso aos professores na pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#D2691E", "#FF69B4", "#9400D3", "#DC143C"))
    
    ggplotly(pAcessoProfessores, tooltip="text")
    
  })
  
  output$dadosLugarEstudo <- renderPlotly({
    
    data <- pesquisa %>%
      group_by(espaco_estudos) %>%
      summarize(countsLugarEstudo =  n(),
                percentageLugarEstudo = n() / nrow(pesquisa))
    
    pLugarEstudo <- ggplot(data=data, aes(x = espaco_estudos , y = countsLugarEstudo, fill = espaco_estudos,
                                          text = paste("Qtde: ", round(countsLugarEstudo), "(",
                                                       round(percentageLugarEstudo * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("Espaço físico para acompanhar as aulas")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#008B8B", "#00FA9A", "#556B2F", "#DC143C", "#FF00FF"))
    
    ggplotly(pLugarEstudo, tooltip="text")
    
  })
  # ------------------------ Part 4 ------------------------ #
  
  
  # ------------------------ Part 5 ------------------------ #
  output$dadosAlteracaoRenda <- renderPlotly({
    
    data <- pesquisa %>%
      group_by(alteracao_renda_pandemia) %>%
      summarize(countsAlteracaoRenda =  n(),
                percentageAlteracaoRenda = n() / nrow(pesquisa))
    
    pAlteracaoRenda <- ggplot(data=data, aes(x = alteracao_renda_pandemia , y = countsAlteracaoRenda, fill = alteracao_renda_pandemia,
                                             text = paste("Qtde: ", round(countsAlteracaoRenda), "(",
                                                          round(percentageAlteracaoRenda * 100), "%)"))) +
      geom_bar(stat="identity")+
      ggtitle("A renda na pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#FF4500", "#40E0D0", "#00FF7F"))
    
    ggplotly(pAlteracaoRenda, tooltip="text")
    
  })
  
  output$dadosGastos <- renderPlotly({
    
    colors <- c('rgb(106,90,205)', 'rgb(0,0,139)','rgb(0,191,255)')
    
    dataGastos <- pesquisa %>%
      group_by(gastos_na_pandemia) %>%
      summarize(countGastos =  n(),
                percentageGastos = n() / nrow(pesquisa))
    
    pieGastos <- plot_ly(data = dataGastos, labels = ~gastos_na_pandemia, values = ~percentageGastos,
                         type = 'pie', 
                         textposition = 'inside', 
                         textinfo = 'percent', 
                         insidetextfont = list(color = "#FFFFFF"),
                         hoverinfo = 'text',
                         text = ~paste(gastos_na_pandemia),
                         sort= FALSE,
                         marker = list(colors=colors, line = list(genero="#FFFFFF", width = 1)),
                         showlegend = FALSE ) %>%
      layout(title= "Gastos na pandemia" )
    
  })
  
  output$dadosDespesas <- renderPlotly({
    
    colors <- c('rgb(128,0,128)', 'rgb(255,20,147)','rgb(135,206,235)', 'rgb(60,179,113)', 'rgb(47,79,79)')
    
    dataDespesas <- despesas %>%
      group_by(despesas_que_cresceram_pandemia) %>%
      summarize(countDespesas =  n(),
                percentageDespesas = n() / nrow(despesas))
    
    pieDespesas <- plot_ly(data = dataDespesas, labels = ~despesas_que_cresceram_pandemia, values = ~percentageDespesas,
                           type = 'pie', 
                           textposition = 'inside', 
                           textinfo = 'percent', 
                           insidetextfont = list(color = "#FFFFFF"),
                           hoverinfo = 'text',
                           text = ~paste(despesas_que_cresceram_pandemia),
                           sort= FALSE,
                           marker = list(colors=colors, line = list(despesas_que_cresceram_pandemia="#FFFFFF", width = 1)),
                           showlegend = FALSE ) %>%
      layout(title= "Despesas que cresceram na pandemia" )
    
  })
  # ------------------------ Part 5 ------------------------ #
  
  
  # ------------------------ Part 6 ------------------------ #
  output$dadosAnsiedade <- renderPlotly({
    
    dataAnsiedade <- pesquisa %>%
      group_by(ansiedade_planejamento_pessoal) %>%
      summarize(countsAnsiedade =  n(),
                percentageAnsiedade = n() / nrow(pesquisa))
    
    pAnsiedade <- ggplot(data=dataAnsiedade, aes(x = ansiedade_planejamento_pessoal , y = countsAnsiedade, fill = ansiedade_planejamento_pessoal,
                                                 text = paste("", round(percentageAnsiedade * 100), "%"))) +
      geom_bar(stat="identity")+
      ggtitle("Ansiedade durante a pandemia")+
      labs(y = "Número de pessoas", x=" ")+
      scale_fill_manual(name = "", values=c("#DC143C", "#FFD700", "#FFA07A", "#BDB76B", "#FF00FF", "#FFB6C1", "#DC143C"))
    
    ggplotly(pAnsiedade, tooltip="text")
    
  })
  # ------------------------ Part 6 ------------------------ #
}

shinyApp(ui = ui, server = server)
# ----------------------------- Shiny Implementation ----------------------------- #