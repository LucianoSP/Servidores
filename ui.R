library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(ggthemes)


# Application title

shinyUI(navbarPage(
  title = 'Servidores Federais',
  tabPanel('Sumario',
           wellPanel( 
             fluidRow(
               column(4,
                       sliderInput ("min_servidores", label = "Número mínimo de Servidores", 0, 2000, 0, step = 10)
                      # numericInput("min_servidores", label = "Número mínimo de Servidores", value=0, min=0, max=2000, step=10)
                      
               ),
               
               column(4,
                       sliderInput ("min_remuneracao", label = "Remuneração Média Mínima:", 0, 50000, 0, step = 1000)
                      # numericInput("min_remuneracao", label = "Remuneração Média Mínima:", value=0, min=0, max=50000, step= 1000)
               ),
               
               column(4,
                      div(h4(textOutput("text1")), h4(textOutput("text2"))),
                      actionButton("action1", "Atualizar Gráficos")
               ))
           ),
           
           
           fluidRow(
             column(6,
                    plotOutput('plot7')
             ),
             
             column(6,
                    plotOutput("plot1")
             )
           ),
           
           fluidRow(
             column(12,
                    plotOutput('plot2')
                    
             )
           )
           
  ),
  
  tabPanel('Carreira',
           
           
           tabsetPanel(type = "tabs", 
                       tabPanel("Todas",
                                
                                DT::dataTableOutput("carreiras")
                                
                       ), 
                       
                       tabPanel("Comparar",  
                                uiOutput("carr2"),
                                DT::dataTableOutput("carreiras3"),
                                hr(),
                                plotOutput('plot_comp1'),br(),
                                plotOutput('plot_comp2'),br(),
                                plotOutput('plot_comp3'),br(),
                                plotOutput('plot_comp4'),br(),
                                plotOutput('plot_comp5')
                                
                                
                       )
                       
           )
           
           
  ),
  
  tabPanel('Servidores',
           
           textOutput("texto"),
           hr(),
           DT::dataTableOutput("tab_resumo"),
           hr(),
           DT::dataTableOutput("datatab_servidor")
           
           
           
           
  ),
  
  tabPanel('Órgãos',
     tabsetPanel(type = "tabs",  
           tabPanel("Comparar",
              uiOutput("orgaos1"),
              hr(),
              plotOutput('plot_org1'),br()
            ),    
           tabPanel("Estatísticas",
                    
                    hr(),
                    plotOutput(''),br()
           )    
          
        )
     )
))

