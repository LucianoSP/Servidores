library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)


shinyUI(fluidPage(

  # Application title
  
  shinyUI(navbarPage(
    title = 'Servidores Federais',
    tabPanel('Sumario',
        wellPanel( 
         fluidRow(
               column(4,
                      sliderInput ("min_servidores", label = "Número mínimo de Servidores", 1, 2000, 1, step = 10)
               ),
               
               column(4,
                      sliderInput ("min_remuneracao", label = "Remuneração Mínima:", 1, 50000, 1, step = 1000)
               ),
               
               column(4,
                      br(),
                      div(style="color:darkblue", h4(textOutput("text1")), h4(textOutput("text2")))
               ))
             ),
         
             
          fluidRow(
            column(6,
              plotOutput('plot1')
            ),
            
            column(6,
              plotOutput("plot7")
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
                         plotOutput('plot_comp1'),
                         plotOutput('plot_comp2'),
                         plotOutput('plot_comp3'),
                         plotOutput('plot_comp4'),
                         plotOutput('plot_comp5')
                         
                         
                )
                
            )
          
        
    ),
    
    tabPanel('Servidores',
             
             textOutput("texto"),
             hr(),
             DT::dataTableOutput("tab_resumo"),
             DT::dataTableOutput("datatab_servidor")
             
             
             
             
    ),
    
    tabPanel('Futuro'       )
    
  ))
))
