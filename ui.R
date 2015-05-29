library(ggvis)
library(shiny)

shinyUI(fluidPage(

  # Application title
  
  shinyUI(navbarPage(
    title = 'Servidores Federais',
    tabPanel('Sumario',
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
               )
             ),
          hr(),   
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
        sidebarLayout(
           sidebarPanel(
             uiOutput("carr2")
             
            ),
             
           mainPanel(
             tabsetPanel(type = "tabs", 
                tabPanel("Todas",
                      
                      DT::dataTableOutput("carreiras")   
                ), 
                
                tabPanel("Comparar",  
                         DT::dataTableOutput("carreiras3"),
                         plotOutput('plot_comp1'),
                         plotOutput('plot_comp2'),
                         plotOutput('plot_comp3'),
                         plotOutput('plot_comp4')
                         
                         
                )
                
            )
          )
        )
    ),
    
    tabPanel('Servidores',
             textInput("servidor_input", label ="Servidor a Pesquisar", value = ""),
             DT::dataTableOutput("tab_resumo"),
             DT::dataTableOutput("datatab_servidor")
             
             
    ),
    
    tabPanel('Futuro'       )
    
  ))
))
