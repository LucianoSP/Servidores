library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(ggthemes)


shinyServer( function(input, output, session) {
  
  
  # servidores = read.table("servidores2.csv", header = TRUE, sep = ";")
  servidores <- readRDS("servidores2.rds")
  servidores = servidores %>% filter(SITUACAO_VINCULO != "EXERC DESCENT CARREI", SITUACAO_VINCULO != "REQUISITADO")
  servidores$NOME = as.character(servidores$NOME)
  carreiras_populosas = servidores %>% select(DESCRICAO_CARGO) %>% group_by(DESCRICAO_CARGO) %>% summarise(n=n()) %>% 
    arrange(desc(n)) %>% slice(1:20) %>% select(DESCRICAO_CARGO) %>% as.matrix() %>% c()
  carreiras_bempagas = servidores %>% group_by(DESCRICAO_CARGO) %>% summarise(n=mean(REMUNERACAO_BRUTA)) %>% 
    arrange(desc(n)) %>% slice(1:20) %>% select(DESCRICAO_CARGO) %>% as.matrix() %>% c()
  
  ####################################SUMARIO###########################################    
  carreiras_grandes = reactive({servidores %>% group_by(DESCRICAO_CARGO) %>% summarise(N=n()) %>% 
      filter(N>input$min_servidores) %>% select(DESCRICAO_CARGO) %>% as.matrix() %>% c()})
  carreiras_bem_remun = reactive({servidores %>% group_by(DESCRICAO_CARGO) %>% summarise(N=mean(REMUNERACAO_BRUTA)) %>% 
      filter(N>input$min_remuneracao) %>% as.matrix() %>% c()})
  
  # Histograma
  output$plot1 <- renderPlot({
    input$action1
    isolate(servidores %>% filter(DESCRICAO_CARGO %in% carreiras_bem_remun(), DESCRICAO_CARGO %in% carreiras_grandes()) %>% filter(!is.na(UF_EXERCICIO)) %>% 
      ggplot(aes(REMUNERACAO_BRUTA)) + 
      geom_histogram(colour = "darkblue", fill = "skyblue") + 
      geom_vline(aes(xintercept=mean(REMUNERACAO_BRUTA, na.rm=T)), color="red", linetype="dashed", size=0.8) +
      geom_vline(aes(xintercept=median(REMUNERACAO_BRUTA, na.rm=T)), color="black", linetype="dashed", size=0.8) +
      labs(x="Remuneração", y="", title="Histograma") +
      #     scale_y_continuous(breaks=seq(0,60000,5000)) + 
      scale_x_continuous(breaks=seq(0,50000,5000)) + 
      theme(plot.title = element_text(size=15, face="bold", vjust=2)) 
      
      )
    
    
  })
  
  # Boxplot
  output$plot2 <- renderPlot({
    input$action1
    isolate(servidores %>% filter(DESCRICAO_CARGO %in% carreiras_bem_remun(), DESCRICAO_CARGO %in% carreiras_grandes()) %>%  
      ggplot(aes(UF_EXERCICIO, REMUNERACAO_BRUTA)) + 
      geom_boxplot(colour = "black", fill = "#FF9999") + 
      labs(x="Estado", y="", title="Boxplot") +
      scale_y_continuous(breaks=seq(0,60000,3000)) + 
      theme(plot.title = element_text(size=15, face="bold", vjust=2)) +
      theme_stata() + scale_colour_stata() 
      )
  })
  
  # Top carreiras mais populosas
  output$plot7 <- renderPlot({
    input$action1
    isolate(servidores %>% filter(DESCRICAO_CARGO %in% carreiras_bem_remun(), DESCRICAO_CARGO %in% carreiras_grandes(), !is.na(DESCRICAO_CARGO)) %>% 
      group_by(DESCRICAO_CARGO) %>% 
      summarise(N_Servidores = n(), Salario_Medio = mean(REMUNERACAO_BRUTA)) %>% 
      arrange(desc(N_Servidores)) %>% slice(1:15) %>%
      ggplot(aes(x=DESCRICAO_CARGO, y=Salario_Medio)) +
      geom_bar(stat = "identity", colour="white", fill="red")  +
      labs(x="", y="Salario Medio", title="As 15 Carreiras mais populosas") +
      theme(plot.title = element_text(size=15, face="bold", vjust=2)) +
        
      coord_flip()
    )
  })
  
  # numero de carreiras
  output$text1 <- renderText ({
    t=servidores %>% filter(DESCRICAO_CARGO %in% carreiras_bem_remun(), DESCRICAO_CARGO %in% carreiras_grandes()) %>% 
      group_by(DESCRICAO_CARGO) %>% summarise(N_Servidores = n()) %>% summarise(n()) %>% as.character()
    paste("   Número de Carreiras: ", t)
  })
  
  # numero de servidores
  output$text2 <- renderText ({
    t=servidores %>% filter(DESCRICAO_CARGO %in% carreiras_bem_remun(), DESCRICAO_CARGO %in% carreiras_grandes()) %>% 
      group_by(DESCRICAO_CARGO) %>% summarise(N_Servidores = n()) %>% summarise(sum(N_Servidores)) %>% as.character()
    paste("   Número de Servidores: ", t)
    
  })
  
  #####################################CARREIRAS##########################################  
  
  REMUN_TOTAL = sum(servidores$REMUNERACAO_BRUTA)
  CARGOS = enc2utf8(servidores %>% select(DESCRICAO_CARGO) %>% distinct() %>% arrange(DESCRICAO_CARGO) %>% as.matrix() %>% c())
  
  # Sidepanel
  output$carr1 <- renderUI({
    selectizeInput("carr1", "Carreira Específica:", CARGOS, selected = NULL, multiple = TRUE,
                   options = NULL)
  })
  
  output$carr2 <- renderUI({
    selectizeInput("carr2", "Comparar Carreiras:", CARGOS, selected = NULL, multiple = TRUE,
                   options = list(placeholder = 'select a state name'))
  })
  
  # TAB Todas
  output$carreiras <- DT::renderDataTable({
    datatable(servidores %>% group_by(DESCRICAO_CARGO) %>% summarise(Salario_Medio = round(mean(REMUNERACAO_BRUTA, rm.na=TRUE), 2), N_Servidores = n(), Gasto_Total=sum(REMUNERACAO_BRUTA), Percentual = round((Gasto_Total*100/REMUN_TOTAL),2)) %>% 
                arrange(desc(Salario_Medio)), filter = "top", rownames = FALSE) 
  })
  
  
  
  # TAB Comparar
  
  output$carreiras3 <- DT::renderDataTable({
    if(is.null(input$carr2))
      return()
    SALARIO_CAR = servidores %>% filter(DESCRICAO_CARGO==input$carr2) %>% group_by(DESCRICAO_CARGO) %>%
      summarise(Numero_Servidores = n()*length(input$carr2), Salario_Medio = round(mean(REMUNERACAO_BRUTA), 2), Salario_Maximo = max(REMUNERACAO_BRUTA), Salario_Minimo = min(REMUNERACAO_BRUTA))
    datatable(SALARIO_CAR)
  })
  
  output$plot_comp1 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% 
      ggplot(aes(REMUNERACAO_BRUTA, fill = DESCRICAO_CARGO)) + 
      geom_histogram(alpha=.8, position="dodge", colour = "black") + 
      labs( x="", y="", title="Distribuição") +
      theme_economist() + scale_colour_economist() +
      theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank())
  })
  
  output$plot_comp2 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% 
      ggplot(aes(REMUNERACAO_BRUTA, fill = DESCRICAO_CARGO)) + 
      geom_density(alpha=0.2) + 
      labs(x="", y="", title="Distribuição") +
      theme_economist() + scale_colour_economist() +
      theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank())
  })
  
  output$plot_comp3 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% group_by(DESCRICAO_CARGO, UF_EXERCICIO) %>% summarise(TOTAL=n()) %>%
      ggplot(aes(UF_EXERCICIO, TOTAL, fill = DESCRICAO_CARGO)) + 
      geom_bar(stat = "identity", alpha=.8, position="dodge") +
      labs(x="", y="", title="Servidores por Estado") +
      theme_economist() + scale_colour_economist() +
      theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank())
  })
  
  output$plot_comp4 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% group_by(UF_EXERCICIO) %>%
      ggplot(aes(UF_EXERCICIO, REMUNERACAO_BRUTA, fill = DESCRICAO_CARGO)) + 
      geom_boxplot(colour = "darkblue") + 
      labs(x="", y="Quantidade de Servidores", title="Boxplot") +
      theme_economist() + scale_colour_economist() +
      theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank())
  })
  
  output$plot_comp5 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% group_by(DESCRICAO_CARGO, ORG_EXERCICIO) %>% summarise(N=n()) %>% arrange(desc(N)) %>% top_n(10) %>%
      ggplot(aes(ORG_EXERCICIO, N, fill = DESCRICAO_CARGO)) + 
      geom_bar(colour = "darkblue", stat = "identity") + 
      labs(y="Quantidade de Servidores", title="Por Órgão") +
      theme_economist() + scale_colour_economist() +
      theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank()) +
      coord_flip()
  })
  
  ####################################Servidores########################################### 
  
  name = reactive({input$datatab_servidor_rows_current})
  nome = reactive({
    if(length(name()) == 1) servidores %>% slice(as.integer(name())) %>% select(NOME) %>% as.matrix() %>% c() else 1
  })
  
  output$texto = renderText({paste("Nome: ", nome())})
  
  output$tab_resumo <- DT::renderDataTable({
    serv_remuneracao = servidores %>% filter(NOME %in% nome()) %>% select(REMUNERACAO_BRUTA)
    serv_cargo = servidores %>% filter(NOME %in% nome()) %>% select(DESCRICAO_CARGO)
    remun_cargo = servidores %>% filter(DESCRICAO_CARGO == serv_cargo[1,]) %>% summarise(Num_Serv = n(), Media = round(mean(REMUNERACAO_BRUTA),2), Min = min(REMUNERACAO_BRUTA), Max = max(REMUNERACAO_BRUTA))
    num_serv_carg = servidores %>% filter(DESCRICAO_CARGO == serv_cargo[1,]) %>% summarise(N_Servidores_carreira = n())
    num_serv_acima = servidores %>% filter(DESCRICAO_CARGO == serv_cargo[1,] & REMUNERACAO_BRUTA > serv_remuneracao[1,]) %>% summarise(N_Servidores_Acima = n())
    num_serv_abaixo = servidores %>% filter(DESCRICAO_CARGO == serv_cargo[1,] & REMUNERACAO_BRUTA < serv_remuneracao[1,]) %>% summarise(N_Servidores_Abaixo = n())
    resumo_geral = cbind(num_serv_carg, num_serv_acima, num_serv_abaixo, remun_cargo)
    datatable(resumo_geral, options = list(dom = 't')) 
  })
  
  
  output$datatab_servidor <- DT::renderDataTable({
    a = servidores %>% select(NOME, DESCRICAO_CARGO, FUNCAO, SITUACAO_VINCULO, ORG_EXERCICIO, UF_EXERCICIO, REMUNERACAO_BRUTA, VERBAS, JETONS) 
    action = dataTableAjax(session, a)
    datatable(a, filter = "top", rownames = FALSE, options = list(ajax = list(url = action), autoWidth = TRUE 
                                                # columnDefs = list(list(width = '300px', targets = c(2,3,4,5)))
                                                ))
  })
  
  
  
  #############################################################################   
  ORGAOS = enc2utf8(servidores %>% select(ORG_EXERCICIO) %>% distinct() %>% arrange(ORG_EXERCICIO) %>% as.matrix() %>% c())
  
  output$orgaos1 <- renderUI({
    selectizeInput("orgaos1", "Órgãos:", ORGAOS, selected = NULL, multiple = TRUE,
                   options = NULL)
  })
 
  output$plot_org1 <- renderPlot({
    if(is.null(input$orgaos1))
      return()
    servidores %>% filter(ORG_EXERCICIO %in% input$orgaos1) %>% group_by(ORG_EXERCICIO, DESCRICAO_CARGO) %>% summarise(N_Servidores=n()) %>% top_n(5) %>%
      ggplot(aes(DESCRICAO_CARGO, N_Servidores)) + 
      facet_grid(ORG_EXERCICIO ~.) +
      geom_bar(stat = "identity", position = "dodge", fill = "darkblue") + 
      labs(y="", x="", title="Número de Servidores por Órgão") +
      theme(axis.title.x = element_text(face="bold", colour="#990000", size=12),
            axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) + 
      coord_flip()
#       theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank()) +
#       coord_flip()
  })
  
  
   
#   output$plot_org1 <- renderPlot({
#     if(is.null(input$orgaos1))
#       return()
#     servidores %>% filter(ORG_EXERCICIO %in% input$orgaos1) %>% group_by(ORG_EXERCICIO) %>% summarise(N_Servidores=n(), Custo=sum(REMUNERACAO_BRUTA)) %>% arrange(desc(N_Servidores)) %>%
#       ggplot(aes(ORG_EXERCICIO, N_Servidores, fill = Custo)) + 
#       geom_bar(stat = "identity", position = "dodge") + 
#       labs(y="", x="", title="Número de Servidores por Órgão") +
#       theme(plot.title = element_text(size=15, face="bold", vjust=2), legend.title=element_blank()) +
#       coord_flip()
#   })
#   
  
  #   servidores %>% group_by(ORG_EXERCICIO) %>% summarise(SM = mean(REMUNERACAO_BRUTA)) %>% arrange (desc(SM)) %>% top_n(20)
  #   servidores %>% filter(DESCRICAO_CARGO %in% input$carr2) %>% group_by(ORG_EXERCICIO) %>% summarise(N=n()) %>% arrange(desc(N))
  #   
  
})