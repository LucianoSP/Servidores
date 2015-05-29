library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)


shinyServer( function(input, output, session) {
  
  
  # servidores = read.table("servidores2.csv", header = TRUE, sep = ";")
  servidores <- readRDS("servidores.rds")
  servidores$NOME = as.character(servidores$NOME)
  carreiras_populosas = servidores %>% select(CARGO) %>% group_by(CARGO) %>% summarise(n=n()) %>% 
                arrange(desc(n)) %>% slice(1:20) %>% select(CARGO) %>% as.matrix() %>% c()
  carreiras_bempagas = servidores %>% group_by(CARGO) %>% summarise(n=mean(REMUNERACAO)) %>% 
    arrange(desc(n)) %>% slice(1:20) %>% select(CARGO) %>% as.matrix() %>% c()
  
  ####################################SUMARIO###########################################    
  carreiras_grandes = reactive({servidores %>% group_by(CARGO) %>% summarise(N=n()) %>% 
      filter(N>input$min_servidores) %>% select(CARGO) %>% as.matrix() %>% c()})
  carreiras_bem_remun = reactive({servidores %>% group_by(CARGO) %>% summarise(N=mean(REMUNERACAO)) %>% 
      filter(N>input$min_remuneracao) %>% as.matrix() %>% c()})
  
  # Histograma
  output$plot1 <- renderPlot({
    servidores %>% filter(CARGO %in% carreiras_bem_remun(), CARGO %in% carreiras_grandes()) %>% filter(!is.na(UF)) %>% 
    ggplot(aes(REMUNERACAO)) + 
      geom_histogram(colour = "darkblue", fill = "skyblue") + 
      geom_vline(aes(xintercept=mean(REMUNERACAO, na.rm=T)), color="red", linetype="dashed", size=0.8) +
      geom_vline(aes(xintercept=median(REMUNERACAO, na.rm=T)), color="black", linetype="dashed", size=0.8) +
      labs(x="Remuneração", y="", title="Histograma") +
      scale_y_continuous(breaks=seq(0,60000,5000)) + scale_x_continuous(breaks=seq(0,50000,5000))
    
    
  })
  
  # Boxplot
  output$plot2 <- renderPlot({
    servidores %>% filter(CARGO %in% carreiras_bem_remun(), CARGO %in% carreiras_grandes()) %>%  
      ggplot(aes(UF, REMUNERACAO)) + 
      geom_boxplot(colour = "black", fill = "#FF9999") + 
      labs(x="Estado", y="", title="Boxplot") +
      scale_y_continuous(breaks=seq(0,60000,3000))
  })
  
  # Top carreiras mais populosas
  output$plot7 <- renderPlot({
    servidores %>% filter(CARGO %in% carreiras_bem_remun(), CARGO %in% carreiras_grandes(), !is.na(CARGO)) %>% 
      group_by(CARGO) %>% 
      summarise(N_Servidores = n(), Salario_Medio = mean(REMUNERACAO), Salario_Max = max(REMUNERACAO), Salario_Min = min(REMUNERACAO)) %>% 
      arrange(desc(N_Servidores)) %>% slice(1:15) %>%
      ggplot(aes(x=reorder(CARGO, Salario_Medio), y=Salario_Medio)) + 
      geom_bar(stat="identity", fill = "darkgreen") + 
      labs(x="", y="Salario Medio", title="As 15 Carreiras mais populosas") +
      coord_flip()
   
  })
  
  # numero de carreiras
  output$text1 <- renderText ({
    t=servidores %>% filter(CARGO %in% carreiras_bem_remun(), CARGO %in% carreiras_grandes()) %>% 
      group_by(CARGO) %>% summarise(N_Servidores = n()) %>% summarise(n()) %>% as.character()
    paste("Número de Carreiras:", t)
  })
  
  # numero de servidores
  output$text2 <- renderText ({
    t=servidores %>% filter(CARGO %in% carreiras_bem_remun(), CARGO %in% carreiras_grandes()) %>% 
      group_by(CARGO) %>% summarise(N_Servidores = n()) %>% summarise(sum(N_Servidores)) %>% as.character()
    paste("Número de Servidores:", t)
    
  })
  
  #####################################CARREIRAS##########################################  
  
  REMUN_TOTAL = sum(servidores$REMUNERACAO)
  CARGOS = servidores %>% select(CARGO) %>% distinct() %>% arrange(desc(CARGO)) %>% as.matrix() %>% c()
  
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
    datatable(servidores %>% group_by(CARGO) %>% summarise(Salario_Medio = round(mean(REMUNERACAO, rm.na=TRUE), 2), N_Servidores = n(), Gasto_Total=sum(REMUNERACAO), Percentual = round((Gasto_Total*100/REMUN_TOTAL),2)) %>% 
      arrange(desc(Salario_Medio))) 
  })
  
  # TAB Específica
  
  
  
  # TAB Comparar
  
  output$carreiras3 <- DT::renderDataTable({
    if(is.null(input$carr2))
      return()
    SALARIO_CAR = servidores %>% filter(CARGO==input$carr2) %>% group_by(CARGO) %>%
                  summarise(Numero_Servidores = n(), Salario_Medio = round(mean(REMUNERACAO), 2), Salario_Maximo = max(REMUNERACAO), Salario_Minimo = min(REMUNERACAO))
    datatable(SALARIO_CAR)
  })
  
  output$plot_comp1 <- renderPlot({
    servidores %>% filter(CARGO %in% input$carr2) %>% 
      ggplot(aes(REMUNERACAO, fill = CARGO)) + 
      geom_histogram(alpha=.8, position="dodge") + 
      labs(x="Remuneração", y="", title="Distribuição") 
  })
  
  output$plot_comp2 <- renderPlot({
    servidores %>% filter(CARGO %in% input$carr2) %>% 
      ggplot(aes(REMUNERACAO, fill = CARGO)) + 
      geom_density(alpha=0.2) + 
      labs(x="Remuneração", y="", title="Distribuição") 
  })
  
  output$plot_comp3 <- renderPlot({
    servidores %>% filter(CARGO %in% input$carr2) %>% group_by(CARGO, UF) %>% summarise(TOTAL=n()) %>%
      ggplot(aes(UF, TOTAL, fill = CARGO)) + 
      geom_bar(stat = "identity", alpha=.8, position="dodge") +
      labs(x="Remuneração", y="", title="Servidores por Estado") 
  })
  
  output$plot_comp4 <- renderPlot({
    if(is.null(input$carr2))
      return()
    servidores %>% filter(CARGO %in% input$carr2) %>% group_by(UF) %>%
              ggplot(aes(UF, REMUNERACAO, fill = CARGO)) + 
              geom_boxplot(colour = "darkblue") + 
              labs(x="Estado", y="Quantidade de Servidores", title="Boxplot")
  })
  
  ####################################Servidores########################################### 
  
  name = reactive({input$servidor_input})
  
  output$tab_resumo <- DT::renderDataTable({
    if(is.null(input$servidor_input))
      return()
    serv = servidores %>% filter(NOME == name()) 
    serv_remuneracao = serv %>% select(REMUNERACAO)
    serv_cargo = serv %>% select(CARGO)
    remun_cargo = servidores %>% filter(CARGO == serv_cargo[1,]) %>% summarise(Num_Serv = n(), Media = round(mean(REMUNERACAO),2), Min = min(REMUNERACAO), Max = max(REMUNERACAO))
    #rank = servidores %>% filter(CARGO == serv_cargo[1,]) %>% mutate(Rank = rank(REMUNERACAO,ties.method = "average")) %>% arrange(desc(Rank))
    num_serv_carg = servidores %>% filter(CARGO == serv_cargo[1,]) %>% summarise(N_Servidores_carreira = n())
    num_serv_acima = servidores %>% filter(CARGO == serv_cargo[1,] & REMUNERACAO > serv_remuneracao[1,]) %>% summarise(N_Servidores_Acima = n())
    num_serv_abaixo = servidores %>% filter(CARGO == serv_cargo[1,] & REMUNERACAO < serv_remuneracao[1,]) %>% summarise(N_Servidores_Abaixo = n())
    resumo_geral = cbind(num_serv_carg, num_serv_acima, num_serv_abaixo, remun_cargo)
    datatable(resumo_geral)
  })
  
  
  output$datatab_servidor <- DT::renderDataTable({
    a = servidores %>% select(-ID)
    action = dataTableAjax(session, a)
    datatable(a, server = TRUE, filter = "top", options = list(ajax = list(url = action)))
  })
  #############################################################################   
})