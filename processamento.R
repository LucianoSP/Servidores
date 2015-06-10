library(data.table)
library(dplyr)


remun = read.table("20150331_Remuneracao.csv", sep = "\t", header = TRUE, fill = TRUE,  quote = "", dec = ",")
cad = read.table("20150331_Cadastro.csv", sep = "\t", header = TRUE, fill = TRUE,  quote = "")
cad = cad %>% select(Id_SERVIDOR_PORTAL, NOME, DESCRICAO_CARGO, FUNCAO, ORG_LOTACAO, ORGSUP_LOTACAO, ORG_EXERCICIO, ORGSUP_EXERCICIO, SITUACAO_VINCULO, REGIME_JURIDICO, UF_EXERCICIO)
remun = remun %>% select(ID_SERVIDOR_PORTAL, REMUNERAÇÃO.BÁSICA.BRUTA..R.., REMUNERAÇÃO.APÓS.DEDUÇÕES.OBRIGATÓRIAS..R.., TOTAL.DE.VERBAS.INDENIZATÓRIAS..R....., TOTAL.DE.HONORÁRIOS..JETONS. )
names(remun) = c("ID", "REMUNERACAO_BRUTA", "REMUNERACAO_LIQUIDA", "VERBAS", "JETONS")
cad = cad %>% rename(ID = Id_SERVIDOR_PORTAL)
cad$ID = as.integer(as.character(cad$ID))

desc_cargo= cad %>% select(ID, DESCRICAO_CARGO)
funcao = cad %>% select(ID, FUNCAO)
funcao = funcao %>% distinct() %>% filter(FUNCAO != "")
desc_cargo = desc_cargo %>% distinct() %>% filter(DESCRICAO_CARGO != "")


servidores = cad %>% select(-DESCRICAO_CARGO, -FUNCAO)
servidores = servidores %>% distinct()
servidores = left_join(servidores, desc_cargo, by="ID")
servidores = left_join(servidores, funcao, by="ID")
servidores = left_join(remun, servidores, by="ID")
servidores$REMUNERACAO_BRUTA = as.numeric(servidores$REMUNERACAO_BRUTA)
servidores$REMUNERACAO_LIQUIDA = as.numeric(servidores$REMUNERACAO_LIQUIDA)
servidores$VERBAS = as.numeric(servidores$VERBAS)
servidores$JETONS = as.numeric(servidores$JETONS)



# saveRDS(servidores, file = "servidores.rds")
