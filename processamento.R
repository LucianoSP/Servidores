
remun = read.table("20150331_Remuneracao.csv", sep = "\t", header = TRUE)
cad = read.table("20150331_Cadastro.csv", sep = "\t", header = TRUE, fill = TRUE)
cad = cad %>% select(Id_SERVIDOR_PORTAL, NOME, DESCRICAO_CARGO, FUNCAO, ORG_LOTACAO, ORGSUP_LOTACAO, ORG_EXERCICIO, ORGSUP_EXERCICIO, SITUACAO_VINCULO, REGIME_JURIDICO, UF_EXERCICIO)
remun = remun %>% select(ID_SERVIDOR_PORTAL, REMUNERAÇÃO.BÁSICA.BRUTA..R.., REMUNERAÇÃO.APÓS.DEDUÇÕES.OBRIGATÓRIAS..R.., TOTAL.DE.VERBAS.INDENIZATÓRIAS..R....., TOTAL.DE.HONORÁRIOS..JETONS. )
names(remun) = c("ID", "REMUNERACAO_BRUTA", "REMUNERACAO_LIQUIDA", "VERBAS", "JETONS")
cad = cad %>% rename(ID = Id_SERVIDOR_PORTAL)
cad$ID = as.integer(as.character(cad$ID))

desc_cargo= servidores %>% select(ID, DESCRICAO_CARGO)
funcao = servidores %>% select(ID, FUNCAO)
funcao = funcao %>% distinct()
desc_cargo = desc_cargo %>% distinct()
desc_cargo = desc_cargo %>% filter(DESCRICAO_CARGO != "")
funcao = funcao %>% filter(FUNCAO != "")

servidores = servidores %>% select(-DESCRICAO_CARGO, -FUNCAO)

servidores = servidores %>% distinct()
servidores = left_join(servidores, cargos, by="ID")
