priOptParameters <- function (data.painel, SKUGroup) {
  
  ############ Identificação da família no painel
  col_p_medio<-which(colnames(data.painel)==paste('p_medio_G',SKUGroup,sep=''))
  col_vda_qtd<-which(colnames(data.painel)==paste('VDA_QTD_G',SKUGroup,sep=''))
  col_vda_brut <- which(colnames(data.painel) == paste('VDA_BRT_G', SKUGroup, sep=''))
  col_vda <- which(colnames(data.painel) == paste('VDA_G', SKUGroup, sep=''))
  col_cmv <- which(colnames(data.painel) == paste('CMV_G', SKUGroup, sep=''))
  
  # Imposto: (Receita Bruta - Receita Líquida) / Receita Bruta no período pré-tratamento
  aux <- which(data.painel$D2 == 0)
  imposto <- (sum(data.painel[aux, col_vda_brut]) - sum(data.painel[aux, col_vda]))/sum(data.painel[aux, col_vda_brut])
  
  # CMV: Média do CMV ao longo do pré tratamento e tratamento
  cmv <- data.painel[,col_cmv]
  cmv <- cmv[which(is.na(cmv)==FALSE)]
  cmv <- cmv[which(is.infinite(cmv)==FALSE)]
  cmv <- mean(cmv)
  

  return(list('Imposto' = imposto, 'CMV' = cmv))
}
