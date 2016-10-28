priRCTEvaluate<-function(data.painel, dias.tratamento, SKUGroup){
  
  ###############################################################
  # Modelo Linear com efeitos fixos de loja e controle por dia
  ###############################################################
  
  col_p_medio<-which(colnames(data.painel)==paste('p_medio_G',SKUGroup,sep=''))
  col_vda_qtd<-which(colnames(data.painel)==paste('VDA_QTD_G',SKUGroup,sep=''))
  
  dummy_dia<-model.matrix(~as.factor(data.painel$Data)-1)
  dummy_dia<-dummy_dia[,-1]
  
  fit1 <- plm(data.painel[,col_vda_qtd] ~ data.painel[,'P_SAP'] + dummy_dia, data = data.painel, index=c("Loja"), effect="individual", model="within")
  
  fit_iv <- plm(data.painel[,col_p_medio] ~ data.painel[,'P_SAP'] + dummy_dia, data = data.painel, index=c('Loja'), effect='individual', model='within')
  
  fitted <- data.painel[,col_p_medio] - fit_iv$residuals
  
  fit2 <- plm(data.painel[,col_vda_qtd] ~ fitted + dummy_dia, data = data.painel, index=c("Loja"), effect="individual",model="within")
  
  fit_did <- plm(data.painel[,col_vda_qtd] ~ data.painel[,'D1'] + data.painel[,'D2'] + data.painel[,'D3'] + dummy_dia, data = data.painel, index=c("Loja"), effect="individual", model="within")
  
  ###############################
  # Tabelas para OUTPUT
  ###############################
  
  # For para construir as tabelas de resultados com os coeficientes estimados das regressões e EP's
  nome = c("1","_iv","2");    table=list()
  for (i in 1:3) {
    table[[i]] = cbind(coefficients(eval(parse(text=paste("fit",nome[i],sep="")))),sqrt(diag(vcov(eval(parse(text=paste("fit",nome[i],sep="")))))))
    rownames(table[[i]]) = substr(rownames(table[[i]]),stop=nchar(rownames(table[[i]])[2]),start = nchar(rownames(table[[i]])[2])-9)
    rownames(table[[i]])[1] = "PSAP"
    colnames(table[[i]]) = c("Estimate","EP")
  }
  
  
  table.did = cbind(coefficients(fit_did),sqrt(diag(vcov(fit_did))))
  rownames(table.did) = substr(rownames(table.did),stop=nchar(rownames(table.did)[4]),start = nchar(rownames(table.did)[4])-9)
  rownames(table.did)[c(1,2)] = c("D2","D3")
  colnames(table.did) = c("Estimate","EP")
  
  
  ##################################
  ###### Outputs para priOptPrice
  ##################################
  
  # Calculo do coeficiente agregado, está usando modelo com IV
  beta = fit2$coefficients[1]
  
  b = beta*length(unique(data.painel$Loja))*dias.tratamento
  
  # Número de empresas em tratamento ou controle
  nTrat = length(unique(data.painel$Loja[which(data.painel$D1 == 1)]))
  nCont = length(unique(data.painel$Loja)) - nTrat
  
  # Quantidade total (Q_tratamento + Q_controle) vendida durante o período de tratamento
  Q = sum(data.painel[which(data.painel$D2 == 1), col_vda_qtd])
  
  # Q1 (vendas esperadas caso não ocorresse tratamento)
  Q1 = Q - beta*(delta_p)*nTrat*dias.tratamento
  Q2 = Q + beta*(delta_p)*nCont*dias.tratamento
  
  return(list("OLS com p.médio" = table[[1]],"IV primeiro estágio" = table[[2]], "OLS com IV" = table[[3]],
                 'DID' = table.did, 'coef_agregado' = b, 'Qtd referência' = Q1))  
}
