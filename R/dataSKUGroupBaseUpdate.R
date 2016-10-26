dataSKUGroupBaseUpdate=function(groups=NULL){

    
  variables=c("SUM(VDA_BRT)","SUM(VDA_QTD)", "SUM(CMV)", "SUM(VDA)", "SUM(EST_QTD)", "SUM(EST_TRAN)")
  
  cat("
      #####################################################################
      #### Começando download de dados de grupos                       ####
      #####################################################################")
  
  cat(paste("\n Inicialização em ",Sys.time()))
  data0=Sys.time()
  
  skus.groups=read.table("/RProjetos/Dados/gruposD040.csv",sep=";",header=TRUE)[,c(4,1)]
  colnames(skus.groups) <- c("Grupo","SKU")
  
  if(length(groups)==0){
    groups=as.vector(unique(skus.groups$Grupo))
    cat("\n The group argument was not informed. The function will download data for all groups. \n") 
  }
  
  skus.groups$Grupo=paste("G",skus.groups$Grupo,sep="")
  
  store=list()
  rem=0
  for(i in groups){
    SKUS=subset(skus.groups, Grupo==paste("G",i,sep=""))$SKU
    data=dataDownload(variables,"group-skus",SKUS)
    eval(parse(text=paste("store$","\"",i,"\"","=data",sep="")))
    file=paste("/RProjetos/Dados/auxiliary-download-",i,".RData",sep="")
    save(store,file=file)
    cat("\n Group",i,"successfully downloaded. \n")
    
    if(rem>=groups[1]){
      file=paste("/RProjetos/Dados/auxiliary-download-",rem,".RData",sep="")
      file.remove(file)
    }
    rem=i
  }
  
  painel.list=lapply(store,dataWideToPanel,type="matrix",sku.column=FALSE)
  
    
  cat("\n
      #####################################################################
      #### Download de grupos finalizado com sucesso                   ####
      #####################################################################")
  
   cat("\n
      #####################################################################
      #### Baixando dados de atividade de loja                         ####
      #####################################################################")
    
   atividade=dataDownload(type="opened-stores", depart="D040") 
   atividade=dataWideToPanel(atividade,type="matrix")[,3:4]
    
   cat("\n
      #####################################################################
      #### Download de dados de atividade de lojas finalizado com sucesso##
      #####################################################################")
    
  cat("\n
      #####################################################################
      #### Iniciando construção de painel  e transformação de variaveis####
      #####################################################################")
  
  # = criar pmedio = #
  p_medio=lapply(painel.list,function(x) cbind(x[,1:2],x$`SUM(VDA_BRT)`/x$`SUM(VDA_QTD)`))
  
  # = Criar p quant 99 = #
  qfun=function(x){
    x=x[!is.na(x)]
    if(length(x)==0){return(NA)}
    aux=sort(x)
    return(aux[round(length(aux)*0.99)])
  }
  p_99=lapply(p_medio,function(x) aggregate(x,list(x[,1]),qfun))
  
  for(i in 1:length(p_99)){
    p_99[[i]]=p_99[[i]][match(p_medio[[i]][,1],p_99[[i]][,1]),4]
  }
  
  # = juntando p_99 e p_medio
  p_medio=lapply(p_medio, function(x) x[,-c(1:2)])
  p_medio=Reduce("cbind",p_medio)
  p_99=Reduce("cbind",p_99)
  colnames(p_medio)=paste("p_medio_G",groups,sep=""); colnames(p_99)=paste("p_99_G",groups,sep="")
  p=cbind(p_medio*1,p_99*1)*1
  row.names(p)=NULL
  
  # = criar X e W = #
  aux=lapply(painel.list, function(x) x$`SUM(VDA_BRT)`)
  aux=Reduce("cbind",aux)
  X=rowSums(aux,na.rm = TRUE)
  w=aux/X
  colnames(w)=paste("w_G",groups,sep="")
  
  
  varlist=gsub("SUM","",variables)
  varlist=gsub("\\(","",varlist)
  varlist=gsub("\\)","",varlist)
  indexes=painel.list[[1]][,1:2]
  colnames(indexes)=c("Data","Loja")
  painel.list=lapply(painel.list,function(x) x[,-c(1:2)])
  for(i in 1:length(groups)){
    colnames(painel.list[[i]])=paste(varlist,"_G",groups[i],sep="")
  }
  painel=cbind(indexes,Reduce("cbind",painel.list),w,X,p,atividade)
  
  save(painel,file="/RProjetos/Dados/auxiliary-painel.RData")
  
  cat("\n
      #####################################################################
      #### Transformação de variáveis concluida com sucesso            ####
      #####################################################################")
  
  cat("\n
      #####################################################################
      #### Introduzido variaveis finais                                ####
      #####################################################################")
  
  painel$Dia.semana = weekdays(as.Date(painel$Data))
  painel$Dia.semana <- ifelse(painel$Dia.semana=="terça", "terca", ifelse(painel$Dia.semana=="sábado", "sabado", painel$Dia.semana))
  painel$Dseg = ifelse(painel$Dia.semana=="segunda",1,0)
  painel$Dter = ifelse(painel$Dia.semana=="terca",1,0)
  painel$Dqua = ifelse(painel$Dia.semana=="quarta",1,0)
  painel$Dqui = ifelse(painel$Dia.semana=="quinta",1,0)
  painel$Dsex = ifelse(painel$Dia.semana=="sexta",1,0)
  painel$Dsab = ifelse(painel$Dia.semana=="sabado",1,0)
  painel$Ddom = ifelse(painel$Dia.semana=="domingo",1,0)
  
  
  #### Anexando dados de cupom ao painel ####
  # Baixa dados de cupons do D40 por SKU x dia x loja
  drv <- JDBC(driverClass="oracle.jdbc.driver.OracleDriver", classPath = "/lib/ojdbc6.jar", "'")
  con <- dbConnect(drv, "jdbc:oracle:thin:@10.150.150.199:1521:puc", "puc_r", "puc_r#2016")
  
  t.cupons.d40.por.sku <- dbSendQuery(con, "SELECT * FROM puc.RPO_PUC_EXTR_CUPOM_LOJA_ITEM")
  t.cupons.d40.por.sku <- fetch(t.cupons.d40.por.sku, n = -1)
  t.cupons.d40.por.sku$data <- as.Date(t.cupons.d40.por.sku$DT_MOVIMENTO_VENDA)
  t.cupons.d40.por.sku$DT_MOVIMENTO_VENDA <- NULL
  t.cupons.d40.por.sku$SKU <- substr(t.cupons.d40.por.sku$CO_PRODUTO_SAP,12,18)
  t.cupons.d40.por.sku$CO_PRODUTO_SAP <- NULL
  t.cupons.d40.agregada <- aggregate(QTD_CUPOM ~ data + CO_LOJA, t.cupons.d40.por.sku, sum)
  
  # Baixa dados de cupons por dia x loja
  t.cupons <- dbSendQuery(con, "SELECT * FROM  puc.RPO_FLUXO_CLI")
  t.cupons <- fetch(t.cupons, n = -1)
  t.cupons$data <- as.Date(t.cupons$PER_DT_DATA)
  t.cupons$PER_DT_DATA <- NULL
  t.cupons <- t.cupons[order(t.cupons$data),]
  
  # Criando a chave nas 3 painels, para possibilitar o match
  painel$chave <- paste(painel$Loja,painel$Data,sep=".")
  t.cupons$chave <- paste(t.cupons$GEO_CD_LOJA, t.cupons$data, sep=".")
  t.cupons.d40.agregada$chave <- paste(t.cupons.d40.agregada$CO_LOJA,t.cupons.d40.agregada$data, sep=".")
  
  # Botando as novas variaveis na painel
  painel$proxy.cupons.d40 <- t.cupons.d40.agregada$QTD_CUPOM[match(painel$chave,t.cupons.d40.agregada$chave)]
  painel$cupons.totais <- t.cupons$QTD_CUPOM[match(painel$chave,t.cupons$chave)]
  file.copy("/RProjetos/Dados/painel-atualizado/base-atualizada.RData","/RProjetos/Dados/paineis-antigos/base-antiga.RData" ,     copy.mode = TRUE, copy.date = FALSE)
  base=painel
  save(base,file="/RProjetos/Dados/painel-atualizado/base-atualizada.RData")
  
  file=paste("/RProjetos/Dados/auxiliary-download-",tail(groups,1),".RData",sep="")
  file.remove(file)
  file.remove("/RProjetos/Dados/auxiliary-painel.RData")
  
  cat("\n 
      #####################################################################
      #### Procedimento concluido com Sucesso                          ####
      #####################################################################")
  
  data1=Sys.time()
  cat(paste("\n Término em ",Sys.time(),". Tempo total de", data1-data0,"." ,sep="" ))
}
