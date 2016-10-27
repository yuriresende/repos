priStone = function(painel,graphic,graphic.path,graphic.name){
  
  
  Base = painel
  ##########################################
  # LOAD PACKAGES AND SOURCE FILES
  ##########################################
  require(plm)
  require(stargazer)
  require(DLAB)
  
  ##########################################
  # Introduz DUMMY dia da semana
  ##########################################
  Base$Dia.semana = weekdays(as.Date(Base$data))
  Base$Dseg = ifelse(Base$Dia.semana=="segunda",1,0)
  Base$Dter = ifelse(Base$Dia.semana=="terça",1,0)
  Base$Dqua = ifelse(Base$Dia.semana=="quarta",1,0)
  Base$Dqui = ifelse(Base$Dia.semana=="quinta",1,0)
  Base$Dsex = ifelse(Base$Dia.semana=="sexta",1,0)
  Base$Dsab = ifelse(Base$Dia.semana=="sábado",1,0)
  Base$Ddom = ifelse(Base$Dia.semana=="domingo",1,0)
  
  ##########################################
  # Coloca 0 em quem é NA!
  ##########################################
  PeQ = Base
  PeQ[is.na(PeQ)]=0
  
  ##########################################
  # SET 1 - BASIC ELASTICITIES
  ##########################################
  t1=colnames(PeQ)
  aux.preco = grep("pVec_",t1, value=T)
  aux.qtd = grep("VDA_QTD_",t1, value=T)
  ind.preco = match(aux.preco,colnames(PeQ))
  ind.qtd = match(aux.qtd,colnames(PeQ))
  
  limiar = 20
  aux = colSums(PeQ[,ind.preco]==0,na.rm=TRUE)/1000 < 27+limiar
  
  # Remove items which are recent and do not have a considerable number of observations
  data <- PeQ
  for (i in 1:length(ind.preco)){
    
    if (aux[i]==1){
      
      texteval = paste("data <- subset(data,pVec_G",i,">0)",sep="")
      eval(parse(text=texteval))
      
    }
    
  }
  
  var.validas = names(aux[c(which(aux==TRUE))])
  grupos.removidos = paste("G",sub("pVec_G", "", setdiff(colnames(PeQ[,ind.preco]),var.validas)),sep="")
  data <- plm.data(data,c("loja","data"))
  
  ##########################################################################################################################
  # Modelo Stone
  ##########################################################################################################################
  
  ind.i = sub("pVec_G", "", var.validas)
  
  ind.Q = c()
  for (i in 1:(length(var.validas))) {
    ind.Q[i] = paste("VDA_QTD_SKU",ind.i[i],sep="")
  }
  
  matriz.coef.Mod.Stone = matrix(NA,length(ind.Q)+7,length(ind.Q))
  
  RHS = c("Dseg+Dter+Dqua+Dqui+Dsex+Dsab+Ddom+aux.reg")
  for (i in 1:(length(var.validas))) {
    RHS = paste(RHS,"+log(",var.validas[i],")",sep="")
  }
  
  LHS = c()
  for (i in 1:(length(var.validas))) {
    LHS[i] = paste("log(VDA_QTD_SKU",ind.i[i],")",sep="")
  }
  
  lnX.p = c("log(aux.data$X)")
  for (i in 1:(length(var.validas))) {
    lnX.p = paste(lnX.p,"-aux.data$W_G",ind.i[i],"*log(aux.data$pVec_G",ind.i[i],")",sep="")
  }
  
  for( k in 1:length(ind.i)){
    
    texteval = paste("data_G",ind.i[k],"<- subset(data,",ind.Q[k],">0)",sep="")
    eval(parse(text=texteval))
    
    texteval = paste("aux.data = data_G",ind.i[k],sep="")
    eval(parse(text=texteval))
    
    texteval = paste("aux.reg = ",lnX.p,sep="")
    eval(parse(text=texteval))
    
    texteval = paste("fit",ind.Q[k]," <- plm(",LHS[k]," ~ ",RHS,", data=data_G",ind.i[k],", index=c(\"loja\",\"data\"),effect=\"individual\",model=\"within\")",sep="")
    eval(parse(text=texteval))
    
    texteval = paste("matriz.coef.Mod.Stone[,",k,"] = fit",ind.Q[k],"$coefficients",sep="")
    eval(parse(text=texteval))
    
    texteval = paste("rm(data_G",ind.i[k],")",sep="")
    eval(parse(text=texteval))
    
    cat("k=",k,"\n")
  }
  
  novos.coefs.modStone = (1-exp(-matriz.coef.Mod.Stone))/(1+exp(-matriz.coef.Mod.Stone))
  
  # Para o futuro salvar com stargazer
  
  # fit = c()
  # for (i in 1:(length(var.validas))) {
  #   if(i==length(var.validas)){fit = paste(fit,"fit",ind.Q[i],sep="")}else{
  #     fit = paste(fit,"fit",ind.Q[i],",",sep="")}
  # }
  # 
  # texteval = paste("stargazer(",fit,",type=\"latex\")",sep="")
  # eval(parse(text=texteval))
  # 
  # texteval = paste("rm(",fit,")",sep="")
  # eval(parse(text=texteval))
  # 
  ###############################################  
  # Assign names to the output
  ###############################################
  ind.i = sub("pVec_G", "", var.validas)
  
  ind.P = ind.Q = c()
  for (i in 1:(length(var.validas))) {
    ind.Q[i] = paste("Q_G",ind.i[i],sep="")
    ind.P[i] = paste("P_G",ind.i[i],sep="")
  }
  
  colnames(matriz.coef.Mod.Stone) = colnames(novos.coefs.modStone) =ind.Q
  
  rownames(matriz.coef.Mod.Stone) = rownames(novos.coefs.modStone) = c("Dseg","Dter","Dqua","Dqui","Dsex","Dsab","log(X/P)",ind.P)
  
  ###############################################  
  # AZULEJOS
  ###############################################
  
  if(graphic==TRUE){
    require(corrplot)
    if(graphic.path==FALSE){
      corrplot(matriz.coef.Mod.Stone, is.corr=FALSE, method="circle",tl.col="black",tl.cex = .8, cl.ratio = 0.3)
      corrplot(novos.coefs.modStone, is.corr=FALSE, method="circle",tl.col="black",tl.cex = .8, cl.ratio = 0.3)
    } else{
      pdf(paste(graphic.path,"/",graphic.name,".pdf",sep=""),width=5,height = 7)
      corrplot(matriz.coef.Mod.Stone, is.corr=FALSE, method="circle",tl.col="black",tl.cex = .8, cl.ratio = 0.3)
      dev.off()
      pdf(paste(graphic.path,"/",graphic.name,"_transform.pdf",sep=""),width=5,height = 7)
      corrplot(novos.coefs.modStone, is.corr=FALSE, method="circle",tl.col="black",tl.cex = .8, cl.ratio = 0.3)
      dev.off()
    }
    
  }
  
  return(list("Matriz coeficientes" = matriz.coef.Mod.Stone, "Matriz coeficientes transformados" = novos.coefs.modStone,
              "Grupos Estimados" = var.validas, "Grupos Excluídos" = grupos.removidos))
}
