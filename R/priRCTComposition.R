priRCTComposition = function(date=NULL){
  
  if(is.null(date)){
    load("/RProjetos/Pricing/Dados Atualizados/Grupos de lojas da aleatorizacao/lojas.aleatorizacao.RData")
    pri.composition = cbind.data.frame(lojas.aleatorizacao$loja,lojas.aleatorizacao$subgrupo)
    colnames(pri.composition) = c("Loja","Grupo")
  }
  
  else if(date<="2016-08-24"){
    load("/RProjetos/Pricing/Dados Atualizados/Grupos de lojas da aleatorizacao/lojas.aleatorizacao.OLD.RData")
    pri.composition = cbind.data.frame(d.loja$Loja,d.loja$GRUPO)
    colnames(pri.composition) = c("Loja","Grupo")
  }
  
  else if(date>="2016-08-25" & date<=Sys.Date()){
    load("/RProjetos/Pricing/Dados Atualizados/Grupos de lojas da aleatorizacao/lojas.aleatorizacao.RData")
    pri.composition = cbind.data.frame(lojas.aleatorizacao$loja,lojas.aleatorizacao$subgrupo)
    colnames(pri.composition) = c("Loja","Grupo")
  } 
  
  return(pri.composition)
}
