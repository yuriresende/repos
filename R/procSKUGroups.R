procSKUGroups = function(date=NULL){
  load("/RProjetos/Pricing/Dados Atualizados/Grupos de SKUs/skus_groups_new.RData")
  if(is.null(date)){
    
    grupo = skus.groups[[length(skus.groups)]]
  }
  
  else if(date<="2016-10-16"){
    
    grupo = skus.groups[[1]]
  }
  
  else if(date>="2016-10-17" & date<="2016-10-21"){
    
    grupo = skus.groups[[2]]
  } 
  
  return(grupo)
}
