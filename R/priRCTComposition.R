priRCTComposition = function(date=NULL){
  load("/RProjetos/Pricing/Dados Atualizados/Composicao Lojas/d.lojas.RData")
  if(is.null(date)){
    pri.composition = d.lojas[[length(d.lojas)]]
  }
  
  else if(date<="2016-08-24"){
    
    pri.composition = d.lojas[[1]]
  }
  
  else if(date>="2016-08-25" & date<=Sys.Date()){
    
    pri.composition = d.lojas[[2]]
  } 
  
  return(pri.composition)
}
