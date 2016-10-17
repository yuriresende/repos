auxCountDays = function(inicio=NULL,fim=NULL,N_t=NULL){
  
  if(is.null(N_t)==TRUE){
    data.inicio = as.Date(inicio)
    data.fim = as.Date(fim)
    N.dias = matrix(NA,length(seq(data.inicio,data.fim,"1 month")),7)
  } else{
    data.inicio = as.Date(names(N_t)[1])
    data.fim = as.Date(names(N_t)[length(N_t)])
    N.dias = matrix(NA,length(N_t),7)
  }
  
  countwd <- function(startdate, enddate, weekday){
  x <- seq( startdate, enddate, by=1 )
  y <- weekdays( x )
  sum( y == weekday )
}
  
  n.meses = length(seq(data.inicio,data.fim,"1 month"))
  vetor.dias = as.numeric(format(seq(data.inicio,data.fim,"1 month"),format="%m"))
  
  for(i in 1:n.meses){
    if(i==1){
    x = ymd(data.inicio)
    aux = ymd(x)
    month(aux) <- vetor.dias[i]+1
    y = ymd(aux)-days(1)
    }else{
    aux = ymd(x)
    month(aux) <- vetor.dias[i]+1
    y = ymd(aux)-days(1)
    }
    N.dias[i,1] = countwd(x,y, "segunda")
    N.dias[i,2] = countwd(x,y, "terça")
    N.dias[i,3] = countwd(x,y, "quarta")
    N.dias[i,4] = countwd(x,y, "quinta")
    N.dias[i,5] = countwd(x,y, "sexta")
    N.dias[i,6] = countwd(x,y, "sábado")
    N.dias[i,7] = countwd(x,y, "domingo")
    
    month(x) <- vetor.dias[i]+1
  
  }
  rownames(N.dias) = format(seq(data.inicio,data.fim,"1 month"),format="%Y-%m")
  colnames(N.dias) = c("seg","ter","qua","qui","sex","sab","dom")
  return(N.dias)
}
