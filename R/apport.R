apportionment=function(data){
  ## == function pgfs == ##
  app=data/rowSums(data,na.rm = TRUE)
  y=rowSums(data,na.rm=TRUE)
  
  return(list("agg.data"=y,"apportionment"=app))
}
