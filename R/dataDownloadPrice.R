dataDownloadPrice=function(SKUS=NULL){
  dates=c("2013-01-01",as.character(Sys.Date()-1))
  require(RJDBC)
  drv <- JDBC(driverClass="oracle.jdbc.driver.OracleDriver", classPath = "/lib/ojdbc6.jar", "'")
  con <- dbConnect(drv, "jdbc:oracle:thin:@10.150.150.199:1521:puc", "puc_r", "puc_r#2016")
  require(parallel)
  options(scipen=999)
  
  t0=Sys.time()
  ######################################
  ###########tratamento de STRING#######
  
  aux=variables="PRECO_SAP"
  
  aux1=c()
  if(length(SKUS)>1){
    for(i in 1:(length(SKUS)-1)){
      aux1=paste(aux1,SKUS[i],",")
    }
  }
  aux1=paste(aux1,SKUS[length(SKUS)])
  
  p1=c(" SELECT GEO_CD_LOJA, PROD_CD_ITEM*1 as PROD_CD_ITEM, to_char(PER_DT_DATA, 'yyyy-mm-dd') as PER_DT_DATA,")
  p2=c("FROM puc.RPO_PUC_PRECO_VIG WHERE PROD_CD_ITEM IN (")
  p3=c(") GROUP BY GEO_CD_LOJA, PROD_CD_ITEM, PER_DT_DATA,PRECO_SAP")
  sql=paste(p1,aux,p2,aux1,p3)
  
  #############QUERY##############
  ################################
  query=dbSendQuery(con,paste(sql))
  query= fetch(query, n = -1) 
  # checar como que estão atualizando essa base
  sort(unique(query$PER_DT_DATA))
  
  taux=Sys.time()
  cat("time elapsed:", taux-t0, "- Data successfuly downloaded from Oracle, organizing into tables...","\n")
  
  dates=as.Date(dates)
  dates=seq(from=dates[1],to=dates[2],by="days")
  dates=as.character(dates)
  
  #tabloj <- dbSendQuery(con, paste("SELECT * FROM puc.RPO_CADASTRO_LOJA"))
  #tabloj= fetch(tabloj, n = -1) 
  
  loj=unique(query$GEO_CD_LOJA)
  
  table=array(NA,dim=c(length(dates),length(loj),length(SKUS)))
  
  dimnames(table)=list(dates,loj,SKUS)
  
  range=1:nrow(query)
  
  gcfv=function(q,dates,loj,SKUS){
    id1=which(dates==q[3])
    id2=which(loj==q[1])
    id3=which(SKUS==q[2])
    res=c(id1,id2,id3)
    return(res)
  }
  
  n.cores=detectCores()-1
  aux=cbind(query[,1],query[,2],query[,3])
  cl=makeCluster(n.cores)
  indexes=parApply(cl,aux, 1, gcfv,dates=dates,loj=loj,SKUS=SKUS)
  stopCluster(cl)
  
  cat("just a couple more minutes =)","\n")
  
  te=function(q,table,range,indexes){
    for(i in range){
      table[indexes[1,i],indexes[2,i],indexes[3,i]]=q[i]
    }
    return(table)
  }
  
  aux=list()
  for(i in 1:length(variables)){
    aux[[i]]=query[,i+3]
  }
  
  n.cores=min(length(variables),32)
  cl=makeCluster(n.cores)
  tables=parLapply(cl,aux, te, range=range,table=table,indexes=indexes)
  names(tables)=variables
  stopCluster(cl)
  
  t1=Sys.time()
  sqltime=(t1-t0)
  cat("Data successfuly extracted. Processing time of", sqltime, "\n")
  return(tables)
}
