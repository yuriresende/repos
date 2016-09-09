
data.download=function(variables=NULL,type="cia",SKUS=NULL, depart=c("D040")){
  dates=c("2013-01-01",as.character(Sys.Date()-1))
  require(RJDBC)
  drv <- JDBC(driverClass="oracle.jdbc.driver.OracleDriver", classPath = "/lib/ojdbc6.jar", "'")
  con <- dbConnect(drv, "jdbc:oracle:thin:@10.150.150.199:1521:puc", "puc_r", "puc_r#2016")
  require(parallel)
  options(scipen=999)
  if(type=="cia"){
    
    t0=Sys.time()
    
    ######################################
    ###########tratamento de STRING#######
    v=strsplit(variables,"(",fixed = TRUE)
    variables1=variables
    for(i in 1:length(variables1)){
      variables1[i]=paste(v[[i]][1],"(",   "t1.",v[[i]][2])
    }
    
    aux=c()
    if(length(variables)>1){
      for(i in 1:(length(variables1)-1)){
        aux=paste(aux,variables1[i],",")
      }
    }
    aux=paste(aux,variables1[length(variables1)])
    
    depart1=depart
    for(i in 1:length(depart)){
      depart1[i]=paste("'",depart1[i],"'",sep="")
    }
    aux1=c()
    if(length(depart1)>1){
      for(i in 1:(length(depart1)-1)){
        aux1=paste(aux1,depart1[i],",")
      }
    }
    aux1=paste(aux1,depart1[length(depart1)])
    
    p1=c("SELECT t1.PROD_CD_ITEM*1 as PROD_CD_ITEM, to_char(t1.PER_DT_DATA, 'yyyy-mm-dd') as PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO,")
    p2=c("FROM puc.RPO_CADASTRO_ITEM t2 INNER JOIN RPO_VDAEST_TRANSACIONAL t1 ON t2.PROD_CD_ITEM=t1.PROD_CD_ITEM WHERE t2.DPROD_CD_DEPARTAMENTO IN (")
    p3=c(") GROUP BY t1.PROD_CD_ITEM, t1.PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO")
    sql=paste(p1,aux,p2,aux1,p3)
    
    #############QUERY##############
    ################################
    query=dbSendQuery(con,paste(sql))
    query= fetch(query, n = -1) 
    
    taux=Sys.time()
    cat("time elapsed:", taux-t0, "- Data successfuly downloaded from Oracle, organizing into tables...","\n")
    
    dates=as.Date(dates)
    dates=seq(from=dates[1],to=dates[2],by="days")
    dates=as.character(dates)
    
    prod=as.character(unique(query$PROD_CD_ITEM))
    
    table=matrix(NA,length(dates),length(prod))
    
    rownames(table)=dates
    colnames(table)=prod
    
    range=1:nrow(query)
    gcfv=function(q,dates,prod){
      q=unlist(c(q))
      id1=which(dates==q[2])
      id2=which(prod==q[1])
      res=c(id1,id2)
      return(res)
    }
    
    n.cores=detectCores()-10
    aux=cbind(query[,1],query[,2])
    cl=makeCluster(n.cores)
    indexes=parApply(cl,aux, 1, gcfv,dates=dates,prod=prod)
    stopCluster(cl)
    
    cat("just a couple more minutes =)","\n")
    
    te=function(q,table,range,indexes){
      for(i in range){
        table[indexes[1,i],indexes[2,i]]=q[i]
      }
      return(table)
    }
    
    aux=list()
    for(i in 1:length(variables)){
      aux[[i]]=query[,i+3]####3 pq uma coluna e dpto
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
  
  if(type=="cd"){
    
    t0=Sys.time()
    
    v=strsplit(variables,"(",fixed = TRUE)
    variables1=variables
    for(i in 1:length(variables1)){
      variables1[i]=paste(v[[i]][1],"(",   "t1.",v[[i]][2])
    }
    
    aux=c()
    if(length(variables)>1){
      for(i in 1:(length(variables1)-1)){
        aux=paste(aux,variables1[i],",")
      }
    }
    aux=paste(aux,variables1[length(variables1)])
    
    depart1=depart
    for(i in 1:length(depart)){
      depart1[i]=paste("'",depart1[i],"'",sep="")
    }
    aux1=c()
    if(length(depart1)>1){
      for(i in 1:(length(depart1)-1)){
        aux1=paste(aux1,depart1[i],",")
      }
    }
    aux1=paste(aux1,depart1[length(depart1)])
    
    p1=c("SELECT t1.PROD_CD_ITEM*1 as PROD_CD_ITEM, to_char(t1.PER_DT_DATA, 'yyyy-mm-dd') as PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO, t3.GEO_TX_CENTRO_DISTRIBUICAO,")
    p2=c("FROM puc.RPO_CADASTRO_ITEM t2 INNER JOIN RPO_VDAEST_TRANSACIONAL t1 INNER JOIN puc.RPO_CADASTRO_LOJA t3 ON t3.GEO_CD_LOJA=t1.GEO_CD_LOJA ON (t2.PROD_CD_ITEM*1)=(t1.PROD_CD_ITEM*1) WHERE t2.DPROD_CD_DEPARTAMENTO IN(")
    p3=c(") GROUP BY t1.PROD_CD_ITEM, t1.PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO, t3.GEO_TX_CENTRO_DISTRIBUICAO")
    sql=paste(p1,aux,p2,aux1,p3)
    
    query= dbSendQuery(con, sql)
    query <- fetch(query, n = -1)
    
    aux=which(is.na(query[,4]))
    query1=query[-c(aux),]
    
    taux=Sys.time()
    cat("time elapsed:", taux-t0, "- Data successfuly downloaded from Oracle, organizing into tables...","\n")
    
    dates=as.Date(dates)
    dates=seq(from=dates[1],to=dates[2],by="days")
    dates=as.character(dates)
    
    #tabprod <- dbSendQuery(con, "SELECT * FROM puc.RPO_CADASTRO_ITEM")
    #tabprod <- fetch(tabprod, n = -1)
    
    prod=as.character(unique(query1$PROD_CD_ITEM))
    
    cd.names=as.vector(unique(query1[,4]))
    
    table=array(NA,dim=c(length(dates),length(prod),length(cd.names)))
    
    dimnames(table)=list(dates,prod,cd.names)
    
    range=1:nrow(query1)
    
    gcfv=function(q,dates,prod,cd.names){
      id1=which(dates==q[2])
      id2=which(prod==q[1])
      id3=which(cd.names==q[3])
      res=c(id1,id2,id3)
      return(res)
    }
    
    n.cores=detectCores()-1
    aux = cbind(query1[,1],query1[,2],query1[,4])
    cl=makeCluster(n.cores)
    indexes=parApply(cl,aux, 1, gcfv,dates=dates,prod=prod,cd.names=cd.names)
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
      aux[[i]]=query1[,i+4]
    }
    
    n.cores=min(length(variables),32)
    cl=makeCluster(n.cores)
    tables=parLapply(cl,aux, te, range=range,table=table,indexes=indexes)
    names(tables)=variables
    stopCluster(cl)
    
    dim(tables[[1]])
    
    t1=Sys.time()
    sqltime=(t1-t0)
    cat("Data successfuly extracted. Processing time of", sqltime, "\n")
    return(tables)
  }
  
  if(type=="skus"){
    
    t0=Sys.time()
    ######################################
    ###########tratamento de STRING#######
    
    aux=c()
    if(length(variables)>1){
      for(i in 1:(length(variables)-1)){
        aux=paste(aux,variables[i],",")
      }
    }
    aux=paste(aux,variables[length(variables)])
    
    aux1=c()
    if(length(SKUS)>1){
      for(i in 1:(length(SKUS)-1)){
        aux1=paste(aux1,SKUS[i],",")
      }
    }
    aux1=paste(aux1,SKUS[length(SKUS)])
    
    p1=c(" SELECT GEO_CD_LOJA, PROD_CD_ITEM*1 as PROD_CD_ITEM, to_char(PER_DT_DATA, 'yyyy-mm-dd') as PER_DT_DATA,")
    p2=c("FROM RPO_VDAEST_TRANSACIONAL WHERE PROD_CD_ITEM IN (")
    p3=c(") GROUP BY GEO_CD_LOJA, PROD_CD_ITEM, PER_DT_DATA")
    sql=paste(p1,aux,p2,aux1,p3)
    
    #############QUERY##############
    ################################
    query=dbSendQuery(con,paste(sql))
    query= fetch(query, n = -1) 
    
    
    taux=Sys.time()
    cat("time elapsed:", taux-t0, "- Data successfuly downloaded from Oracle, organizing into tables...","\n")
    
    dates=as.Date(dates)
    dates=seq(from=dates[1],to=dates[2],by="days")
    dates=as.character(dates)
    
    tabloj <- dbSendQuery(con, paste("SELECT * FROM puc.RPO_CADASTRO_LOJA"))
    tabloj= fetch(tabloj, n = -1) 
    
    
    loj=tabloj$GEO_CD_LOJA
    
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
  
  if(type=="opened-stores"){
    
    variables=c("SUM(VDA_QTD)")
    t0=Sys.time()
    
    v=strsplit(variables,"(",fixed = TRUE)
    variables1=variables
    for(i in 1:length(variables1)){
      variables1[i]=paste(v[[i]][1],"(",   "t1.",v[[i]][2])
    }
    
    aux=c()
    if(length(variables)>1){
      for(i in 1:(length(variables1)-1)){
        aux=paste(aux,variables1[i],",")
      }
    }
    aux=paste(aux,variables1[length(variables1)])
    
    depart1=depart
    for(i in 1:length(depart)){
      depart1[i]=paste("'",depart1[i],"'",sep="")
    }
    aux1=c()
    if(length(depart1)>1){
      for(i in 1:(length(depart1)-1)){
        aux1=paste(aux1,depart1[i],",")
      }
    }
    aux1=paste(aux1,depart1[length(depart1)])
    
    p1=c("SELECT t1.GEO_CD_LOJA, to_char(t1.PER_DT_DATA, 'yyyy-mm-dd') as PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO,")
    p2=c("FROM puc.RPO_CADASTRO_ITEM t2 INNER JOIN RPO_VDAEST_TRANSACIONAL t1 ON (t2.PROD_CD_ITEM*1)=(t1.PROD_CD_ITEM*1) WHERE t2.DPROD_CD_DEPARTAMENTO IN (")
    p3=c(") GROUP BY t1.GEO_CD_LOJA, t1.PER_DT_DATA, t2.DPROD_CD_DEPARTAMENTO")
    sql=paste(p1,aux,p2,aux1,p3)
    
    query= dbSendQuery(con, sql)
    query <- fetch(query, n = -1)
    taux=Sys.time() 
    cat("time elapsed:", taux-t0, "- Data successfuly downloaded from Oracle, organizing into tables...","\n")
    
    
    #########################################
    dates=as.Date(dates)
    dates=seq(from=dates[1],to=dates[2],by="days")
    dates=as.character(dates)
    
    tabloj <- dbSendQuery(con, "SELECT * FROM puc.RPO_CADASTRO_LOJA")
    tabloj <- fetch(tabloj, n = -1)
    
    loj=tabloj$GEO_CD_LOJA
    
    table=matrix(NA,length(dates),nrow(tabloj))
    
    rownames(table)=dates
    colnames(table)=loj
    
    range=1:nrow(query)
    gcfv=function(q,dates,loj){
      id1=which(dates==q[2])
      id2=which(loj==q[1])
      res=c(id1,id2)
      return(res)
    }
    
    n.cores=detectCores()-1
    cl=makeCluster(n.cores)
    indexes=parApply(cl,query, 1, gcfv,dates=dates,loj=loj)
    stopCluster(cl)
    
    cat("just a couple more minutes =)","\n")
    
    te=function(q,table,range,indexes){
      for(i in range){
        table[indexes[1,i],indexes[2,i]]=q[i]
      }
      return(table)
    }
    
    aux=list(query[,4])
    tables=lapply(aux, te, range=range,table=table,indexes=indexes)
    activity=tables[[1]]
    opened=tables[[1]]
    opened[is.na(opened)]=0
    opened[opened!=0]=1
    tables=list("activity"=activity,"opened"=opened)
    
    t1=Sys.time()
    sqltime=(t1-t0)
    cat("Data successfuly extracted. Processing time of", sqltime, "\n")
    return(tables)
  }
  
}

