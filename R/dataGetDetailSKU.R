
dataGetDetailSKU=function(depart="D040"){
  require(RJDBC)
  drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver", 
              classPath = "/lib/ojdbc6.jar", "'")
  con <- dbConnect(drv, "jdbc:oracle:thin:@10.150.150.199:1521:puc", 
                   "puc_r", "puc_r#2016")

  tabprod <- dbSendQuery(con, paste("SELECT *
                                    FROM puc.RPO_CADASTRO_ITEM"))
  tabprod <- fetch(tabprod, n = -1)
  
  select=c()
  for(i in 1:length(depart)){
    select=c(select,which(tabprod$DPROD_CD_DEPARTAMENTO==depart[i]))
  }
  tabprod=tabprod[select,]
  
  nam=as.character(tabprod$PROD_TX_ITEM)
  aux=strsplit(nam, "-", fixed = TRUE)
  nam=rep(NA,length(aux))
  for(i in 1:length(nam)){
    nam[i]=aux[[i]][2]
  }
  indexes=data.frame("item"=nam,"code"=as.numeric(tabprod$PROD_CD_ITEM),tabprod$CPROD_CD_CLASSE,tabprod$DPROD_CD_DEPARTAMENTO)
  return(indexes)

}
