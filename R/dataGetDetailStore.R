dataGetDetailStore=function(){
  require(RJDBC)
  drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver", 
              classPath = "/lib/ojdbc6.jar", "'")
  con <- dbConnect(drv, "jdbc:oracle:thin:@10.150.150.199:1521:puc", 
                   "puc_r", "puc_r#2016")

  tabloj <- dbSendQuery(con, paste("SELECT * FROM puc.RPO_CADASTRO_LOJA"))
  tabloj= fetch(tabloj, n = -1) 
  return(tabloj)

}
