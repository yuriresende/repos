dataScheduleSKUGroupUpdate=function(when="sexta 20 00"){
  
  loop=10
  while(loop==10){
    
    check=Sys.time()
    day=weekdays(check)
    hora=hour(check)
    min=minute(check)
    check=paste(day,hora,min)
    
    if(check==when){
      save.base=dataSKUGroupBaseUpdate()
    }
    
  }
}
