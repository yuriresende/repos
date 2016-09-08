error.measures=function(pred,real){
  ## == function hrfb == ##
  erro=real-pred
  if(length(ncol(erro))!=0){
    mae=colMeans(abs(erro))
    rmse=sqrt(colMeans((erro^2)))
    mape=colMeans(abs(erro/real))
  }else{
    mae=mean(abs(erro))
    rmse=sqrt(mean((erro^2)))
    mape=mean(abs(erro/real))
  }
  erros=rbind("MAPE"=mape,"RMSE"=rmse,"MAE"=mae)
  return(erros)
}
