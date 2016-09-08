indivual.forecasts=function(real,rate,pred,npred,h){
  # == function hsfy == ##
  
  indpred=list()
  for(i in 1:ncol(pred)){
    indpred[[i]]=pred[,i]*rate[(nrow(rate)-npred-h+1):(nrow(rate)-h),]
  }
  names(indpred)=colnames(pred)
  
  return(list("indpred"=indpred,"real"=real))
}
