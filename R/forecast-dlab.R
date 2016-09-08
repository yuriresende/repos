forecasts.dlab=function(y,X,h=1,npred=8,alpha.sh=0,alpha.ew=0.95){
  ## ==== funcao hmfv ==== ##
  
  if(min(y)<=0){
    logvar="não foi possível fazer log-variação devido a valores negativos em y"
    U=1
  }else{
    U=0
  }
  
  
  aux=embed(y,h+1)
  y=aux[,1]
  lagy=aux[,ncol(aux)]
  T=length(y)
  Xnv=tail(X,T)
  Xall.nv=cbind(lagy,Xnv)
  
  if(U==0){
    ldy=diff(log(y),1)
    ldlagy=diff(log(lagy),1)
    TT=length(ldy)
    Xlv=tail(X,TT)
    Xall.lv=cbind(ldlagy,Xlv)
  }
  

  
  ## === Autorregressivo === ##
  # = Nivel = #
  {
    save.ar.niv=rep(NA,npred)
    for(i in 1:npred){
      model=lm(y[1:(T-npred-1+i)]~lagy[1:(T-npred-1+i)])
      prev=c(1,lagy[(T-npred+i)])%*%coef(model)
      save.ar.niv[i]=prev
    }
  }
  # = log variacao = #
  if(U==0){
    save.ar.lv=rep(NA,npred)
    for(i in 1:npred){
      model=lm(ldy[1:(TT-npred-1+i)]~ldlagy[1:(TT-npred-1+i)])
      prev=c(1,ldlagy[(TT-npred+i)])%*%coef(model)
      save.ar.lv[i]=prev
    }
    save.ar.lv=exp(save.ar.lv)*y[(length(y)-npred):(length(y)-1)]
  }
  
  
  ## === AR X === ##
  # = Nivel = #
  {
    save.arx.niv=rep(NA,npred)
    for(i in 1:npred){
      model=lm(y[1:(T-npred-1+i)]~Xall.nv[1:(T-npred-1+i),])
      prev=c(1,Xall.nv[(T-npred+i),])%*%coef(model)
      save.arx.niv[i]=prev
    }
  }
  # = log variacao = #
  if(U==0){
    save.arx.lv=rep(NA,npred)
    for(i in 1:npred){
      model=lm(ldy[1:(TT-npred-1+i)]~Xall.lv[1:(TT-npred-1+i),])
      prev=c(1,Xall.lv[(TT-npred+i),])%*%coef(model)
      save.arx.lv[i]=prev
    }
    save.arx.lv=exp(save.arx.lv)*y[(length(y)-npred):(length(y)-1)]
  }
  
  
  ## == shrinkage == ##
  # = Nivel = #
  {
    save.sh.niv=rep(NA,npred)
    
    for(i in 1:npred){
      model=biclasso(Xall.nv[1:(T-npred-1+i),],y[1:(T-npred-1+i)],alpha=alpha.sh)
      prev=c(1,Xall.nv[(T-npred+i),])%*%model$coef
      save.sh.niv[i]=prev
    }
  }
  
  # = log variacao = #
  if(U==0){
    save.sh.lv=rep(NA,npred)
    
    for(i in 1:npred){
      model=biclasso(Xall.lv[1:(TT-npred-1+i),],ldy[1:(TT-npred-1+i)],alpha=alpha.sh)
      prev=c(1,Xall.lv[(TT-npred+i),])%*%model$coef
      save.sh.lv[i]=prev
    }
    save.sh.lv=exp(save.sh.lv)*y[(length(y)-npred):(length(y)-1)]
  }
  
  
  ## == EWMA == ##
  # = Nivel = #
  {
    save.ew.niv=rep(NA,npred)
    
    for(i in 1:npred){
      prev=ses(y[1:(T-npred-1+i)],alpha=alpha.ew,h=h)$mean[h]
      save.ew.niv[i]=prev
    }
  }
  
  # = log variacao = #
  if(U==0){
    save.ew.lv=rep(NA,npred)
    
    for(i in 1:npred){
      model=biclasso(Xall.lv[1:(TT-npred-1+i),],ldy[1:(TT-npred-1+i)],alpha=alpha.sh)
      prev=ses(ldy[1:(TT-npred-1+i)],alpha=alpha.ew,h=h)$mean[h]
      save.ew.lv[i]=prev
    }
    save.ew.lv=exp(save.ew.lv)*y[(length(y)-npred):(length(y)-1)]
  }
  
  
  ## == Random Forest == ##
  # = Nivel = #
  {
    save.rf.niv=rep(NA,npred)
    
    for(i in 1:npred){
      model=randomForest(Xall.nv[1:(T-npred-1+i),],y[1:(T-npred-1+i)])
      prev=predict(model,Xall.nv[(T-npred+i),])
      save.rf.niv[i]=prev
    }
  }
  
  # = log variacao = #
  if(U==0){
    save.rf.lv=rep(NA,npred)
    
    for(i in 1:npred){
      model=randomForest(Xall.lv[1:(TT-npred-1+i),],ldy[1:(TT-npred-1+i)])
      prev=predict(model,Xall.lv[(TT-npred+i),])
      save.rf.lv[i]=prev
    }
    save.rf.lv=exp(save.rf.lv)*y[(length(y)-npred):(length(y)-1)]
  }
  
  nivel=cbind(save.ar.niv,save.arx.niv,save.ew.niv,save.sh.niv,save.rf.niv)
  nivel=cbind(nivel,rowMeans(nivel))
  if(U==0){
    logvar=cbind(save.ar.lv,save.arx.lv,save.ew.lv,save.sh.lv,save.rf.lv)
    logvar=cbind(logvar,rowMeans(logvar))
    colnames(logvar)=c("AR","ARX","EWMA","LASSO","Random Forest","AVG")
  }
  colnames(nivel)=c("AR","ARX","EWMA","LASSO","Random Forest","AVG")
  real=tail(y,npred)
  
  return(list("logvar"=logvar,"nivel"=nivel,"real"=real))
}
