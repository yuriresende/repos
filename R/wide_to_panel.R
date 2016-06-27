
wide.to.panel=function(varlist,type="cube"){
  if(type=="cube"){
    nam=dimnames(varlist[[1]])
    panel=expand.grid(nam[[1]],nam[[2]],nam[[3]])
    namp=colnames(panel)
    for(i in 1:length(varlist)){
      panel=data.frame(panel,as.vector(varlist[[i]]))
    }
    colnames(panel)=c(namp,names(varlist))
  }
  if(type=="matrix"){
    nam=dimnames(varlist[[1]])
    panel=expand.grid(nam[[1]],nam[[2]])
    namp=colnames(panel)
    for(i in 1:length(varlist)){
      panel=data.frame(panel,as.vector(varlist[[i]]))
    }
    colnames(panel)=c(namp,names(varlist))
  }
  return(panel)
}
