
dataWideToPanel=function (varlist, type = "cube",sku.column=TRUE) 
{
  if (type == "cube") {
    nam = dimnames(varlist[[1]])
    panel = expand.grid(nam[[1]], nam[[2]], nam[[3]])
    namp = colnames(panel)
    for (i in 1:length(varlist)) {
      panel = data.frame(panel, as.vector(varlist[[i]]))
    }
    colnames(panel) = c(namp, names(varlist))
    
    if(sku.column==TRUE){
      skus=as.vector(unique(panel[,3]))
      panel1=data.frame(panel[1:(nrow(panel)/length(skus)),1:2])
      for(i in 1:length(skus)){
        panel1=data.frame(panel1,panel[which(panel[,3]==skus[i]),-c(1:3)])
      }
      nam=c()
      aux=colnames(panel)[-c(1:3)]
      for(i in 1:length(skus)){
        nam=c(nam,paste(aux,skus[i]) )
      }
      colnames(panel1)=c("Data","Loja",nam)
      panel=panel1
    }
    
  }
  if (type == "matrix") {
    nam = dimnames(varlist[[1]])
    panel = expand.grid(nam[[1]], nam[[2]])
    namp = colnames(panel)
    for (i in 1:length(varlist)) {
      panel = data.frame(panel, as.vector(varlist[[i]]))
    }
    colnames(panel) = c(namp, names(varlist))
  }
  return(panel)
}
