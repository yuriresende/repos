
  
coloreMapa<- function(regiaoEscolhida, tituloEscolhido, paletaEscolhida, bordaEscolhida, hachuraEscolhida, densidadeHachura, anguloEscolhido)
  {
  #Cria a paleta
  paletaCores = colorRampPalette(brewer.pal(9,paletaEscolhida))(101)
  regiaoEscolhida$Cores = NULL
  #Define uma cor para cada local, baseado no valor que foi passado como input
  regiaoEscolhida$Cores = paletaCores[floor(regiaoEscolhida$ValoresTf+1)]
  #Encontra quais locais não constavam na matriz de input
  naoFornecido = which(is.na(regiaoEscolhida$Cores))
  frame()
  layout(matrix(1:2,nrow=1),widths=c(0.9,0.1))
  #Plota o mapa
  plot(regiaoEscolhida, col=regiaoEscolhida$Cores, main=tituloEscolhido, border=bordaEscolhida)
  #Hachura os locais sem dados disponiveis
  plot(regiaoEscolhida[naoFornecido,], add=T, density=densidadeHachura, angle=anguloEscolhido, col=hachuraEscolhida, border=bordaEscolhida)
  xl <- 1
  yb <- 1
  xr <- 1.5
  yt <- 10
  originalPar <- par(no.readonly=TRUE)
  par(mar=c(5.1,0.5,4.1,0.5))
  plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,10),xaxt="n",yaxt="n",bty="n")
  #Criando o degradê que ficará na legenda
  gradient.rect(
    xl,
    head(seq(yb,yt,(yt-yb)/1),-1),
    xr,
    tail(seq(yb,yt,(yt-yb)/1),-1),
    col= paletaCores,
    gradient="y"
  )
  seqLegenda = seq(from = min(regiaoEscolhida$Valores, na.rm=TRUE), to = max(regiaoEscolhida$Valores, na.rm=TRUE), by=(max(regiaoEscolhida$Valores, na.rm=TRUE)-min(regiaoEscolhida$Valores, na.rm=TRUE))/(10-1))
  seqTxtLegenda = seq(from=yb, to=yt, by=(yt-yb)/(yt-1))
  mtext(round(seqLegenda,3),side=2,at=seqTxtLegenda,las=1,cex=0.6)
  #Retorna os parâmetros gráficos para o que estava definido anteriormente
  par(originalPar)
  }
