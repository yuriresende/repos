  
coloreMapaEstados <- function(matrizInput,titulo="",paleta="YlOrRd", borda="black", hachura="blue", densidade_hachura=10, angulo_hachura=45)
  {
  require("maptools")
  require("RColorBrewer")
  require("plotrix")
  library("maptools")
  library("RColorBrewer")
  library("plotrix")
  
  tituloEscolhido = titulo
  bordaEscolhida = borda 
  paletaEscolhida = paleta
  hachuraEscolhida = hachura
  densidadeHachura = densidade_hachura
  anguloHachura = angulo_hachura
  coresDisponiveis = rownames(brewer.pal.info)
  if(!is.element(paletaEscolhida, coresDisponiveis))
  {
    originalPar <- par(no.readonly=TRUE)
    layout(matrix(1:1,nrow=1),widths=c(0.9,0.1))
    display.brewer.all()
    stop("Houve um erro pois a paleta escolhida para o mapa deve ser uma paleta do pacote RColorBrewer. As opções disponiveis são as que estão no plot.")
    par(originalPar)
  }
  Estados  <- readShapePoly(fn='/RProjetos/Mapas/LM_UF.shp')
  regiaoEscolhida = Estados
  regiaoEscolhida$sigla= as.character(regiaoEscolhida$siglaUf)  
  regiaoEscolhida$Valores = as.numeric(matrizInput[match(regiaoEscolhida$sigla, matrizInput[,1]),2])
  regiaoEscolhida$ValoresTf = 100*(regiaoEscolhida$Valores - min(regiaoEscolhida$Valores, na.rm=TRUE))/(max(regiaoEscolhida$Valores, na.rm=TRUE)-min(regiaoEscolhida$Valores, na.rm=TRUE))
  coloreMapa(regiaoEscolhida, tituloEscolhido, paletaEscolhida, bordaEscolhida, hachuraEscolhida, densidadeHachura, anguloHachura)
  }
