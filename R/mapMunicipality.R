mapMunicipality <- function(matrizInput,titulo="",regiao="Brasil", paleta="YlOrRd", borda="black", hachura="blue", densidade_hachura=30, angulo_hachura=45)
  {
  if(borda==FALSE)
  {
     borda="transparent"
  }
  if(regiao=="Brasil")
  {
    regiao="Municipios"
  }
  require("maptools")
  require("RColorBrewer")
  require("plotrix")
  library("maptools")
  library("RColorBrewer")
  library("plotrix")
  
  tituloEscolhido = titulo
  regiaoEscolhida = regiao
  paletaEscolhida = paleta
  bordaEscolhida = borda 
  hachuraEscolhida = hachura
  densidadeHachura = densidade_hachura
  anguloHachura = angulo_hachura
  coresDisponiveis = rownames(brewer.pal.info)
  #Se a paleta escolhida não for uma paleta válida, retorna uma mensagem de erro e as opções possíveis
  if(!is.element(paletaEscolhida, coresDisponiveis))
  {
    originalPar <- par(no.readonly=TRUE)
    layout(matrix(1:1,nrow=1),widths=c(0.9,0.1))
    display.brewer.all()
    stop("Houve um erro pois a paleta escolhida para o mapa deve ser uma paleta do pacote RColorBrewer. As opções disponiveis são as que estão no plot.")
    par(originalPar)
  }
  
  Municipios  <- readShapePoly(fn='/RProjetos/Mapas/LM_MUNICIPIO_2007.shp')
  Municipios$UFChar = NULL
  Municipios$UFChar = as.character(Municipios$UF)
  
  #Definição de regiões
  CentroOeste = subset(Municipios, UFChar=="MT" | UFChar=="MS" | UFChar=="GO" | UFChar=="DT")
  Sul = subset(Municipios, UFChar=="RS" | UFChar=="SC" | UFChar=="PR" )
  Sudeste = subset(Municipios, UFChar=="SP" | UFChar=="RJ" | UFChar=="ES" | UFChar=="MG")
  Nordeste = subset(Municipios, UFChar=="MA" | UFChar=="PI" | UFChar=="CE" | UFChar=="RN" | UFChar=="PE" | UFChar=="PB" | UFChar=="SE" | UFChar=="AL" | UFChar=="BA")
  Norte = subset(Municipios, UFChar=="AM" | UFChar=="RR" | UFChar=="AP" | UFChar=="PA" | UFChar=="TO" | UFChar=="RO" | UFChar=="AC")
  
  #Definição de Estados
  MT = subset(Municipios, UFChar=="MT")
  MS = subset(Municipios, UFChar=="MS")
  GO = subset(Municipios, UFChar=="GO")
  DT = subset(Municipios, UFChar=="DT")
  RS = subset(Municipios, UFChar=="RS")
  SC = subset(Municipios, UFChar=="SC")
  PR = subset(Municipios, UFChar=="PR")
  SP = subset(Municipios, UFChar=="SP")
  RJ = subset(Municipios, UFChar=="RJ")
  ES = subset(Municipios, UFChar=="ES")
  MG = subset(Municipios, UFChar=="MG")
  MA = subset(Municipios, UFChar=="MA")
  PI = subset(Municipios, UFChar=="PI")
  CE = subset(Municipios, UFChar=="CE")
  RN = subset(Municipios, UFChar=="RN")
  PE = subset(Municipios, UFChar=="PE")
  PB = subset(Municipios, UFChar=="PB")
  SE = subset(Municipios, UFChar=="SE")
  AL = subset(Municipios, UFChar=="AL")
  BA = subset(Municipios, UFChar=="BA")
  AM = subset(Municipios, UFChar=="AM")
  RR = subset(Municipios, UFChar=="RR")
  PA = subset(Municipios, UFChar=="PA")
  TO = subset(Municipios, UFChar=="TO")
  RO = subset(Municipios, UFChar=="RO")
  AC = subset(Municipios, UFChar=="AC")
  
  regiaoEscolhida = get(regiaoEscolhida)
  regiaoEscolhida$geocodigo= as.character(regiaoEscolhida$geocodigo)
  #Encontra o valor, procurando pelo codigo
  regiaoEscolhida$Valores = as.numeric(matrizInput[match(regiaoEscolhida$geocodigo, matrizInput[,1]),2])
  #Mapeia de 0 a 100
  regiaoEscolhida$ValoresTf = 100*(regiaoEscolhida$Valores - min(regiaoEscolhida$Valores, na.rm=TRUE))/(max(regiaoEscolhida$Valores,na.rm=TRUE)-min(regiaoEscolhida$Valores,na.rm=TRUE))
  coloreMapa(regiaoEscolhida, tituloEscolhido, paletaEscolhida, bordaEscolhida, hachuraEscolhida, densidadeHachura, anguloHachura)
  }
  
