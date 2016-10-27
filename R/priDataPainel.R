priDataPainel<-function (Base, d_lojas, inicio.experimento, inicio.tratamento, final.experimento,
                         familia, preco ,delta_p, criterio_corte, grupo.controle, grupo.tratamento) {
  
  require(plm)
  
  # função auxiliar para manipulação dos dados em lista 
  ':=' <- function(lhs, rhs) {
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
      lhs <- lhs[-1]
    if (length(lhs) == 1) {
      do.call(`=`, list(lhs[[1]], rhs), envir=frame)
      return(invisible(NULL)) 
    }
    if (is.function(rhs) || is(rhs, 'formula'))
      rhs <- list(rhs)
    if (length(lhs) > length(rhs))
      rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
    for (i in 1:length(lhs))
      do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
    return(invisible(NULL)) 
  }
  
  # função que faz subset nos dias do pré e pós tratamento, insere D1, D2, D3, preço SAP e remove lojas inexistentes
  organiza_base<-function(d_lojas,datas.in,datas.depois,preco,delta_p,grupo.controle,grupo.tratamento){
    
    ###############################################################################
    # Filtrando a Base para analisar dados somente do período de experimento
    ###############################################################################
    
    Base = subset(Base,is.element(as.Date(Base$data),datas.in))
    
    #######################################
    # Retira lojas fechadas pelo nosso critério
    #######################################
    
    #Base = subset(Base,LOJA.FECHADA==1)
    
    #######################################
    # Dummy de grupo 
    #######################################
    
    aux=matrix(NA,nrow(Base),1)
    
    for (i in 1:length(d_lojas[,1])){
      
      index = which(as.character(Base$loja)==as.character(d_lojas[i,which(toupper(colnames(d_lojas))=='LOJA')]))
      
      if (as.character(d_lojas[i,which(toupper(colnames(d_lojas))=='SUBGRUPO')])%in%grupo.tratamento){
        aux[index,] = 1
      }else{ 
        if (as.character(d_lojas[i,which(toupper(colnames(d_lojas))=='SUBGRUPO')])%in%grupo.controle){
          aux[index,] = 0
        }else{
          aux[index,] = NA
        }
      }
    }
    
    Base.series = cbind(Base,aux)
    Base = Base.series
    colnames(Base)[ncol(Base)]="D1"
    
    ##########################################
    # Dummy de estar no período de tratamento
    ##########################################
    
    aux2=matrix(NA,nrow(Base),1)
    
    aux2[which(is.element(Base$data,as.character(datas.depois))),]=1
    aux2[is.na(aux2)]=0
    
    Base.series = data.frame(Base,aux2)
    colnames(Base.series)[ncol(Base.series)]="D2"
    
    Base.series$D3 = Base.series$D1*Base.series$D2
    Base = Base.series
    
    ##########################################
    # Introduz preço SAP
    ##########################################
    
    Base$P_SAP = Base$D3*(preco+delta_p)
    Base$P_SAP[which(Base$P_SAP==0)] = preco
    
    ##########################################
    # Retirar dados com erros
    ##########################################
    
    retira.lojas.n.inauguradas = subset(Base,!is.na(Base$D1))
    retira.dias.sem.PSAP = subset(retira.lojas.n.inauguradas,!is.na(retira.lojas.n.inauguradas$P_SAP))
    Base = retira.dias.sem.PSAP
    
    return(Base)
  }
  
  # remove lojas que praticaram preço médio PRÉ tratamento INFERIOR ao esperado. Podemos generalizar futuramente para preços superiores e erros NO tratamento
  filtra_base<-function(PeQ,criterio_corte,familia,datas.depois,datas.in){
    
    datas.antes <- datas.in[which(datas.in!=datas.depois)]
    
    Base_aux<- subset(PeQ,is.element(as.Date(PeQ$data),datas.antes))
    
    col_vda_brut<-which(colnames(Base_aux)==paste('VDA_BRUT_SKU',familia,sep=''))
    col_vda_qtd<-which(colnames(Base_aux)==paste('VDA_QTD_SKU',familia,sep=''))
    
    Base_agregada <- aggregate(cbind(Base_aux[,col_vda_brut],Base_aux[,col_vda_qtd]) ~ loja, data=Base_aux, FUN = "sum")
    
    Base_agregada$pmedio<-Base_agregada$V1/Base_agregada$V2
    
    excluir<-Base_agregada$loja[which(Base_agregada$pmedio<criterio_corte)]
    
    Base_filtrada <- PeQ[which(PeQ$loja%in%excluir==FALSE),]
    
    return(list(Base_filtrada,excluir))
  }
  
  datas.in = seq(as.Date(inicio.experimento),as.Date(final.experimento),by="1 d")
  datas.depois = seq(as.Date(inicio.tratamento),as.Date(final.experimento),by='1 d')
  
  n_dias<-length(datas.depois)
  
  Base<-organiza_base(d_lojas,datas.in,datas.depois,preco,delta_p,grupo.controle,grupo.tratamento)
  
  # Formato de painel
  col_p_medio<-which(colnames(Base)==paste('p.medio',familia,sep='.'))
  col_vda_qtd<-which(colnames(Base)==paste('VDA_QTD_SKU',familia,sep=''))
  
  PeQ = Base
  c(PeQ,excluidos) := filtra_base(PeQ,criterio_corte,familia,datas.depois,datas.in)
  
  PeQ[is.na(PeQ)] = 0
  PeQ[which(PeQ[,col_p_medio]==0),col_p_medio] <- PeQ[which(PeQ[,col_p_medio]==0),'P_SAP']
  
  data.painel <- plm.data(PeQ,c("loja","data"))
  
  return(list('data.painel' = data.painel, 'dias.tratamento' = n_dias,'Lojas excluídas' = as.character(excluidos)))
}
