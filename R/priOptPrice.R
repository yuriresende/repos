priOptPrice <- function (familia, coef_agregado, Q, preco, range_preco, imposto, cmv) {

  x <- range_preco
  b <- coef_agregado
  Q1 <- Q
  
  quantidade <- Q1 + b*(x - preco)

  # Como a reta está linear, precisamos fazer esse ajuste para não ter demanda negativa
  quantidade[which(quantidade < 0)] <- 0

  receita <- quantidade*x

  lucro <- quantidade*(x*(1 - imposto) - cmv)

  margem <- (x*(1 - imposto) - cmv) / (x*(1 - imposto))

  # Elasticidade calculada somente para o ponto atual 
  # (o coeficiente está multiplicado pelo número de dias do tratamento, por isso usamos Q1 no denominador)
  elasticidade <- b*preco/Q

  pmaxreceita = (Q1 - b*preco)/(-2*b)

  # Não está considerado o imposto no pmaxlucro
  pmaxlucro = (Q1 - b*preco - b*cmv)/(-2*b)

  plot(range_preco, receita, ylab="Receita Bruta", xlab = 'Preços simulados', type = 'l')
  plot(range_preco, lucro, ylab="Lucro Bruto", xlab = 'Preços simulados', type = 'l')
  plot(range_preco, margem, ylab="Margem", xlab = 'Preços simulados', type = 'l')

  return(list('Preço max lucro' = pmaxlucro, 'Preço max receita bruta' = pmaxreceita, 'Receita bruta' = receita, 
              'Lucro' = lucro, 'Margem' = margem, 'Elasticidade' = elasticidade))
}
