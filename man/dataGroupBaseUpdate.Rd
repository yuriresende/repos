\name{dataGroupBaseUpdate}
\alias{dataGroupBaseUpdate}
\title{Atualiza a base de dados dos grupos DLAB de SKUS do departamento D040.}
\usage{
dataGroupBaseUpdate(groups=NULL)
}


\arguments{
  \item{groups}{Grupos a serem baixados. A função permite que o usuário escolha os grupos. Entretanto, se o argumento for deixado em branco a função baixa os dados de todos os grupos de acordo com a tabela disponível na pasta /RProjetos/Dados/gruposD040.csv}
}

\value{

- Esta função não retorna nada. A base atualizada é automaticamente salva na pasta /RProjetos/Dados/painel-atualizado/base-atualizada.RData

- A base antiga é movida para /RProjetos/Dados/paineis-antigos/base-antiga.RData

}

}
  
  \author{
  Coded by Henrique Helfer Hoeltgebaum and Gabriel Vasconcelos, last updated on 01-11-2016.
  }
