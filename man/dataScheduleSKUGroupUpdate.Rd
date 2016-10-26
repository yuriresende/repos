\name{dataScheduleSKUGroupUpdate}
\alias{dataScheduleSKUGroupUpdate}
\title{Agenda atualização do banco de dados de grupo para o dia de hora desejada pelo usuário.}
\usage{
dataScheduleSKUGroupUpdate=(when="sexta 20 00")
}


\arguments{
  \item{when}{Dia e hora do agendamento. Devem ser informados no formato "dia hora minuto". Por exemplo "Sexta 19 30"}
}

\value{

- Esta função não retorna nada. A base atualizada é automaticamente salva na pasta /RProjetos/Dados/painel-atualizado/base-atualizada.RData

}

\details{

- IMPORTANTE: Esta função foi feita para ser rodada preferencialmente no terminal do linux via putty. Ela não deve ser utilizada no RStudio pois impedirá o usuário de realizar quaisquer outras tarefas. 
  
- O tempo aproximado de atualização da base para todos os grupos é de 7 horas. 
  
} 
  
  
}
  
  \author{
  Coded by Gabriel Vasconcelos, last updated on 01-11-2016.
  }
