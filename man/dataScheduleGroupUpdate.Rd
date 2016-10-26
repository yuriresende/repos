\name{dataScheduleGroupUpdate}
\alias{dataScheduleGroupUpdate}
\title{Agenda atualização do banco de dados de grupo para o dia de hora desejada pelo usuário.}
\usage{
dataScheduleGroupUpdate=(when="sexta 20 00")
}


\arguments{
  \item{when}{Dia e hora do agendamento. Devem ser informados no formato "dia hora minuto". Por exemplo "Sexta 19 30"}
}

\value{

- Esta função não retorna nada. A base atualizada é automaticamente salva na pasta /RProjetos/Dados/painel-atualizado/base-atualizada.RData

}

\details{

- O tempo aproximado de atualização da base para todos os grupos é de 7 horas. 
  
} 
  
  
}
  
  \author{
  Coded by Gabriel Vasconcelos, last updated on 01-11-2016.
  }
