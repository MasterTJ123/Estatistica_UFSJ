#########################################################
#                ATIVIDADE INDIVIDUAL 1                 #          
#########################################################

          # Instruções para usar o programa #           

# Esse script funciona como uma mistura entre console e
# código, ou seja, o usuário/programador precisa apertar
# Ctrl + Shift + S e informar os dados no console do RStudio

#########################################################
#                       QUESTÃO 1                       #          
#########################################################

                # Limpeza do ambiente #

rm(list=ls())

             # Captura de dados e operações #

tam_pop <- as.numeric(readline("Informe o tamanho da população: "))
tam_amo <- as.numeric(readline("Informe o tamanho da amostra: "))

aux_amo <- 0

while(aux_amo == 0){
  pla_amo <- readline("Informe o plano de amostragem: ")
  if(pla_amo == 'aleatória simples'){
    res <- sample(1:(tam_pop),(tam_amo))
    print(res)
    aux_amo <- 1
  } else if(pla_amo == 'sistemática'){
    k <- floor(tam_pop/tam_amo)
    ual <- sample(1:k,1)
    res <- ual + (0:(tam_amo-1))*k
    print(res)
    aux_amo <- 1
  } else if(pla_amo == 'estratificada'){
    qua_est <- as.numeric(readline("Informe a quantidade de estratos: "))
    tam_est <- c()
    N <- c()
    for(i in 1:qua_est){
      cat("Informe o tamanho do", i, "º estrato: ")
      tam_est[i] <- as.numeric(readline())
      N[i] <- round((tam_amo*tam_est[i])/(tam_pop))
    }
    aux_est <- 1
    j <- 0
    res <- c()
    for(i in 1:qua_est){
      res <- sample(aux_est:(j+tam_est[i]),size=N[i])
      cat("Amostras do", i, "º estrato:", res, "\n")
      aux_est <- aux_est + tam_est[i]
      j <- j + tam_est[i]
    }
    aux_amo <- 1
  } else {
    print("Erro! O plano de amostragem informado não é válido!")
  }
}
