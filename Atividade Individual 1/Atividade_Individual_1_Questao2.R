#########################################################
#                ATIVIDADE INDIVIDUAL 1                 #          
#########################################################

          # Instruções para usar o programa #           

# Esse script funciona como uma mistura entre console e
# código, ou seja, o usuário/programador precisa apertar
# Ctrl + Shift + S e informar os dados no console do RStudio.
# Além disso, caso as amostras sejam fracionárias, é preciso
# separar as casas decimais por ponto e não vírgula. Por fim,
# os valores das variáveis e vetores são listados no "Environment".

#########################################################
#                       QUESTÃO 2                       #          
#########################################################

                # Limpeza do ambiente #

rm(list=ls())

             # Captura de dados e operações #

indice <- 1
val_aux <- 0
val_amo <- c()
cat("(Digite ok para encerrar a coleta de amostras)\n")
while(1){
  val_aux <- (readline(cat("Informe o valor da", indice, "º amostra: ")))
  if(val_aux == 'ok'){
    break
  }
  val_amo[indice] <- as.numeric(val_aux)
  indice <- indice + 1
}
num_rea <- 0
num_rea <- as.numeric(readline("Informe o número de vezes para reamostragem: "))
vet_aux <- c()
media <- c()
mediana <- c()
des_pad <- c()
for(i in 1:num_rea){
  vet_aux <- sample(val_amo, size=(length(val_amo)), replace=TRUE)
  media[i] <- mean(vet_aux)
  mediana[i] <- median(vet_aux)
  des_pad[i] <- sd(vet_aux)
}
media_media <- mean(media)
media_mediana <- mean(mediana)
media_desvio_padrao <- mean(des_pad)
cat("A média das", num_rea, "médias é:", media_media, "\n")
cat("A média das", num_rea, "medianas é:", media_mediana, "\n")
cat("A média dos", num_rea, "desvios padrões é:", media_desvio_padrao, "\n")
