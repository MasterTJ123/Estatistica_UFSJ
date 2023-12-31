#########################################################
#                ATIVIDADE INDIVIDUAL 1                 #          
#########################################################

          # Instru��es para usar o programa #           

# Esse script funciona como uma mistura entre console e
# c�digo, ou seja, o usu�rio/programador precisa apertar
# Ctrl + Shift + S e informar os dados no console do RStudio.
# Al�m disso, caso as amostras sejam fracion�rias, � preciso
# separar as casas decimais por ponto e n�o v�rgula. Por fim,
# os valores das vari�veis e vetores s�o listados no "Environment".

#########################################################
#                       QUEST�O 2                       #          
#########################################################

                # Limpeza do ambiente #

rm(list=ls())

             # Captura de dados e opera��es #

indice <- 1
val_aux <- 0
val_amo <- c()
cat("(Digite ok para encerrar a coleta de amostras)\n")
while(1){
  val_aux <- (readline(cat("Informe o valor da", indice, "� amostra: ")))
  if(val_aux == 'ok'){
    break
  }
  val_amo[indice] <- as.numeric(val_aux)
  indice <- indice + 1
}
num_rea <- 0
num_rea <- as.numeric(readline("Informe o n�mero de vezes para reamostragem: "))
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
cat("A m�dia das", num_rea, "m�dias �:", media_media, "\n")
cat("A m�dia das", num_rea, "medianas �:", media_mediana, "\n")
cat("A m�dia dos", num_rea, "desvios padr�es �:", media_desvio_padrao, "\n")
