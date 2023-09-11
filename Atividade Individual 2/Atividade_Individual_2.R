#########################################################
#                ATIVIDADE INDIVIDUAL 2                 #          
#########################################################

          # Instruções para usar o programa #           

# Esse script funciona como uma mistura entre console e
# código, ou seja, o usuário/programador precisa apertar
# Ctrl + Shift + S e informar os dados no console do RStudio.
# Além disso, os tipos de variáveis do vetor de dados informado
# devem ser todas do mesmo tipo.

#########################################################
#                       QUESTÃO 1                       #          
#########################################################

                # Limpeza do ambiente #

rm(list=ls())

             # Captura de dados e operações #

vetor_dados <- c()
tam_vetor <- as.numeric(readline("Informe o tamanho do vetor de dados: "))
for(i in 1:tam_vetor){
  cat("Informe o", i, "º dado: ")
  vetor_dados[i] <- readline()
}
vet_as_int <- suppressWarnings(as.integer(vetor_dados))
vet_as_double <- suppressWarnings(as.double(vetor_dados))
is_not_a_number <- is.na(vet_as_int)
is_quantitativa_discreta <- TRUE
is_not_a_number_aux <- TRUE
tipo_variavel <- ""
for(i in 1:length(is_not_a_number)){
  if(is_not_a_number[i] == TRUE){
    is_not_a_number_aux <- TRUE
  } else {
    is_not_a_number_aux <- FALSE
  }
}
while(1){
  if(is_not_a_number_aux == TRUE){
    print("A variável é qualitativa!")
    tipo_variavel <- "Qualitativa"
    break
  } else {
    for(i in 1:length(vetor_dados)){
      if(vet_as_int[i] != vet_as_double[i]){
        is_quantitativa_discreta <- FALSE
      } else {
        is_quantitativa_discreta <- TRUE
      }
    }
    if(is_quantitativa_discreta == TRUE){
      print("A variável é quantitativa discreta!")
      tipo_variavel <- "Quantitativa discreta"
    } else {
      print("A variável é quantitativa contínua!")
      tipo_variavel <- "Quantitativa contínua"
    }
    break
  }
}
if(tipo_variavel == 'Qualitativa'){
  valores_qualitativo <- table(vetor_dados)
  grafico_qualitativo <- table(vetor_dados)
  valores_qualitativo <- data.frame(valores_qualitativo)
  variavel_qualitativo <- valores_qualitativo$vetor_dados
  frequencia_qualitativo <- valores_qualitativo$frequencia_qualitativo
  barplot(grafico_qualitativo, main=c("Gráfico de valores e frequência"), ylab=c("Frequência"), xlab=c("Variáveis"), cex.axis = 0.8, ylim =c(0,10))
  grafico_qualitativo<-as.data.frame(grafico_qualitativo)
  frequencia <- grafico_qualitativo$Freq
  Frequencia_relativa <- prop.table(frequencia)
  tabela_qualitativo <- data.frame(variavel_qualitativo, frequencia, Frequencia_relativa)
  print(tabela_qualitativo)
  moda <- as.character(tabela_qualitativo$variavel_qualitativo[which( (tabela_qualitativo$frequencia )== max(tabela_qualitativo$frequencia) )])
  cat("A moda do vetor é", moda)
  } else if (tipo_variavel == 'Quantitativa discreta'){
  histograma_quantitativo_discreto <- hist(vet_as_int)
  xi_discreto <- histograma_quantitativo_discreto$mids
  fi_discreto <- histograma_quantitativo_discreto$counts
  fri_discreto <- fi_discreto / (length(vet_as_int))
  fpi_discreto <- fri_discreto * 100
  intervalos_discretos <- histograma_quantitativo_discreto$breaks
  classes_discretos <- c()
  for(i in 1:length(xi_discreto)){
    classes_discretos[i]<-sprintf("%d-%d",intervalos_discretos[i],intervalos_discretos[i+1])
  }
  valores_quantitativo_discreto <- table(vet_as_int)
  grafico_quantitativo_discreto <- table(vet_as_int)
  valores_quantitativo_discreto <- data.frame(valores_quantitativo_discreto)
  variavel_quantitativo_discreto <- valores_quantitativo_discreto$vet_as_int
  frequencia_quantitativo_discreto <- valores_quantitativo_discreto$frequencia_quantitativo_discreto
  barplot(grafico_quantitativo_discreto, main=c("Gráfico de valores e frequência"), ylab=c("Frequência"), xlab=c("Variáveis"), cex.axis = 0.8, ylim =c(0,10))
  fpi_discreto[length(fpi_discreto) + 1] <- sum(fpi_discreto)
  fri_discreto[length(fri_discreto) + 1] <- sum(fri_discreto)
  xi_discreto[length(xi_discreto) + 1] <- '-'
  fi_discreto[length(fi_discreto) + 1] <- sum(fi_discreto)
  classes_discretos[length(classes_discretos) + 1] <- '-'
  tabela_quantitativo_discreto <- data.frame(classes_discretos, xi_discreto, fi_discreto, fri_discreto, fpi_discreto)
  print(tabela_quantitativo_discreto)
  cat("A média do vetor é", mean(vet_as_int),"\nA mediana do vetor é", median(vet_as_int),"\nO desvio-padrão do vetor é", sd(vet_as_int))
} else if(tipo_variavel == 'Quantitativa contínua'){
  histograma <- hist(vet_as_double)
  xi <- histograma$mids
  fi <- histograma$counts
  fri <- fi/(length(vet_as_double))
  fpi <- fri*100
  intervalos <- histograma$breaks
  classes_intervalo <- c()
  for(i in 1:length(xi)){
    classes_intervalo[i] <- sprintf("%d-%d",intervalos[i],intervalos[i+1])
  }
  hist(vet_as_double, main = "Histograma", col="red", ylab = 'Frequência', xlab = 'Intervalos')
  fpi[length(fpi)+1] <- sum(fpi)
  fri[length(fri)+1] <- sum(fri)
  xi[length(xi)+1] <- '-'
  classes_intervalo[length(classes_intervalo)+1] <- '-'
  fi[length(fi)+1] <- sum(fi)
  tabela_distribuicao_frequencia <- data.frame(classes_intervalo, xi, fi, fri, fpi)
  print(tabela_distribuicao_frequencia)
  cat("A média do vetor é", mean(vet_as_double),"\nA mediana do vetor é", median(vet_as_double),"\nO desvio-padrão do vetor é", sd(vet_as_double))
}


