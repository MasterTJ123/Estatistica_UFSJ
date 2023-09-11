#########################################################
#                ATIVIDADE INDIVIDUAL 3                 #          
#########################################################

# Instruções para usar o programa #           

# Para rodar o código, basta apertar Ctrl + Shift + S

#########################################################
#                       QUESTÃO 1                       #          
#########################################################

# Limpeza do ambiente #

rm(list=ls())

# Amostra 1 #

quantidade_amostras <- 20
media_amostra1 <- 15
desvio_padrao_amostra1 <- 8
amostra1 <- rnorm(quantidade_amostras, media_amostra1, desvio_padrao_amostra1) 

# Amostra 2 #

media_amostra2 <- 5
desvio_padrao_amostra2 <- 3
amostra2 <- rnorm(quantidade_amostras, media_amostra1, desvio_padrao_amostra2)

# Razão das amostras #

nova_amostra <- amostra1/amostra2
print("Razão das amostras 1 e 2: ")
print(nova_amostra)
cat("\n")

# Jackknife #

dp_nova_amostra = sd(nova_amostra)

dp_jackknife <- c()

for(i in 1:quantidade_amostras){
    dp_jackknife[i] <- sd(nova_amostra[-i])
}

print("Estimativas de desvio-padrão: ")
print(dp_jackknife)
cat("\n")

media_dp_jackknife = mean(dp_jackknife)

vies = (quantidade_amostras * dp_nova_amostra) - ((quantidade_amostras - 1) * media_dp_jackknife)

print("O vies é: ")
print(vies)