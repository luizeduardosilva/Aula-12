                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoiníveis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (país) igual a "Brazil", selecionando as colunas cujas variáveis são "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÂMBIO)

colnames(br) <-  c("PIB","Emprego","Câmbio")   #Renomeia as colunas para PIB, Trabalho e Câmbio

                                        #Separando as variáveis
PIB <- br$PIB[45:62]                    #Cria o vetor para variável PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para variável EMPREGO
CAMBIO <- br$Câmbio[45:62]              #Cria o vetor para variável CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 até 2011 


                                    #Analise para o EMPREGO

plot(EMPREGO, type = "l")                            #Cria gráfico 
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(emprego, main="Pessoa Empregadas no Brasil", ylab="Qte de Pessoas Empregadas-milhões", xlab="Ano")                                      #Cria gráfico da Série Temporal

acf(emprego)                                          #Função de Autocorrelação
pacf(emprego)                                         ##Função de Autocorrelação Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regressão linear simples 
reglinEMP                                             #Exibe os resultados da regressão linear
summary(reglinEMP)
plot(emprego)                                         #Gráfcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regressão linear estimada

                                    #Analise para o PIB

plot(PIB, type = "l")                            #Cria gráfico 
pib <- ts(PIB, start = 1994, frequency = 1)  #Define como Série Temporal
plot(emprego, main="PIB - Brasil", ylab="PIB", xlab="Ano")    #Cria gráfico da Série Temporal

acf(pib)                                          #Função de Autocorrelação
pacf(pib)                                         ##Função de Autocorrelação Parcial
reglinPib <- lm(PIB ~ Anos)                       #Regressão linear simples 
reglinPib                                             #Exibe os resultados da regressão linear
summary(reglinPib)
plot(pib)                                         #Gráfcio dos dados
abline(reglinPib, col="Blue")                         #Insere a linha de regressão linear estimada

                                #Analise para o CAMBIO

plot(CAMBIO, type = "l")                            #Cria gráfico 
cambio <- ts(CAMBIO, start = 1994, frequency = 1)  #Define como Série Temporal
plot(cambio, main="Câmbio - Brasil", ylab="Câmbio", xlab="Ano")   #Cria gráfico da Série Temporal

acf(cambio)                                          #Função de Autocorrelação
pacf(cambio)                                         ##Função de Autocorrelação Parcial
reglinCAM <- lm(CAMBIO ~ Anos)                       #Regressão linear simples 
reglinCAM                                             #Exibe os resultados da regressão linear
summary(reglinCAM)
plot(cambio)                                         #Gráfcio dos dados
abline(reglinCAM, col="Blue")                         #Insere a linha de regressão linear estimada

#Removendo Tendência

residuosEMP <- reglinEMP$residuals                    #Salva os resíduos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosEMP,type="l")                            #Gráfico dos resíduos
abline(reglinEMPres, col="Blue")                      #Insere a linha de regressão linear dos resíduos

residuosPib <- reglinPib$residuals                    
reglinPibres <- lm(residuosPib ~ Anos)                
plot(residuosPib,type="l")                            
abline(reglinPibres, col="Red")                      

residuosCAM <- reglinCAM$residuals                    
reglinCAMres <- lm(residuosCAM ~ Anos)                
plot(residuosCAM,type="l")                            
abline(reglinCAMres, col="Green")                      

#Removendo Tendência por meio da diferença

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr´pafico somente para a serie da diferença

pdpib <- diff(PIB)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(PIB[2:18],pdpib))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdpib, type="l")                                   #Cria gr´pafico somente para a serie da diferença

pdcambio <- diff(CAMBIO)                                #Calcula a primeira diferença da série de dados
diferenca1 <- (data.frame(CAMBIO[2:18],pdcambio))       #Exibe a tabela da série original coma diferença <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdcambio, type="l")                                   #Cria gr´pafico somente para a serie da diferença

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferença
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferença
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

pdpib1 <- diff(pib)                                            #Calculando-se a primeira diferença
TesteDF_PIB1_trend <- ur.df(pdpib1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB1_trend) 

pdpib2 <- diff(diff(pib))                                      #Calculando-se a segunda diferença
TesteDF_PIB2_trend <- ur.df(pdpib2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB2_trend)

pdcambio1 <- diff(cambio)                                            #Calculando-se a primeira diferença
TesteDF_Cambio1_trend <- ur.df(pdcambio1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Cambio1_trend) 

pdcambio2 <- diff(diff(cambio))                                      #Calculando-se a segunda diferença
TesteDF_Cambio2_trend <- ur.df(pdcambio2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Cambio2_trend)

#Estimando a série temporal - Emprego

arima120 <- arima(emprego, c(1,2,0))
arima121 <- arima(emprego, c(1,2,1))
arima122 <- arima(emprego, c(1,2,2))
arima123 <- arima(emprego, c(1,2,3))

arima220 <- arima(emprego, c(2,2,0))
arima221 <- arima(emprego, c(2,2,1))
arima222 <- arima(emprego, c(2,2,2))
arima223 <- arima(emprego, c(2,2,3))
#ARMA
arma10 <- arima(emprego, c(1,0,0))
arma11 <- arima(emprego, c(1,0,1))
arma12 <- arima(emprego, c(1,0,2))
arma13 <- arima(emprego, c(1,0,3))

arma20 <- arima(emprego, c(2,0,0))
arma21 <- arima(emprego, c(2,0,1))
arma22 <- arima(emprego, c(2,0,2))
arma23 <- arima(emprego, c(2,0,3))
#MA
arima021 <- arima(emprego, c(2,2,0))
arima022 <- arima(emprego, c(2,2,0))
arima023 <- arima(emprego, c(2,2,0))
#AR
arima120 <- arima(emprego, c(1,2,0))
arima220 <- arima(emprego, c(2,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,rima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima0120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima0120")) 
Resultados <- data.frame(Modelo,AIC,BIC)

#Estimando a série temporal - PIB

arima120 <- arima(pib, c(1,2,0))
arima121 <- arima(pib, c(1,2,1))
arima122 <- arima(pib, c(1,2,2))
arima123 <- arima(pib, c(1,2,3))

arima220 <- arima(pib, c(2,2,0))
arima221 <- arima(pib, c(2,2,1))
arima222 <- arima(pib, c(2,2,2))
arima223 <- arima(pib, c(2,2,3))
#ARMA
arma10 <- arima(pib, c(1,0,0))
arma11 <- arima(pib, c(1,0,1))
arma12 <- arima(pib, c(1,0,2))
arma13 <- arima(pib, c(1,0,3))

arma20 <- arima(pib, c(2,0,0))
arma21 <- arima(pib, c(2,0,1))
arma22 <- arima(pib, c(2,0,2))
arma23 <- arima(pib, c(2,0,3))
#MA
arima021 <- arima(pib, c(2,2,0))
arima022 <- arima(pib, c(2,2,0))
arima023 <- arima(pib, c(2,2,0))
#AR
arima120 <- arima(pib, c(1,2,0))
arima220 <- arima(pib, c(2,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,rima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima0120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima0120")) 
Resultados <- data.frame(Modelo,AIC,BIC)

#Estimando a série temporal - CAMBIO

arima120 <- arima(cambio, c(1,2,0))
arima121 <- arima(cambio, c(1,2,1))
arima122 <- arima(cambio, c(1,2,2))
arima123 <- arima(cambio, c(1,2,3))

arima220 <- arima(cambio, c(2,2,0))
arima221 <- arima(cambio, c(2,2,1))
arima222 <- arima(cambio, c(2,2,2))
arima223 <- arima(cambio, c(2,2,3))
#ARMA
arma10 <- arima(cambio, c(1,0,0))
arma11 <- arima(cambio, c(1,0,1))
arma12 <- arima(cambio, c(1,0,2))
arma13 <- arima(cambio, c(1,0,3))

arma20 <- arima(cambio, c(2,0,0))
arma21 <- arima(cambio, c(2,0,1))
arma22 <- arima(cambio, c(2,0,2))
arma23 <- arima(cambio, c(2,0,3))
#MA
arima021 <- arima(cambio, c(2,2,0))
arima022 <- arima(cambio, c(2,2,0))
arima023 <- arima(cambio, c(2,2,0))
#AR
arima120 <- arima(cambio, c(1,2,0))
arima220 <- arima(cambio, c(2,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,rima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima0120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima0120")) 
Resultados <- data.frame(Modelo,AIC,BIC)
