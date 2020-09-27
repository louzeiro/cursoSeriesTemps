library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

# arquivo do hotel, sem cabeçalho, tem os anos e a taxa de ocupação
dados = read.csv("Documentos/cursosExtras/udemy/cursoSeriesTemps/hotel.csv", header = F)
print(dados)
class(dados)
# transformando o DF em TS
ocupacao =  ts(dados,
               start = c(2003,1),
               end = c(2017,12), 
               frequency = 12) # freq mensal
class(ocupacao)
summary(ocupacao) #observa-se que o hotel possue uma baixa ocupação, pois a média e a media apontam 30%

############################################
###########3## exploração dos dados
autoplot(ocupacao) # possue uma sazonalidade e uma possível tendencia
hist(ocupacao) # observa-se que a maior freq presente é de 30%, poucos casos de 50%
boxplot(ocupacao)# observa-se que ocupação acima de 65% são incomuns, sendo consideradas outliers
## decomposição da série
ocupacaoDec = decompose(ocupacao)
autoplot(ocupacaoDec)
## Observa-se que:
## * sem a influencia da tendencia, há um forte padrão de sazonalidade
## * embora sofrendo muita variação, a tendência do ultimo ano aponta um comportamento de crescimento 

## explorando a tendencia
autoplot(ocupacaoDec$trend) # dando um zoom na tendencia, observa-se que anteriormente a ocupação variava bastante,
autoplot(window(ocupacaoDec$trend, start = c(2015,9))) #mas que no ultimo ano houve uma forte tendência de crescimento

## explorando a sazonalidade
ggseasonplot(ocupacao) #observa-se que os meses que historicamente possuem menor ocupação são os meses de dez e jan
                       # e os meses que mais possuem ocupação é mai/jun o pico é jul, decaindo em set.

ggseasonplot(window(ocupacao, start=c(2016,1))) #analisando apenas os ultimos anos 2016 a 2017 observa-se que conforme 
# já dito, a sazonalidade se manteve e o ultimo ano foi o ano com maior ocupação

############## Previsões
### Testes
### inicialmente será feito o teste de estacionaridade para verificar se eles precisam de um processo de diferenciação
testeEstacionaridade  = ur.kpss(ocupacao)
print(testeEstacionaridade) # saida ===> The value of the test statistic is: 0.0569, ou seja um pouco acima de 0.05, 
# nesse caso ainda não é conclusivo o teste. Visualmente os dados apontam uma estacionaridade, mas de todos modo será 
# feito o teste de diferenciação.
ndiffs(ocupacao) # saida ===>  0, desse modo, pode-se assumir que, com base nos resultados dos testes, os dados são estacionários
# Agora será avaliado a autocorrelação entre os dados, para isso sera gerado o diagrama de correlação e autocorrelação
tsdisplay(ocupacao) # saida ==> ACF, o diagrama de autocorrelação, aponta forte sazonalidade e o PACF, diagrama parcial de autocorrelação,
#aponta diversos pontos de autorrelação

### Modelos
# iniciando com o auto.arima, com uma busca profunda, para se obter um modelo gerado automaticamente

#======> Best model: ARIMA(1,0,1)(0,1,1)[12], ou seja para a parte sazonal o m  elhor modelo é o (1,0,1)
#para  a parte não sazonal o modelo (0,1,1) e a frequencia [12]
modelo = auto.arima(ocupacao,
           trace = T, # habilitando o display para acompanhar
           stepwise = F, # permitindo uma busca mais profunda
           approximation = F)
print(modelo) # exibindo os parametros do modelo

## avaliando os resíduos
checkresiduals(modelo) # o resultado do teste de hipotese, p-value = 0.2315, aponta que não há correlação entre os resíduos
# assim, esses residuais são considerados ruídos branco. Com o diagrama de autocorrelação observa-se que apenas duas leg passaram do limiar.
# com o histograma, observa-se na linha, o acumulado da distribuição, que os resíduos estão distribuídos normalmente.
# mais para confirmar faz-se o teste de normalidade, o shapiro-teste
shapiro.test(modelo$residuals)# saida =====> p-value = 0.541 >> 0.05, dessa forma, o entendimento é que de fato os dados estão normalmente disribuídos
var(modelo$residuals) # variancia baixa
mean(modelo$residuals)# prox de zero
# com isso pode-se concluir que foi criado um bom modelo de previsão da ocupaçao do hotel

previsao = forecast(modelo, h = 24) # 24 meses
print(previsao)
autoplot(previsao)


#### Comparação entre modelos

# hold-out, separando os dados
ocupacaoTreino = window(ocupacao, start=c(2003,1), end=c(2015,12), frequency=12)
ocupacaoTeste = window(ocupacao, start=c(2016,1), end=c(2017,12), frequency=12)
## agora vamos gerar um modelo arima com os dados de treino
modeloArima = auto.arima(ocupacaoTreino, trace = T,
                         stepwise = F, approximation = F)
print(modeloArima)
prevArima = forecast(modeloArima, h=24)
#### modelo ets com suavização exponencial
modeloETS = ets(ocupacaoTreino)
prevETS = forecast(modeloETS, h=24)

##### comparando os modelos
plot(ocupacao) # observa-se que os dois modelos acompanharam a sazonalidade
lines(prevArima$mean, col = "blue")
lines(prevETS$mean, col = "green")
legend("topleft", 
       legend = c("dados","Ar.", "ETS"), 
       col = c("black","blue","green"),
       lty = 1:2, cex = 0.8)

plot(ocupacaoTeste, main = "Zoom") # zoom nos dados de treino e os modelos
lines(prevArima$mean, col = "blue", )
lines(prevETS$mean, col = "green")
legend("topleft", 
       legend = c("dados","Ar.", "ETS"), 
       col = c("black","blue","green"),
       lty = 1:2, cex = 0.8)

### acuracia 
accuracy(prevArima, ocupacaoTeste) ## arima teve os melhores resultados
accuracy(prevETS, ocupacaoTeste)
