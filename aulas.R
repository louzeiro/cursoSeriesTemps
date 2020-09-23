# bibliotecas necessárias no curso
#install.packages("forecast")
#install.packages("ggplot2")
#install.packages("urca")
#install.packages("lmtest")
#install.packages("seasonal")
#install.packages("seasonalview")


####---- testes iniciais conhecendo a funcao TS
myts = rnorm(60) # gerando 60 num aleatorios normalmente distribuidos
# para gerar uma serie temporal é necessário usar a funcao ts
# nesse caso, vamos gerar uma ts de 60 amostras emulando uma série mesanl de 
# 5 anos jan/12 a dez/16
myts = ts(myts,
          start = c(2012,1),
          end = c(2016,12),
          frequency = 12) # freq mensal

class(myts) # verificando a classe
plot(myts) # plot da serie temporal criada

#### --- Importe de dados
## o arq usado tem a estrutura (ano,valor), no intervalo[1884,1934]
tempts = read.csv(file.choose(), # escolhendo o arquivo
                  sep = ",",     # separador
                  header = FALSE)# não há cabeçalho
print(tempts) #exibindo o objeto criado, foram criadas duas colunas V1 e V2

tempts = ts(tempts[2], # pegandoa a segunda coluna do objeto criado
            start = c(1884),
            end = c(1934),
            frequency = 1 # anual
            )
  plot(tempts)

  ###---- Extra sobre a função ts
  ## Argumento frequency, exemplos de configuração (ciclo*coleta)
  ## Exemplo1: dados coletados de hora em hora em uma linha de produção
  ##          Ciclo: dia, Coleta: hora, frequency=1*24=24
  ## Exemplo2: dados coletados de hora em hora de temperatura
  ##          Ciclo: ano, Coleta: hora, frequency=365*24=8736
  ## Exemplo3: dados coletados por dia em um ano de venda
  ##          Ciclo: ano, Coleta: dia, frequency=365*1=365
  ## Exemplo4: dados coletados por mês em um ano de venda
  ##          Ciclo: ano, Coleta: dia, frequency=12*1=12
  ## Comentário: no ex3 o ciclo se repete depois de 365 dias coletados
  ## no exx4 o ciclo se repete depois de 12 meses coletados
  
  
  ### explorando o conj de dados de manchas solares
  print(sunspots)  
  class(sunspots)
  summary(sunspots)
  start(sunspots) # inicio da serie, jan/1749
  end(sunspots) # fim da série, dez/1983
  frequency(sunspots) # frequencia da série
  sun2 = window(sunspots, #dataset
                start=c(1749,1), #inicio da janela
                end=c(1759,12)) #fim da janela
 print(sun2)  
 
 ### visualizações 
 plot(sunspots)
 hist(sunspots)
 boxplot(sunspots) 

 library(ggplot2)
 library(forecast)
 autoplot(AirPassengers) # ggplot2
 plot(aggregate(AirPassengers, FUN = mean)) # agregando a média anuais dos passageiros
 
 #Conceitos importantes
 
 # estarionaridade
 # é um conceito muito importante em TS visto que, dependendo da classificação a metodologia é escolhida. Uma TS é 
 # estacionária quando a série varia entorno de uma média e uma variância, como a sunspots, quando há sazionalidade
 # e/ou tendência a série é dita não estacionária. A classificação pode ser visual, verificando o comportamento, ou 
 # com testes de hipóteses, nesse caso: Dickey-Fuller, KPSS, Philips-Perron.
 
 # componentes de TS
 # => tendência
 # => sazionaridade é a componente da TS que, se presente, há padrões que repete-se em intervalos regulares. 
 # => ciclo é um aumento ou redução de frequencia sem intervalos fixos.
 # => erro, dentre as componentes, o erro é a unica que nãopode ser explicada matematicamente, são dados que estão na TS
 # e são aleatórios.
 
 # correlação r
 # mostra a força e a direção da relação entre variaveis aleatórias, por exemplo em uma correlação positiva 
 # temos que quando x aumenta y aumenta tbm. Varia de -1 a 1.
 
 # coeficiente de determinação R^2
 # Determina o quanto o modelo consegue explicar os valores, quanto maior o valor mais explicativo o modelo é. 
 # Varia entre 0 e 1 e é obtido com o quadrado do coeficiente de correlação. Por exemplo r=0,93 r^2=0,86, nesse caso
 # 86% das variaveis dependentes conseguem ser explicadas pela variável independente.
 
 # Autocorrelação
 # é a relação da variável com ela msm. Em séries temporais há apenas uma variável e uma anotação de tempo, essa anotação de 
 # tempo não se qualifica como variavel aleatória. Nesse caso utiliza-se a autocorrelação para verificar se existe
 # uma relação matemática entre os intervalos da TS. Varia de -1 a 1. Ela é media em intervalos (LAG), por exemplo:
 # para lag de 1 intervalo, será calculado a autocorrelação entre os valoes de 01 intervalo vizinho, para 2 será calculado 
 # a autocorrelação de 2 períodos distantes. Utiliza-se o diagrama de autocorrelação (ACF) para exibir a autocorrelação de uma TS. 
 # o PACF mede a correlação parcial de ACF em diferentes intervalos. 
 
 # Erro e residuais (rever)
 
 # Importancia dos Residuais em modelos de TS
 # Os resíduos não devem estar relacionados, ou seja zero autocorrelação, pois se existir correlação então o modelo 
 # precisa se revisado pois, há informações nos residuos que deveriam ser utilizadas na previsão. Outra premissa é que 
 # a média residual precisa ser aproximadamente 0. Dessa forma, analisar residuais é importantíssimo para avaliar o modelo.
 
 # Pratica de análise residuais
 library(ggplot2)
 library(forecast)
 autoplot(presidents) # TS da avaliação dos presidentes USA
 #para analisar os residuos é necessário criar um modelo e avaliar os residuos. Nesse caso:
 modelo = auto.arima(presidents)
# o objeto modelo tem os residuos.
  modelo$residuals
  autoplot(modelo$residuals) # aparentemente comportamento aleatório.
  hist(modelo$residuals) # parece estar normalmente distribuídos
  var(modelo$residuals, na.rm = T) # a variancia sem considerar o NA
  mean(modelo$residuals, na.rm = T) # media dos residuos -0.49 considera-se estatisticamente prox de 0
  
  Acf(modelo$residuals, na.action = na.pass) #diagrama de correlação do residuais, na.action = na.pass não considera os NA 
  # No pacote forecast há uma função que analisa os residuais.
  checkresiduals(modelo) # nesse caso como a função é expecífica para análise de residuos, então utiliza-se o modelo criado
                         # Como resultado do teste de hipótese resultou em 0.07285 > 0.05 então os residuos são considerados
                         # ruídos brancos, e dessa forma eles não carregam informações relevantes para o modelo.
  # Outro requito é se os resíduos estão normalmente distribuidos, para verificar aplica-se o teste de hipótese shapiro test.
  shapiro.test(modelo$residuals)# nesse caso deu 0.5862 > 0.05 logo está normalmente distribuído.
  # assim, após essa bateria de testes e visualizações pode-se dizer que este é um bom modelo preditivo.
  
  ## testes para avaliar a estacionaridade da Série.
  ## A classifiação de uma série quanto a sua estacionaridade é importante pois com base nisso o modelo é tratado, 
  ## em casos de séries não estacionárias, é realizada uma diferenciação transformando-a em estacionária.
  ## Uma TS é estácionária  quando a média e a variância são constantes ao longo do tempo. 
  ## realizando o teste de hipótese de estacionaridade, onde p-value > 0.05 é indício de estacionaridade
  Box.test(airmiles, type = "Ljung-Box") # resultando em p-value = 5.035e-06, nesse caso há indícios que não é estacionária
  # então é necessário fazer a diferenciação da TS. 
  airmilesDiferenciada =  diff(airmiles)
  Box.test(airmilesDiferenciada,type = "Ljung-Box") # resutando em  p-value = 0.1407 > 0.05 muito acima do alfa padrão
  # sendo um indício de que a série está estacionária
  # comparando as séries.
  split.screen(figs = c(2,1)) # dividindo a tela
  screen(1) # tela 1
  plot(airmiles, main = "Airmiles")
  screen(2)
  plot(airmilesDiferenciada, main = "Diferenciação")
  
  # teste para verificar quantos processos de diferenciação são necessários na série.
  ndiffs(airmiles, test="pp") # philps-perron, resultou em 1
  ndiffs(airmilesDiferenciada, test="pp")# resultou em 0, nesse caso não há necessidade de diferenciar novamente
  
  ## Decomposição
  ## é o processo de separar a sazonalidade, tendencia e o erro da ts. É um processo para o compreender e previsões. 
  ## Os modelo principais são: o aditivo, que é melhor quando o elemento sazonal ou tendência não é proporcional ao nível 
  # da série, o valor médio,  e o multiplicativo, que é melhor quando o elemento sazonal ou tendência muda com o nível da série
  
  # Transformações
  #library(ggplot2)
  #library(forecast)
  
  t1 =BoxCox(AirPassengers, lambda = 0)# transf log, lambd=0
  t2 = BoxCox(AirPassengers, lambda = 0.1)# transf pot, lambd != 0
  lbd = BoxCox.lambda(AirPassengers)# essa fucnao faz a geração do lambda
  t3 = BoxCox(AirPassengers, lbd)  
  t4 = diff(AirPassengers)  # diferenciação
  t5 = log10(AirPassengers)  # trans log
  # exibindo as transformações exceto a diferenciação, observa-se que o eixo y mudou em ambas
  split.screen(figs=c(2,2)) # 2 linhas e 2 colunas
  screen(1) 
  plot(t1)
  screen(2) 
  plot(t2)
  screen(3) 
  plot(t3)
  screen(4) 
  plot(t5)
  close.screen(all=T) # fechando a divisão de tela
  # medias móveis, mais uma tecnica de transformação de series, nesse caso a media movel suavisa os dados
  # a ordem da média móvel diz quantas "casas" serão usadas p.ex. ordem 5, serão usadas duas casas antes, duas depois e o ponto central
  
  #library(ggplot2)
  #library(forecast)
  autoplot(fdeaths) # uma TS de mortes por cancer no pulmão???
  
  fdeaths5 = ma(fdeaths, order = 5) # funcao do forecast que calcula a media movel
  autoplot(fdeaths5)
  fdeaths12 = ma(fdeaths, order = 12) #  com um intervalo de 12, observa-se que faltaram os dados dos 6 primeiros meses
  autoplot(fdeaths12) # e os 6 ultimos da TS
  # funcao de suavização, limpesa de outlier de TS do pacote forecast
  fdeathsClean = tsclean(fdeaths)
  autoplot(fdeathsClean)
  
  #plotando os dados originais e as suavizações
  plot(fdeaths) 
  lines(fdeaths5, col = "red")
  lines(fdeaths12, col = "blue")
  lines(fdeathsClean, col = "green")
 legend("topright", legend = c("Orig.", "MA5","MA12", "TSC"),
        col = c("black", "red", "blue", "green"), 
        lty = 1:2, cex = 0.8)  
 
 
 