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
  
  #### Como prever usando Series Temps
  ## I- usando os próprios dados da Série - Séries temporais puras
  ## II- usando outras variáveis - modelo explanatório
  ## III- usando as duas técninas - um modelo misto 
  ### Como definir um bom modelo?
  ## I- Avaliar os resíduais
  ## II- Avaliar a performance do modelo (MAE, RMSE, ...) 
  ## III - Avaliar metricas (AIC, BIC, ...)
  ### Em geral: * residuais em conformidade não indicam por si só um bom modelo, 
  ###             é preciso avaliar mais parãmetros;
  ###           * um primeiro bom modelo, provavelmente pode ser melhorado
  ###           * inteligência inclui a capacidade de escolher as informações relevantes
  
  #### Técnicas e previsões 
  ###  Naive: pega o ultimo valor obtido e o projeta para o futuro.
  ###  naive Sazonal: é um modelo naive para dados com sazonalidade, onde ele considera o 
  ###                ultimo valor no periodo de tempo. Por exemplo: para prever Jan/2021, será 
  ###                usado o valor obtido em jan/2020. Para fev/2021, será usado fev/2020, ...
  #library(forecast)
  #library(ggplot2)
  
  # passeio aleatorio
  set.seed(4312) # setando a  semente, de modo que seja possível reproduzir o ensaio
  x = cumsum(sample(c(-1,1), #intervalo
                    100, #num amostra
                    replace = T))# com reposição, os valores poderão se repetir
  print(x) # é um passeio aleatório, os valores vão variando em +1 ou -1
  # transformando em uma TS anual (frequency=1)
  serieX = ts(x, start=c(1901), end = c(2000),frequency = 1)
  autoplot(serieX)  
  
  ## previsão de 5 anos com a func naive do pacote forecast
  ## A técnica Naive faz previsão linear para o futuro sem tendência e sem sazonalidade
  prevX = naive(serieX, h=5) # h é o parametro que indica a quantidade de previsões
  prevX ## o ultimo ponto da série foi o -2, então a partir dele que ocorerram as previsões
  prevX$fitted # valores preditos
  prevX$residuals # residuais
  autoplot(prevX)  # plot da TS com intervalo de confiança de 80% e 95%

    ## mudando o intervalo de confiança
  prevX95_99 = naive(serieX, h=5, level = c(95,99)) # IC de 95 a 99
  print(prevX95_99)
  autoplot(prevX95_99)
  split.screen(figs = c(2,1))
  screen(1)
  plot(prevX)
  screen(2)
  plot(prevX95_99)
  close.screen(all=T)  
  
  # naive em dados sazonais, usando o dataset mensal AirPassengers
  prevAir = snaive(AirPassengers, h=12)
  autoplot(prevAir) # observa-se que é apresentado exatamente o ultimo ano, com margens de erro de 80 a 95%
  prevAir$mean # esses foram os pontos previstos
  window(AirPassengers, start=c(1960)) # comparando com o ultimo ano do dataset
  # são os mesmos valores
  
  
  #### técnica MEAN que calcula a média dos dados históricos e extrapola para o futuro
  #### essa técnica pode ser aplicada quando os dados são aleatórios, random walk por exemplo
  autoplot(fdeaths) # mortes por cancer de pulmão no reino unido
  mean(fdeaths)
  prev = meanf(fdeaths, h=4)
  print(prev)
  autoplot(prev)
  ## os dados mais recentes de uma TS possue o peso maior na previsão
  ## usando os dados do intervalo 1/1976 até 12/1979
  fdeaths76_79 = window(fdeaths, start=c(1976,1), end = c(1979,12))
  autoplot(fdeaths76_79)
  mean(fdeaths76_79)
  prev76_79 = meanf(fdeaths76_79, h=4)
  print(prev76_79)
  autoplot(prev76_79)
 # comparando as previsões
  plot(prev)
  lines(prev76_79$mean, col="red") # exibindo os valores previstos com os dados da janela
  
  ### Técnica DRIFT
  ### Essa técnica não acompanha o crescimento linear, ela acompanha a sazonalidade, para isso
  ### ela utiliza o primeiro e o ultimo valor do intervalo para traçar uma reta, criando uma 
  ### reta de tendência para o futuro.
  autoplot(austres)
  prev = rwf(austres, h=12, drift = F)# com o drift = F, a funcao rwf se comporta como o naive
  autoplot(prev)                      # simplismente extrapolando a ultima previsao
  prevDrift = rwf(austres, h=12, drift = T)
  autoplot(prevDrift)
  
  
  ### previsão com decomposição com a função stlf do pacote forecast
  #library(forecast)
  #library(ggplot2)
  autoplot(AirPassengers)
  prev = stlf(AirPassengers, h = 48)# criando o modelo
  autoplot(prev)
  
  ### previsão com SUAVIZAÇÂO EXPONENCIAL
  ### Principio Básico:
  ##    * As observações passadas possuem pesos
  ##    * Quanto mais recente as observações, maiores serão pesos nas previsões
  ##    * Utiliza média que reduzem quanto mais distantes são as observações
  ##    * O parametro alfa, entre 0 e 1, define o índice de redução:
  ##        Prox de 0 - observações mais antigos tem maior peso
  ##        Prox de 1 - observações mais recentes tem maior peso
  
  #library(foreach)
  #library(ggplot2)
  
  ## Tendencia linear de Hold (Hold Linear Trend - 1957)
  ## Indicada para TS com tendencia, aplica a suavização exponencial, gerando uma tendencia 
  ## linear para o futuro.
  autoplot(austres) # é uma TS trimestral, tem uma tendencia forte e é linear
  modelo1 = holt(austres, h = 16 ) # com horizonte de 16 teremos uma previsão de 4 anos pra frente
  autoplot(modelo1)
  modelo1$model # exibindo o modelo criado, observa-se que o alpha foi de 0.999
  
  ## modelo com mais peso nos dados antigos
  modelo2 = holt(austres, h = 16, alpha = 0.2) # alpha é o peso dos dados com o tempo
  modelo2$model
  #comparando os modelos
  plot(modelo1)
  lines(modelo2$mean, col = "red") # plotando os point forecast, os pontos previstos
  # observa-se que o modelo com mais peso aos dados passados, possuem uma tendencia média mais 
  # baixa que o modelo com alpha de 0.999
  
  ## Tendência amortecida (Damped - 1985)
  # é uma evolução da tendencia linear de hold adicionando um parametro de amortecimento na previsão
  modelo3 = holt(austres, h = 16,
                 damped = T, # ativando o amortecimento
                 phi = 0.9)  # nivel de amortecimento da tendencia linear de Holt
  
  autoplot(modelo3)
  modelo4 = holt(austres, h = 16,
                 damped = T, # ativando o amortecimento
                 phi = 0.8)  # nivel de amortecimento da tendencia linear de Holt
  
  autoplot(modelo4)
  # comparando graficamente
  plot(modelo3)
  lines(modelo4$mean, col="red")
  # comparando textualmente
  print(modelo3$mean)
  print(modelo4$mean)
  
  ## Holt-Winters Sazonal
  # Modelo que inclue tanto a captura da sazonalidade quanto a tendencia. Possue duas forma:
  # aditivo, indicado para TS com variação sazonal constante, e o multiplicativo, indicado para
  # TS com variação sazonal variante na série
  modelo5 = hw(JohnsonJohnson, h=16,
               seasonal = "additive")
  autoplot(modelo5) # observa-se que o modelo prevê tanto a tendencia quanto a sazonalidade, e que o intervalo de confiança
  #vai aumentando conforme a pervisão se afasta.
  modelo6 = hw(JohnsonJohnson, h=16,
               seasonal = "multiplicative")
  autoplot(modelo6)
  
  # comparando os medelos graficamente
  plot(modelo5)
  lines(modelo6$mean, col ="red" )
  # comparando os medelos textualmente
  print(modelo5$mean)
  print(modelo6$mean)
  
  # modelo holt-winter sazonal multiplicativo amortecido com o phi de 0.9
  modelo7 = hw(JohnsonJohnson, h = 16,
               seasonal = "multiplicative", 
               damped = T, #amortecimento
               phi = 0.9) #taxa de amortecimento
  autoplot(modelo7) # observa-se que há um amortecimento na previsão 
  
  # ETS (Erro, Trend, Seasonal)
  # é um poderoso método onde os dados são repassados e ele vai ajustando os parâmetros de 
  # sazonalidade e tendência ou os parametros podem ser configurados.
  ## Na prática inicalmente se cria o modelo
  modelo8 = ets(JohnsonJohnson) # definindo utomaticamente os parametros
  print(modelo8) # exiindo os parametros do modelo criado
  autoplot(modelo8$residuals) # residuos do modelo
  autoplot(modelo8$fitted) # valores ajustados
  ## fazendo a previsão
  prev = forecast(modelo8, h =16, level = c(85,90))
  autoplot(prev)  
  # configuração manual dos parametross
  # para definir se será usado o modelo aditivo ou multiplicativo, inicialmente será feita a decomposição da TS para verificar 
  # a variação da sazonalidade e a tendẽcia.
  autoplot(decompose(JohnsonJohnson))
  # observa-se que aparentemente a série apresenta uma sazonalidade constante, então o modelo aditivo é o mais indicado e 
  # a tendência apresenta uma inclinação forte, então aplicar o amortecimento é uma opção
  # Feita a análise, será então criado o modelo
  modelo9 = ets(JohnsonJohnson, damped = T,
                model = "ZAA") #modelo com erro=Auomático (z), Tendencia = Aditiva, Sazonlidade = Aditivo
  print(modelo9)  # no resumo temos que foi criado ETS(M,Ad,A), onde o Erro foi Multiplicado (pq foi definido autoaticamente)
 # a sazonalidade foi Ad, aditiva amortecida e a sazonalidade foi aditiva
  modelo10 = ets(JohnsonJohnson, damped = T, model = "ZZZ") #Deixando tudo automático
  print(modelo10) # foi criado o modelo ETS(M,Ad,A) que foi exatamente o modelo criado com a análise feita com a decomposição

  # Técnica ARIMA 
  # é uma tecnica robusta, podendo ser aplicada em praticamente qualquer TS, no entanto funciona melhor com dados estáveis
  # com pouco outlier. Um requisito para o uso do Arima é que os dados sejam estacionários, caso não sejam, então podem ser 
  # transfirmados usandoa a diferenciação para remover a tendência. 
  # O Arima não sazonal possue três elementos:
  #   * AR - Autoregressivo, avalia a relação entre os períodos (lags): autocorrelação, extraindo essa influencia para prever o futuro
  #   * I - Integrated, aplica a diferênciação se necessário
  #   * MA - Media Movel, avalia os erros entre períodos e extraí esses erros 
  # Onde os parametros são:
  #   * p - ordem da parte autoregressiva
  #   * d - grau de diferênciação
  #   * q - ordem da média móvel
  # Por exemplo:
  # p = 1, signifca que uma determinada observação pode ser explicada pela observaçaõ prévia + erro
  # p = 2, signifca que uma determinada observação pode ser explicada por DUAS observações prévias + erro
  
  # d = 0, significa que não é aplicado a diferenciação
  # d = 1, significa que é aplicado a diferenciação de primeira ordem
  # d = 2, significa que é aplicado a diferenciação de segunda ordem
  
  # q = 1, signifca que uma determinada observação pode ser explicada erro da observação prévia
  # d = 0, significa que uma determinada observação pode ser explicada erro de duas observações prévias
    
  # AR(1) ou ARIMA(1,0,0), apenas elemento autoregressivo de 1ª ordem 
  # AR(2) ou ARIMA(2,0,0), apenas elemento autoregressivo de 2ª ordem 
  # MA(1) ou ARIMA(0,0,1) apena media movel
  # ARMA(1,1) ou ARIMA(1,0,1) apenas autoregressão e media movel de ordem 1
  
  ## PARA DADOS SAZONAIS -- ARIMA(P,D,Q), mas se for o caso é melhor remover a sazonalidade com o processo de decomposição
  
  # Como definir os parametros?
  # p - é a ordem da parte autoregressiva - PACF
  # d - grau de diferenciação - Teste de estacionaridade
  # q - ordem da média móvel - ACF
   
  #Como saber qual é o melhor modelo?
  # provavelmente será o modelo que tiver o Akaike Information Criteria (AIC e AICc (usado quando há uma amostra pequena)) 
  # e o Baysian Information Criteria (BIC) menor
  
  ## Diagrama de processo para a construção de um modelo ARIMA
  # Exploração dos dados (decomposição) => Estabilizar (outlier, diferenciação) => Modelo (inicialmente o autoarima 
  # para identificar o melhor modelo) => avalia o resíduo para identificar padrões => Previsão (para olhar para o futuro).
  
  # Devido ao grau de dificuldade na elaboração do modelo ARIMA, visto que há muitas combinações do modelo
  # (que tal um AG que minimize o AICc e/ou BIC???????????) de será utilizado o auto.arima
  
  #library(forecast)
  #library(ggplot2)
  
  modelo = auto.arima(co2, trace = T)# trace é para mostrar o processo iterativo
  # Como resposta o  ARIMA(1,1,1)(1,1,2)[12], onde a parta não sazonal ficou com a conf(1,1,1) e a parte sazonal (1,1,2) 
  # e a frequencia dos dados igual a 12
  print(modelo) # AIC=180.78   AICc=180.97   BIC=205.5
  #tentando melhorar o modelo, removendo o stepwise o auto.arima fará uma busca maior de combinações
  modelo2 = auto.arima(co2, trace = T,
                       stepwise = F,
                       approximation = F)
  # A resposta da busca profunda foi: Best model: ARIMA(0,1,3)(0,1,1)[12]  
  print(modelo2) #metricas AIC=176.86   AICc=177   BIC=197.47
  
  prev1 = forecast(modelo, h=12)
  autoplot(prev1)
  prev2 =  forecast(modelo2, h=12)
  autoplot(prev2)
  
  # comparando as previsões
  plot(prev1)  
  lines(prev2$mean, col = "red")  
  
  
  ## Regressao com TS, usando a funcao tslm(varDependente ~ varIndependente ) do pacote forecast
  plot(Seatbelts) # explorando a TS
  x1 = tslm(DriversKilled ~ trend, data = Seatbelts) #modelo 1
  x2 = tslm(DriversKilled ~ season, data = Seatbelts)  #modelo 2
  x3 = tslm(DriversKilled ~ trend+season, data = Seatbelts)  #modelo 3 multipla
  
  # Performance dos modelos com a funcao CV que diz qual a melhor combinação de parametros
  CV(x1)
  CV(x2)
  CV(x3)# esse foi o melhor modelo pois foi baixo no CV*,AIC*,AICc e BIC e maior no AdjR2*, onde * são os principais parametros
  
  r1 = forecast(x1, h=12) # apenas a tendencia
  autoplot(r1)
  r2 = forecast(x2, h=12) # apenas a sazonalidade 
  autoplot(r2)
  
  r3 = forecast(x3, h=12) # sazonalidade  e tendencia
  autoplot(r3)
  
  #comparando os modelos
  plot(r1)
  lines(r2$mean, col = "green")
  lines(r3$mean, col = "red")
  legend("topright",legend = c("tend", "seas", "tend+seas"),
         col = c("blue", "green", "red"), lty = 1:2, cex=0.8)
  
  
  # REDE NEURAL PARA TS - Autoregressive Neural Network (NNAR)
  # principais parametros: intervalo de entrada e nós da camada interna
  # saída: apenas preisão pontual, não gera intervalos de confiança
  modeloRNA = nnetar(co2) # pacote forecast
  prevRNA = forecast(modeloRNA, h=24)  
  print (prevRNA)
  autoplot(prevRNA)  
  
  # explorando modelos com a adição de variaveis independentes
  plot(Seatbelts)
  autoplot(Seatbelts[,c("DriversKilled")]) # visualizando apenas a variavel DriversKilled
  # criando a var independente usando o intervalo de 1980 a 1983 da variavel DriversKilled
  dados = window(Seatbelts[,c("DriversKilled")], start = c(1980,1), end = c(1983,12))
  print(dados)
  modelo1 = auto.arima(dados)
  print(modelo1)
  prev1 = forecast(modelo1, h =12)
  autoplot(prev1)
  
  # modelo 2 usará  uma vetor de var independent
  motorista = as.vector(window(Seatbelts[,c("drivers")], start = c(1980,1), end = c(1983,12)))
  print(motorista)

  modelo2 = auto.arima(dados, xreg = motorista)
  print(modelo2)  
  print(modelo1) # o modelo apresentou indices melhores
  # é necessário obter o vetor dos dados no ano para fazer a previsão
  motoristas2 = as.vector(window(Seatbelts[,c("drivers")], start = c(1984,1), end = c(1984,12)))
  prev2 = forecast(modelo2, xreg = motoristas2) # usandoa  variavel explanatória junto
  print(prev2)  
  
  autoplot(prev2)  
  
  # comparando
  plot(prev)
lines(prev2$mean, col = "red")
