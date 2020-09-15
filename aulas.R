# bibliotecas necessárias no curso
install.packages("forecast")
install.packages("ggplot2")
install.packages("urca")
install.packages("lmtest")
install.packages("seasonal")
install.packages("seasonalview")


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
  