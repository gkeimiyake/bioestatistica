### Questão 1) Faça uma análise descritiva completa comparando os resultados 
### obtidos no inverno e na primavera para cada variável envolvida no estudo
### (utilize tabelas e gráficos). Faça uma breve discussão comparando os resultados
### obtidos para cada variável. 

### Carregar pacotes 
library(readxl)
library(dplyr)

library(ggplot2)
### organização dos dados 
arquivo1<-"inverno.xlsx"
arquivo2<-"primavera.xlsx"
dados_primavera<-read_excel(arquivo2)
dados_inverno<-read_excel(arquivo1)

### criando função que faz o cálculo da média, mediana, desvio padrão, mínimo e máxim
analise_descritiva<-function(dados) {
       resumo <- dados %>%
         summarise(across (everything(), list(
           media=mean,
           mediana=median,
           moda=mode,
           desvio_padrao=sd,
           minimo=min,
           maximo=max,
           amplitude=range
         )))
       return(resumo)
}

### calculando com a função e inserindo os resultados em objeto próprio 
resumo_inverno <-analise_descritiva(dados_inverno)
resumo_primavera <-analise_descritiva(dados_primavera)

### criando um objeto que tenha comparado a análise dos dois grupos de dados
easyreadinverno<- t(resumo_inverno)
easyreadprimavera<- t(resumo_primavera)
combined_df <- cbind(easyreadprimavera, easyreadinverno)
colnames(combined_df) <- c("Primavera", "Inverno")

### criando boxplots individuais para cada variável  
#   primeiro para o dataset do inverno:
column_names_inverno <- colnames(dados_inverno)
for (col in column_names_inverno) {
  plot <- ggplot(dados_inverno, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", col),
         y = col) +
    theme_minimal()
  print(plot)
  ggsave(paste0("boxplot_", gsub(" ", "_", col), ".png"), plot = plot)
}

#    para primavera:

column_names_primavera <- colnames(dados_primavera)
for (col in column_names_primavera) {
  plot <- ggplot(dados_primavera, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", col),
         y = col) +
    theme_minimal()
  print(plot)
  ggsave(paste0("boxplot_", gsub(" ", "_", col), ".png"), plot = plot)
}


## os resultados podem ser interpretados no outro arquivo, fazer um arquivo mark
## down se for para comparar os resultados por aqui 


### Questão 2)  Desenvolva uma estimativa intervalar com 95% e 99% de confiança
### para massa seca, teor de umidade das amostras, massa total de óleo, densidade, 
### volume e rendimento do óleo essencial para PIPER (PIPERACEAE) –Primavera. 
### Interprete os resultados que obteve. 

# Função para calcular intervalos de confiança
calc_intervalo_confianca <- function(x, conf.level = 0.95) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  se <- sd_x / sqrt(n)
  
  # Valor crítico z
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  # Calcular intervalo de confiança
  lower_bound <- mean_x - z * se
  upper_bound <- mean_x + z * se
  
  return(c(lower_bound, upper_bound))
}

# Calcular intervalos de confiança para 95% e 99%
intervalosprimavera_95 <- sapply(dados_primavera, calc_intervalo_confianca, conf.level = 0.95)
intervalosprimavera_99 <- sapply(dados_primavera, calc_intervalo_confianca, conf.level = 0.99)

# Exibir resultados
intervalosprimavera_95
intervalosprimavera_99

### Fazendo a mesma coisa só que com os dados do inverno para a questão 3:

# Calcular intervalos de confiança para 95% e 99%
intervalosinverno_95 <- sapply(dados_inverno, calc_intervalo_confianca, conf.level = 0.95)
intervalosinverno_99 <- sapply(dados_inverno, calc_intervalo_confianca, conf.level = 0.99)

# Exibir resultados
intervalosinverno_95
intervalosinverno_99


### Questão 4) Com base nos dados amostrais, há evidência estatística significativa
### de que a umidade média da amostra no inverno é inferior a umidade média na 
### primavera, ao nível de 5% de significância? Qual o p-valor?


# Selecionar a coluna de "Unidade" (Umidade)
umidade_primavera <- dados_primavera$`Unidade da amostra (%)`
umidade_inverno <- dados_inverno$`Unidade da amostra (%)`

# Realizar o teste t
t_test_questao_4 <- t.test(umidade_inverno, umidade_primavera, alternative = "less")
# Exibir os resultados
t_test_questao_4



###Com base nos dados amostrais, há evidência estatística significativa de que a
###massa média de óleo da amostra na primavera é superior a massa média de óleo 
###no inverno, ao nível de 5% de significância? Qual o p-valor? 

# Selecionar a coluna de Massa média de óleo 
MMoleo_primavera <- dados_primavera$`Massa total de óleo (mg)`
MMoleo_inverno <- dados_inverno$`Massa total de óleo (mg)`

# Realizar o teste t 
t_test_questao_5 <- t.test(MMoleo_inverno,MMoleo_primavera, alternative= "less")

# Exibir os resultados 
t_test_questao_5

### Questão 6: Com base na amostra da primavera, verifique se há correlação linear 
### significativa entre a umidade da amostra e a massa total de óleo. 
### Ajuste um modelo de regressão para descrever esta relação. Interprete seus resultados.  

# Já temos coluna selecionada para massa de óleo e umidade (primavera)
# Entãoi calculamos a correlação 

correlacao <- cor(umidade_primavera,MMoleo_primavera)
#Ajuste modelo regressão linear:
modelo_regressao <- lm(MMoleo_primavera~umidade_primavera, data = dados_primavera)
#Resumo do modelo de regressão:
resumo_modelo_regressao <- summary(modelo_regressao)
#Exibir resultados
correlacao
resumo_modelo_regressao
 modelo_regressao
# Plotar gráfico de dispersão com os dados de regressão 

ggplot(dados_primavera, aes(x =`Massa total de óleo (mg)`, y = `Unidade da amostra (%)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relação entre Umidade da Amostra e Massa Total de Óleo",
       x = "Umidade da Amostra (%)",
       y = "Massa Total de Óleo (mg)") +
  theme_minimal()


### Questao 7: Com base na amostra do inverno, verifique se há correlação linear
###significativa entre a umidade da amostra e a massa total de óleo. Ajuste 
### um modelo de regressão para descrever esta relação. Interprete seus resultados 

#Repetimos processo da questão 6
correlacao_inverno <- cor(umidade_inverno,MMoleo_inverno)

#Ajuste modelo regressão 
modelo_regressao_inverno <-lm(MMoleo_inverno~umidade_inverno,data=dados_inverno)
resumo_modelo_regressao_inverno<-summary(modelo_regressao_inverno)

#Exibir resultados
correlacao_inverno
resumo_modelo_regressao_inverno

#GRafico de dispersao com os resultados 
ggplot(dados_inverno,aes(x=`Unidade da amostra (%)`, y= `Massa total de óleo (mg)`)) + 
  geom_point() +
  geom_smooth(method = "lm", col="blue") +
  labs (title = "Relação entre Umidade da Amostra e Massa Total do Óleo", 
        x="Umidade da Amostra (%)",
        y="Massa Total do Óleo (mg)") +
  theme_minimal()
### Com base na amostra da primavera, verifique se há correlação linear significativa
### entre a massa seca da amostra e o rendimento total de óleo. Ajuste um modelo de
### regressão para descrever esta relação. Interprete seus resultados. 

# Selecionar a massa seca e rendimento de oleo de primavera 
massa_seca_primavera <-dados_primavera$`Massa seca (20g)`
rendimento_oleo_primavera<-dados_primavera$`Rendimento do óleo (uL g ms)`

# correlação entre os dados
correlacao_primavera_8 <-cor(massa_seca_primavera,rendimento_oleo_primavera)

#ajuste modelo regressao
modelo_regressao_8 <-lm(rendimento_oleo_primavera~massa_seca_primavera,data=dados_primavera)

#resumo modelo regressao
resumo_modelo_regressao_primavera_8<-summary(modelo_regressao_8)

#exibir resultados
correlacao_primavera_8
resumo_modelo_regressao_primavera_8

#Grafico de dispersão com os resultados

ggplot(dados_primavera, aes(x=`Massa seca (20g)`, y=`Rendimento do óleo (uL g ms)`))+
  geom_point()+
  geom_smooth(method = "lm", col="blue")+
  labs(title="Relação entre Massa Seca e Rendimento do óleo", 
       x="Massa Seca (20g)",
       y="Rendimento do óleo (uL g ms)")+
  theme_minimal()

###Questão 9) Com base na amostra do inverno, verifique se há correlação linear
###significativa entre a massa seca da amostra e o rendimento total de óleo.
###Ajuste um modelo de regressão para descrever esta relação. Interprete seus 
###resultados. 

# Selecionamos colunas de massa seca e rendimento do óleo para inverno 
massa_seca_inverno<-dados_inverno$`Massa seca (20g)`
rendimento_oleo_inverno<-dados_inverno$`Rendimento de óleo (uL g ms)`

#Correlação entre dados
correlação_inverno_9<-cor(massa_seca_inverno,rendimento_oleo_inverno)

#ajuste modelo 
modelo_regressao_9<-lm(rendimento_oleo_inverno~massa_seca_inverno,data = dados_inverno)

#resumo modelo
resumo_modelo_inverno_9<-summary(modelo_regressao_9)

#exibir resultados
correlação_inverno_9
resumo_modelo_inverno_9

#grafico 
ggplot(dados_inverno, aes(x=`Massa seca (20g)`, y=`Rendimento de óleo (uL g ms)`))+
  geom_point()+
  geom_smooth(method = "lm", col="blue")+
  labs(title = "Relação entre Massa Seca e Rendimento de Óleo",
       x="Massa seca (20g)",
       y="Rendimento de óleo (uL g ms)")+
  theme_minimal()




# Adicionar uma coluna para indicar a estação
dados_inverno$Estacao <- "Inverno"
dados_primavera$Estacao <- "Primavera"

# Combinar os dados em um único dataframe
dados_combinados <- rbind(dados_inverno, dados_primavera)
colnames(dados_combinados)[1]<- "massa_seca"
colnames(dados_inverno)

ggplot(dados_combinados, aes(x = Estacao, y = rendimento , fill = Estacao)) +
  geom_boxplot(color = "black", fill = "white") + 
  labs(x = "Estacao", y = "Rendimento", title = "Comparação de Rendimento de Óleo entre Inverno e Primavera") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )
