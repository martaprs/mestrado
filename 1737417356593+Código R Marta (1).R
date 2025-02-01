
# AED chamar dataset e ver ------------------------------------------------
Social_Meida_Dataset <- read.csv ('Social Meida Dataset Marta.csv', sep = ';',dec=',')

# 1. Load necessary libraries
library(dplyr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(corrplot)
library(GGally)

# 3. Quick summary checks
skim(Social_Meida_Dataset)
summary(Social_Meida_Dataset)   # Base R
plot_missing(Social_Meida_Dataset)

# 4. Exploratory Visualizations

## 4.1 Numeric correlations
numeric_data <- dplyr::select_if(Social_Meida_Dataset, is.numeric)
corr_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(corr_matrix, method = "circle")

## 4.2 Pairwise plots
ggpairs(numeric_data)

## 4.3 Histograms / Boxplots / Scatter
ggplot(Social_Meida_Dataset, aes(x = Age)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "white") +
  theme_minimal()

ggplot(Social_Meida_Dataset, aes(x = Gender, y = `Amount Spent (USD)`)) +
  geom_boxplot() +
  theme_minimal()

ggplot(Social_Meida_Dataset, aes(x = `Income (USD)`, y = `Amount Spent (USD)`)) +
  geom_point(aes(color = Gender)) +
  theme_minimal()


qacBase::contents(Social_Meida_Dataset)
library(funModeling)

# Quick status of the dataset: data type, missing values, etc.
df_status(Social_Meida_Dataset)




library(inspectdf)


# Inspect categorical variables
inspect_cat(Social_Meida_Dataset) %>% show_plot()










# roteiro 01 --------------------------------------------------------------
# Identificar outliers nas variáveis numéricas
boxplot(Social_Meida_Dataset$`Income (USD)`, main = "Income (USD)")
boxplot(Social_Meida_Dataset$`Amount Spent (USD)`, main = "Amount Spent (USD)")
boxplot(Social_Meida_Dataset$`Social Media Usage (Hours/Day)`, main = "Social Media Usage (Hours/Day)")
### Não faz sentido retirar outliers





# Recodificar variáveis categóricas (exemplo: Education Level)
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Education_Level_Recoded = case_when(
    `Education Level` == "High School" ~ "Ensino Médio",
    `Education Level` == "Bachelor's" ~ "Graduação",
    `Education Level` == "Master's" ~ "Mestrado",
    `Education Level` == "PhD" ~ "Doutorado"
  ))
# Tabela de frequência para variáveis categóricas
table(Social_Meida_Dataset$Gender)
table(Social_Meida_Dataset$Education_Level_Recoded)
table(Social_Meida_Dataset$`Purchase Decision`)



# Medidas descritivas
summary(Social_Meida_Dataset$`Income (USD)`)
summary(Social_Meida_Dataset$`Amount Spent (USD)`)
summary(Social_Meida_Dataset$`Social Media Usage (Hours/Day)`)

# Cálculo do Coeficiente de Variação (CV)
cv <- function(x) { sd(x) / mean(x) * 100 }
cv(Social_Meida_Dataset$`Income (USD)`)
cv(Social_Meida_Dataset$`Amount Spent (USD)`)
cv(Social_Meida_Dataset$`Social Media Usage (Hours/Day)`)









# Cruzamento entre Gender e Purchase Decision
table(Social_Meida_Dataset$Gender, Social_Meida_Dataset$`Purchase Decision`)

# Gráfico de barras para Gender vs Purchase Decision
ggplot(Social_Meida_Dataset, aes(x = Gender, fill = `Purchase Decision`)) +
  geom_bar(position = "dodge") +
  theme_minimal()

# Gráfico de dispersão entre Income e Amount Spent
ggplot(Social_Meida_Dataset, aes(x = `Income (USD)`, y = `Amount Spent (USD)`, color = Gender)) +
  geom_point() +
  theme_minimal()

# Gráfico de boxplot para Social Media Usage por Education Level
ggplot(Social_Meida_Dataset, aes(x = Education_Level_Recoded, y = `Social Media Usage (Hours/Day)`)) +
  geom_boxplot() +
  theme_minimal()




# Modificação final na base -----------------------------------------------


# Carregar bibliotecas necessárias
library(dplyr)
library(forcats)

# 1. Remover a categoria "Non-binary" da variável Gender
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  filter(Gender != "Non-binary")

# 2. Agrupar "Ensino Médio" com "Graduação" e renomear
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Education_Level_Recoded = case_when(
    `Education Level` == "High School" ~ "Graduação ou Ensino Médio",
    `Education Level` == "Bachelor's" ~ "Graduação ou Ensino Médio",
    `Education Level` == "Master's" ~ "Mestrado",
    `Education Level` == "PhD" ~ "Doutorado"
  ))

# 3. Agrupar Product Category em 3 tipos principais
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Product_Category_Grouped = case_when(
    `Product Category` %in% c("Beauty", "Fashion") ~ "Beleza e Moda",
    `Product Category` %in% c("Electronics", "Home Decor") ~ "Eletrônicos e Casa",
    `Product Category` %in% c("Fitness", "Food") ~ "Saúde e Bem-Estar",
    TRUE ~ "Outros"  # Para categorias como "N/A"
  ))

# 4. Remover a variável Specific Product
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  dplyr::select(-`Specific Product`)

# 5. Agrupar "Not Influential" com "Somewhat Influential"
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Influence_Level_Grouped = case_when(
    `Influence Level` %in% c("Not Influential", "Somewhat Influential") ~ "Pouco ou Medianamente Influente",
    `Influence Level` == "Very Influential" ~ "Muito Influente"
  ))

# 6. Agrupar Social Media Platforms de forma inteligente
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Social_Media_Platforms_Grouped = case_when(
    grepl("Facebook|Instagram", `Social Media Platforms`) ~ "Facebook e Instagram",
    grepl("TikTok|Snapchat", `Social Media Platforms`) ~ "TikTok e Snapchat",
    grepl("LinkedIn|Twitter", `Social Media Platforms`) ~ "LinkedIn e Twitter",
    TRUE ~ "Outras Plataformas"
  ))

# 7. Remover a variável Brand Name
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  dplyr::select(-`Brand Name`)

# 8. Remover valores iguais a 0 em Amount Spent (USD)
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  filter(`Amount Spent (USD)` > 0)

# Verificar o resultado
skim(Social_Meida_Dataset)









summary(as.factor(Social_Meida_Dataset$Social_Media_Platforms_Grouped))
summary(as.factor(Social_Meida_Dataset$Influence_Level_Grouped))
summary(as.factor(Social_Meida_Dataset$Product_Category_Grouped))
summary(as.factor(Social_Meida_Dataset$Education_Level_Recoded))
summary(as.factor(Social_Meida_Dataset$City))



# 1. Remover variáveis antigas que foram modificadas
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  dplyr::select(-`Education Level`, -`Social Media Platforms`, -`Product Category`, -`Purchase Decision`, -`Customer Name`)

# 2. Agrupar "Doutorado" e "Mestrado" em uma única categoria
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Education_Level_Recoded = case_when(
    Education_Level_Recoded %in% c("Doutorado", "Mestrado") ~ "Pós-Graduação",
    TRUE ~ Education_Level_Recoded  # Mantém "Graduação ou Ensino Médio"
  ))

# 3. Agrupar "LinkedIn e Twitter" com "TikTok e Snapchat" em uma única categoria
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Social_Media_Platforms_Grouped = case_when(
    Social_Media_Platforms_Grouped %in% c("LinkedIn e Twitter", "TikTok e Snapchat") ~ "Outras Plataformas",
    TRUE ~ Social_Media_Platforms_Grouped  # Mantém "Facebook e Instagram"
  ))

# 4. Categorizar a variável City em grupos maiores
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(City_Grouped = case_when(
    City %in% c("Los Angeles", "New York", "Chicago", "Miami") ~ "Grandes Metrópoles",
    City %in% c("Austin", "Boston", "San Francisco", "Seattle") ~ "Cidades Tecnológicas",
    City %in% c("Houston", "Phoenix", "San Diego", "Denver") ~ "Cidades em Expansão",
    TRUE ~ "Grandes Metrópoles"  # Para cidades com menos representatividade
  ))

# Verificar o resultado final
skim(Social_Meida_Dataset)
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  dplyr::select(-City)
skim(Social_Meida_Dataset)
qacBase::contents(Social_Meida_Dataset)
## RELATÓRIO DO ROTEIRO 01: EXPLORAÇÃO DE DADOS

## 1. CARREGAMENTO E INSPEÇÃO INICIAL
## - O dataset foi carregado e inspecionado para verificar a estrutura, tipos de variáveis e dados ausentes.
## - Verificou-se que não há dados ausentes e que as variáveis estão distribuídas entre numéricas e categóricas.

## 2. TRATAMENTO DE OUTLIERS E DADOS AUSENTES
## - Foram identificados outliers nas variáveis numéricas, mas decidiu-se não removê-los, pois fazem parte do contexto da pesquisa.
## - Dados ausentes foram verificados e confirmou-se que não há valores faltantes.

## 3. RECODIFICAÇÃO DE VARIÁVEIS CATEGÓRICAS
## - A variável `Education Level` foi recodificada para português e agrupada em:
##   - "Graduação ou Ensino Médio" (combinação de "High School" e "Bachelor's").
##   - "Pós-Graduação" (combinação de "Mestrado" e "Doutorado").
## - A variável `Product Category` foi agrupada em três categorias principais:
##   - "Beleza e Moda" (Beauty e Fashion).
##   - "Eletrônicos e Casa" (Electronics e Home Decor).
##   - "Saúde e Bem-Estar" (Fitness e Food).
## - A variável `Influence Level` foi agrupada em:
##   - "Pouco ou Medianamente Influente" (combinação de "Not Influential" e "Somewhat Influential").
##   - "Muito Influente" ("Very Influential").
## - A variável `Social Media Platforms` foi agrupada em:
##   - "Facebook e Instagram".
##   - "Outras Plataformas" (combinação de "LinkedIn e Twitter" e "TikTok e Snapchat").
## - A variável `City` foi agrupada em:
##   - "Grandes Metrópoles" (Los Angeles, New York, Chicago, Miami e demais cidades).
##   - "Cidades Tecnológicas" (Austin, Boston, San Francisco, Seattle).
##   - "Cidades em Expansão" (Houston, Phoenix, San Diego, Denver).


## 4. REMOÇÃO DE VARIÁVEIS DESNECESSÁRIAS
## - As seguintes variáveis foram removidas:
##   - `Specific Product`: Muitas categorias com poucas observações.
##   - `Brand Name`: Muitas marcas com poucas observações, não relevantes para a análise.
##   - `Purchase Decision`: Após a remoção de valores zero em `Amount Spent (USD)`, a variável ficou com apenas uma categoria ("Yes").
##   - `Customer Name`: Nomes não são úteis para análises estatísticas.
##   - `City`: Substituída por `City_Grouped`.

## 5. FILTRAGEM DE DADOS
## - Foram removidos valores iguais a zero em `Amount Spent (USD)`, pois o foco da análise é o comportamento de compra de consumidores que gastaram algo.

## 6. RESULTADO FINAL
## - A base final possui 285 observações e 12 variáveis, com categorias simplificadas e organizadas para análises futuras.
## - As variáveis foram agrupadas e recodificadas para facilitar a interpretação e a modelagem estatística.


## FIM DO RELATÓRIO DO ROTEIRO 01.




# roteiro 02 --------------------------------------------------------------
# Análise descritiva: Média e desvio padrão de Amount Spent por grupo
Social_Meida_Dataset %>%
  group_by(Gender) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Education_Level_Recoded) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Product_Category_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Influence_Level_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Social_Media_Platforms_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(City_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))







# Teste de Shapiro-Wilk para normalidade
shapiro.test(Social_Meida_Dataset$`Amount Spent (USD)`)

# Gráfico Q-Q para verificar normalidade
qqnorm(Social_Meida_Dataset$`Amount Spent (USD)`)
qqline(Social_Meida_Dataset$`Amount Spent (USD)`, col = "red")







# Teste de Levene para homogeneidade das variâncias
library(car)
leveneTest(`Amount Spent (USD)` ~ Gender, data = Social_Meida_Dataset)
leveneTest(`Amount Spent (USD)` ~ Education_Level_Recoded, data = Social_Meida_Dataset)
leveneTest(`Amount Spent (USD)` ~ Product_Category_Grouped, data = Social_Meida_Dataset)
leveneTest(`Amount Spent (USD)` ~ Influence_Level_Grouped, data = Social_Meida_Dataset)
leveneTest(`Amount Spent (USD)` ~ Social_Media_Platforms_Grouped, data = Social_Meida_Dataset)
leveneTest(`Amount Spent (USD)` ~ City_Grouped, data = Social_Meida_Dataset)








# ANOVA para Gender
anova_gender <- aov(`Amount Spent (USD)` ~ Gender, data = Social_Meida_Dataset)
summary(anova_gender)

# ANOVA para Education_Level_Recoded
anova_education <- aov(`Amount Spent (USD)` ~ Education_Level_Recoded, data = Social_Meida_Dataset)
summary(anova_education)

# ANOVA para Product_Category_Grouped
anova_product <- aov(`Amount Spent (USD)` ~ Product_Category_Grouped, data = Social_Meida_Dataset)
summary(anova_product)

# ANOVA para Influence_Level_Grouped
anova_influence <- aov(`Amount Spent (USD)` ~ Influence_Level_Grouped, data = Social_Meida_Dataset)
summary(anova_influence)

# ANOVA para Social_Media_Platforms_Grouped
anova_social_media <- aov(`Amount Spent (USD)` ~ Social_Media_Platforms_Grouped, data = Social_Meida_Dataset)
summary(anova_social_media)

# ANOVA para City_Grouped
anova_city <- aov(`Amount Spent (USD)` ~ City_Grouped, data = Social_Meida_Dataset)
summary(anova_city)
# Teste de Tukey HSD



TukeyHSD(anova_influence)

TukeyHSD(anova_city)





# Para variáveis com heterogeneidade (Gender, Education Level, Product Category, Social Media Platforms), VAMOS  usar testes não paramétricos (como Kruskal-Wallis)


kruskal.test(`Amount Spent (USD)` ~ Gender, data = Social_Meida_Dataset)
kruskal.test(`Amount Spent (USD)` ~ Education_Level_Recoded, data = Social_Meida_Dataset)
kruskal.test(`Amount Spent (USD)` ~ Product_Category_Grouped, data = Social_Meida_Dataset)
kruskal.test(`Amount Spent (USD)` ~ Social_Media_Platforms_Grouped, data = Social_Meida_Dataset)
skim(Social_Meida_Dataset)
qacBase::contents(Social_Meida_Dataset)
## RELATÓRIO DO ROTEIRO 02: ANOVA

## 1. ANÁLISE DESCRITIVA
## - Foram calculadas as médias e desvios padrão de `Amount Spent (USD)` para cada grupo das variáveis categóricas.
## - Destaques:
##   - Homens gastam mais (USD 270) do que mulheres (USD 109).
##   - Consumidores com "Graduação ou Ensino Médio" gastam mais (USD 237) do que os com "Pós-Graduação" (USD 133).
##   - A categoria "Saúde e Bem-Estar" tem o maior gasto médio (USD 322), seguida por "Eletrônicos e Casa" (USD 246) e "Beleza e Moda" (USD 65.8).
##   - Consumidores que usam "Outras Plataformas" gastam mais (USD 286) do que os que usam "Facebook e Instagram" (USD 158).

## 2. VERIFICAÇÃO DOS PRESSUPOSTOS
## - **Normalidade:** O teste de Shapiro-Wilk rejeitou a hipótese de normalidade (`p-value < 2.2e-16`), e o gráfico Q-Q confirmou a não normalidade.
## - **Homogeneidade das Variâncias:** O teste de Levene mostrou heterogeneidade para Gender, Education Level, Product Category e Social Media Platforms, mas homogeneidade para Influence Level e City Grouped.

## 3. RESULTADOS DA ANOVA
## - **Gender:** Diferença significativa (`p-value = 1.71e-07`). Homens gastam mais do que mulheres.
## - **Education Level:** Diferença significativa (`p-value = 0.000967`). "Graduação ou Ensino Médio" gasta mais do que "Pós-Graduação".
## - **Product Category:** Diferença significativa (`p-value = 1.27e-09`). "Eletrônicos e Casa" e "Saúde e Bem-Estar" gastam mais do que "Beleza e Moda".
## - **Influence Level:** Sem diferença significativa (`p-value = 0.872`).
## - **Social Media Platforms:** Diferença significativa (`p-value = 0.00024`). "Outras Plataformas" gastam mais do que "Facebook e Instagram".
## - **City Grouped:** Sem diferença significativa (`p-value = 0.497`).

## 4. TESTES NÃO PARAMÉTRICOS (KRUSKAL-WALLIS)
## - Para confirmar os resultados da ANOVA, foram realizados testes de Kruskal-Wallis:
##   - **Gender:** Diferença significativa (`p-value = 9.119e-07`).
##   - **Education Level:** Diferença significativa (`p-value = 2.045e-05`).
##   - **Product Category:** Diferença significativa (`p-value = 3.66e-12`).
##   - **Social Media Platforms:** Sem diferença significativa (`p-value = 0.08296`).

## 5. CONCLUSÕES
## - Variáveis com efeito significativo: Gender, Education Level, Product Category.
## - Variáveis sem efeito significativo: Influence Level, City Grouped, Social Media Platforms (no teste de Kruskal-Wallis).
## - Os pressupostos de normalidade e homogeneidade das variâncias foram violados para algumas variáveis, mas os testes não paramétricos confirmaram os resultados da ANOVA.

## FIM DO RELATÓRIO DO ROTEIRO 02.



# Roteiro 03 --------------------------------------------------------------
# Análise descritiva: Média e desvio padrão de Amount Spent por grupo
Social_Meida_Dataset %>%
  group_by(Gender) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Education_Level_Recoded) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Product_Category_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Influence_Level_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(Social_Media_Platforms_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))

Social_Meida_Dataset %>%
  group_by(City_Grouped) %>%
  summarise(Mean_Amount_Spent = mean(`Amount Spent (USD)`), SD_Amount_Spent = sd(`Amount Spent (USD)`))







# Gráficos de dispersão para verificar linearidade
ggplot(Social_Meida_Dataset, aes(x = `Income (USD)`, y = `Amount Spent (USD)`)) +
  geom_point() +
  theme_minimal()

ggplot(Social_Meida_Dataset, aes(x = `Social Media Usage (Hours/Day)`, y = `Amount Spent (USD)`)) +
  geom_point() +
  theme_minimal()
# Ajuste do modelo de regressão múltipla
modelo_regressao <- lm(`Amount Spent (USD)` ~ Gender + Education_Level_Recoded + Product_Category_Grouped + Influence_Level_Grouped + Social_Media_Platforms_Grouped + City_Grouped, data = Social_Meida_Dataset)

summary(modelo_regressao)




# Teste de Durbin-Watson para independência dos resíduos
library(lmtest)
dwtest(modelo_regressao)
# Teste de Breusch-Pagan para homocedasticidade
library(lmtest)
bptest(modelo_regressao)
# Extrair os resíduos do modelo e salvá-los em um objeto
residuos <- resid(modelo_regressao)

# Agora você pode realizar os testes
shapiro.test(residuos)

# E criar os gráficos Q-Q
qqnorm(residuos)
qqline(residuos, col = "red")






# Cálculo do VIF para verificar multicolinearidade
library(car)
vif(modelo_regressao)
# Configurar o layout 2x2
par(mfrow=c(2,2))

# Criar os gráficos diagnósticos
plot(modelo_regressao)

# Para retornar ao layout padrão depois (opcional)
par(mfrow=c(1,1))

# tentativa melhorar: -----------------------------------------------------
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Log_Amount_Spent = log(`Amount Spent (USD)` + 1))

# Ajustar o modelo com a variável transformada
modelo_regressao_log <- lm(Log_Amount_Spent ~ Gender + Education_Level_Recoded + Product_Category_Grouped + Influence_Level_Grouped + Social_Media_Platforms_Grouped + City_Grouped, data = Social_Meida_Dataset)
modelo_stepp <- stepAIC(modelo_regressao_log, direction = "both")
summary(modelo_stepp)
# Verificar os resultados
summary(modelo_regressao_log)
modelo_regressao_simplificado <- lm(`Amount Spent (USD)` ~ Gender + Education_Level_Recoded + Product_Category_Grouped + Influence_Level_Grouped + Social_Media_Platforms_Grouped, data = Social_Meida_Dataset)

# Verificar os resultados
summary(modelo_regressao_simplificado)
modelo_regressao_interacao <- lm(`Amount Spent (USD)` ~ Gender * Product_Category_Grouped + Education_Level_Recoded + Influence_Level_Grouped + Social_Media_Platforms_Grouped, data = Social_Meida_Dataset)

# Verificar os resultados
summary(modelo_regressao_interacao)



# Modelo com variáveis contínuas e transformação log
modelo_novo <- lm(Log_Amount_Spent ~ Gender + Education_Level_Recoded + 
                    Product_Category_Grouped + Influence_Level_Grouped + 
                    Social_Media_Platforms_Grouped + 
                    `Income (USD)` + I(`Income (USD)`^2) +
                    `Social Media Usage (Hours/Day)` + 
                    I(`Social Media Usage (Hours/Day)`^2),
                  data = Social_Meida_Dataset)
summary(modelo_novo)
# Seleção stepwise
library(MASS)
modelo_step <- stepAIC(modelo_novo, direction = "both")
summary(modelo_step)





















# Teste de Durbin-Watson para independência dos resíduos
library(lmtest)
dwtest(modelo_novo)
# Teste de Breusch-Pagan para homocedasticidade
library(lmtest)
bptest(modelo_novo)
# Extrair os resíduos do modelo e salvá-los em um objeto
residuos <- resid(modelo_novo)

# Agora você pode realizar os testes
shapiro.test(residuos)

# E criar os gráficos Q-Q
qqnorm(residuos)
qqline(residuos, col = "red")






# Cálculo do VIF para verificar multicolinearidade
library(car)
vif(modelo_novo)
# Configurar o layout 2x2
par(mfrow=c(2,2))

# Criar os gráficos diagnósticos
plot(modelo_novo)

# Para retornar ao layout padrão depois (opcional)
par(mfrow=c(1,1))











# comparação --------------------------------------------------------------

# Função para calcular métricas de comparação
compare_models <- function(models_list) {
  results <- data.frame(
    Model = names(models_list),
    R2 = numeric(length(models_list)),
    Adj_R2 = numeric(length(models_list)),
    AIC = numeric(length(models_list)),
    BIC = numeric(length(models_list)),
    RMSE = numeric(length(models_list)),
    Num_Variables = numeric(length(models_list))
  )
  
  for(i in seq_along(models_list)) {
    model <- models_list[[i]]
    summary_model <- summary(model)
    
    # Cálculo do RMSE
    rmse <- sqrt(mean(residuals(model)^2))
    
    results$R2[i] <- summary_model$r.squared
    results$Adj_R2[i] <- summary_model$adj.r.squared
    results$AIC[i] <- AIC(model)
    results$BIC[i] <- BIC(model)
    results$RMSE[i] <- rmse
    results$Num_Variables[i] <- length(coef(model)) - 1  # -1 para remover o intercepto
  }
  
  return(results)
}

# Criar lista com todos os modelos
models_list <- list(
  "Modelo_Original" = modelo_regressao,
  "Modelo_Log" = modelo_regressao_log,
  "Modelo_Simplificado" = modelo_regressao_simplificado,
  "Modelo_Interacao" = modelo_regressao_interacao,
  "Modelo_Novo" = modelo_novo,
  "Modelo_Step" = modelo_step
)

# Comparar os modelos
comparison_results <- compare_models(models_list)

# Ordenar resultados por R² ajustado
comparison_results_ordered <- comparison_results[order(-comparison_results$Adj_R2), ]

comparison_results_ordered


# roteiro 04 --------------------------------------------------------------
library(tidyverse)
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(Treatment = ifelse(`Social Media Usage (Hours/Day)` > 3, 1, 0))
library(tableone)
table1 <- CreateTableOne(vars = c("Age", "Gender", "Income (USD)", "Education_Level_Recoded"), 
                         strata = "Treatment", 
                         data = Social_Meida_Dataset, 
                         test = FALSE)
print(table1, smd = TRUE)
ps_model <- glm(Treatment ~ Age + Gender + `Income (USD)` + Education_Level_Recoded + 
                  Product_Category_Grouped + Influence_Level_Grouped, 
                family = binomial(), 
                data = Social_Meida_Dataset)
summary(ps_model)
library(MatchIt)
match_result <- matchit(Treatment ~ Age + Gender + `Income (USD)` + Education_Level_Recoded + 
                          Product_Category_Grouped + Influence_Level_Grouped, 
                        method = "nearest", 
                        data = Social_Meida_Dataset)
summary(match_result)







matched_data <- match.data(match_result)
table1_matched <- CreateTableOne(vars = c("Age", "Gender", "Income (USD)", "Education_Level_Recoded"), 
                                 strata = "Treatment", 
                                 data = matched_data, 
                                 test = FALSE)
print(table1_matched, smd = TRUE)




t.test(`Amount Spent (USD)` ~ Treatment, data = matched_data)
plot(match_result, type = "density")
plot(match_result, type = "hist")

# roteiro 05 --------------------------------------------------------------
# Carregando bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
# Para esta análise, vamos considerar:
# - Tratamento: Influência nas redes sociais (Muito Influente vs Pouco/Medianamente Influente)
# - Período: Vamos criar dois períodos baseados na mediana do uso de redes sociais
# - Resultado: Amount Spent (USD)

# 1. Preparar os dados para DiD
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(
    # Definir tratamento (1 = Muito Influente, 0 = Pouco/Medianamente Influente)
    treatment = ifelse(Influence_Level_Grouped == "Muito Influente", 1, 0),
    # Definir período (1 = Alto uso de redes sociais, 0 = Baixo uso)
    post = ifelse(`Social Media Usage (Hours/Day)` > median(`Social Media Usage (Hours/Day)`), 1, 0)
  )

# 2. Estatísticas descritivas por grupo e período
summary_stats <- Social_Meida_Dataset %>%
  group_by(treatment, post) %>%
  summarise(
    mean_amount = mean(`Amount Spent (USD)`),
    sd_amount = sd(`Amount Spent (USD)`),
    n = n()
  )

# 3. Modelo DiD básico
did_model <- lm(`Amount Spent (USD)` ~ treatment + post + treatment:post, 
                data = Social_Meida_Dataset)

# 4. Modelo DiD com controles
did_model_controls <- lm(`Amount Spent (USD)` ~ treatment + post + treatment:post +
                           Gender + Education_Level_Recoded + Product_Category_Grouped +
                           `Income (USD)` + City_Grouped,
                         data = Social_Meida_Dataset)

# 5. Teste de tendências paralelas
# Criar períodos anteriores simulados usando Social Media Usage
Social_Meida_Dataset <- Social_Meida_Dataset %>%
  mutate(
    time_period = cut(`Social Media Usage (Hours/Day)`, 
                      breaks = quantile(`Social Media Usage (Hours/Day)`, probs = c(0, 0.33, 0.66, 1)),
                      labels = c("Early", "Mid", "Late"))
  )

# 6. Visualização das tendências
ggplot(Social_Meida_Dataset, aes(x = `Social Media Usage (Hours/Day)`, 
                                 y = `Amount Spent (USD)`, 
                                 color = factor(treatment))) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Tendências de Gastos por Nível de Influência",
       x = "Uso de Redes Sociais (Horas/Dia)",
       y = "Gastos (USD)",
       color = "Nível de Influência")

# 7. Teste de robustez com erros padrão robustos
robust_se <- coeftest(did_model_controls, vcov = vcovHC(did_model_controls, type = "HC1"))

# 8. Análise placebo
# Criar tratamento placebo aleatório
set.seed(123)
Social_Meida_Dataset$placebo_treatment <- sample(c(0,1), 
                                                 size = nrow(Social_Meida_Dataset), 
                                                 replace = TRUE)

placebo_model <- lm(`Amount Spent (USD)` ~ placebo_treatment + post + 
                      placebo_treatment:post, 
                    data = Social_Meida_Dataset)
summary(placebo_model)







library(plm)
summary(did_model_controls)
summary(robust_se)
print(summary_stats)
