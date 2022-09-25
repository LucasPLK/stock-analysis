library(tidyr)
library(dplyr)
library(reshape2)
library(factoextra) # visualização de clusters
library(dbscan)
library(cluster)
source("functions.R") # minhas funções definidas em um arquivo a parte

# Carregamento dos dados
df <- read.csv('../data/data.csv')

## Limpeza do dataset
df[df == '-'] <- NA
df[df == '-%'] <- NA

for(i in 1:ncol(df)) {
  # remoção dos simbolos de percentual
  df[ , i] <- gsub('%', '', df[ , i])
  df[ , i] <- gsub(',', '.', df[ , i])

}

# faz o trim dos textos
df <- data.frame(lapply(df, trimws), stringsAsFactors = FALSE)

# transforma celulas vazias em NA
df[df == ''] <- NA


# df <- transform(df, ATUAL = as.numeric(as.character(ATUAL)))
# df <- transform(df, X2021 = as.numeric(as.character(X2021)))
# df <- transform(df, X2020 = as.numeric(as.character(X2020)))
# df <- transform(df, X2019 = as.numeric(as.character(X2019)))
# df <- transform(df, X2018 = as.numeric(as.character(X2018)))
# df <- transform(df, X2017 = as.numeric(as.character(X2017)))
# df <- transform(df, X2016 = as.numeric(as.character(X2016)))
# df <- transform(df, X2015 = as.numeric(as.character(X2015)))
# df <- transform(df, X2014 = as.numeric(as.character(X2014)))
# df <- transform(df, X2013 = as.numeric(as.character(X2013)))

i <-c(4:13)
df[ , i] <- apply(df[ , i], 
                  2, # 2 significa operar sobre colunas, 1 operar sobre as linhas     
                  function(x) as.numeric(as.character(x)))

sapply(df, class) # verificando o tipo de cada coluna 

#df <- df %>% drop_na() # dropa nans quando tem ao menos 1 nan na linha

# sum(is.na(df[df$Indicador == 'LPA', 'X2020'])) # verificando NANs em uma única coluna

View(df[-100:-1, ])


# verifico a quantidade de nulos que existem por indicar em cada ano
df_qtd_nulos = check_nans_by_indicator_and_year(df, 
                                      unique(df$Indicador), 
                                      c('ATUAL', 'X2021', 'X2020', 
                                        'X2019', 'X2018',
                                        'X2017', 'X2016', 'X2015', 
                                        'X2014', 'X2013')
                                      )

#Agrupo os dados por ano e verifico em qual ano tenho mais nulos
View(df_qtd_nulos %>% group_by(anos) %>% 
  summarise(TotalNulos = sum(qtd_nulos), TotalColuna=sum(qtd_total)) %>% 
  arrange(desc(TotalNulos)))


#Agrupo os dados por indicador e verifico em qual ano tenho mais nulos
View(df_qtd_nulos %>% group_by(indicador) %>% 
       summarise(TotalNulos = sum(qtd_nulos), TotalColuna=sum(qtd_total)) %>% 
       arrange(desc(TotalNulos)))

#pivot table
df_pivot = dcast(df, Ticker + Setor ~ Indicador, value.var="ATUAL", fun.aggregate = sum)

head(df_pivot)


df_pivot_filtered <- df_pivot %>% select(where(~mean(is.na(.)) < 0.1))

ncol(df_pivot)
ncol(df_pivot_filtered)
View(df_pivot_filtered)


# Faço o fill na com a média dos dados do segmento:
# df_pivot_filtered$LPA %>% replace_na(0)

# for(i in 3:ncol(df_pivot_filtered)){
#   df_pivot_filtered[is.na(df_pivot_filtered[,i]), i] <- mean(df_pivot_filtered[,i], na.rm = TRUE)
# }

df_pivot_filtered <-df_pivot_filtered %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))    

write.csv(df_pivot_filtered, '../data/data_cleaned_fillna.csv')

View(df_pivot_filtered)

df_pivot_filtered_scaled <- as.data.frame(scale(df_pivot_filtered[, c(3:ncol(df_pivot_filtered))]))

# https://www.projectpro.io/recipes/do-dbscan-clustering-r
# install.packages('dbscan')

data_for_cluster <- df_pivot_filtered %>% select('LPA', 'P/L')


d <- dbscan::dbscan(data_for_cluster, eps = 1.8, MinPts =  2)
fviz_cluster(d, data_for_cluster, geom = "point")
