# Função para carga dos pacotes

pacotes <- c("tidyverse","cluster","dendextend","factoextra","fpc",
             "gridExtra","readxl")

#Descrição das livrarias usadas 

#tidyverse - pacote para manipulacao de dados
#cluster - algoritmo de cluster
#dendextend - compara dendogramas
#factoextra - algoritmo de cluster e visualizacao
#fpc - algoritmo de cluster e visualizacao
#gridExtra - para a funcao grid arrange
#readxl - função para carregar arquivo excel

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Agrupando os lanches pelo metodo hierarquico 

#Carregar base de dados: 
mcdonalds <- read.table("MCDONALDS.csv", sep = ";", dec = ",", header = T)

#transformar o nome dos lanches em linhas
rownames(mcdonalds) <- mcdonalds[,1]
mcdonalds <- mcdonalds[,-1]

#Padronizar variaveis
mcdonalds.padronizado <- scale(mcdonalds)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(mcdonalds.padronizado, method = "euclidean")

#Calcular o Cluster: metodos disponiveis "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "ward.D2" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#Criar o grafico e destacar os grupos
rect.hclust(cluster.hierarquico, k = 2)

#VERIFICANDO ELBOW 
#No método usamos wss, que significa: "para o total dentro da soma do quadrado"
fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "wss")

#criando 4 grupos de lanches
grupo_lanches4 <- cutree(cluster.hierarquico, k = 4)
table(grupo_lanches4)

#transformando em data frame a saida do cluster
Lanches_grupos <- data.frame(grupo_lanches4)

#juntando com a base original
Base_lanches_fim <- cbind(mcdonalds, Lanches_grupos)

# Adicione uma coluna com o nome desejado antes da tabela
Base_lanches_fim <- cbind(Lanches = rownames(Base_lanches_fim), Base_lanches_fim)

# Salve o arquivo CSV com o nome da primeira coluna definido como "Nome"
write.table(Base_lanches_fim, file = "Base_Final.csv", sep = ";", row.names = FALSE, quote = FALSE, dec = ",", col.names = TRUE)



#FAZENDO ANALISE DESCRITIVA

#MEDIAS das variaveis por grupo
mediagrupo <- Base_lanches_fim %>% 
  group_by(grupo_lanches4) %>% 
  summarise(n = n(),
            Valor.Energetico = mean(Valor.Energetico), 
            Carboidratos = mean(Carboidratos), 
            Proteinas = mean(Proteinas),
            Gorduras.Totais = mean(Gorduras.Totais), 
            Gorduras.Saturadas = mean(Gorduras.Saturadas), 
            Gorduras.Trans = mean(Gorduras.Trans),
            Colesterol = mean(Colesterol), 
            Fibra.Alimentar = mean(Fibra.Alimentar), 
            Sodio = mean(Sodio),
            Calcio = mean(Calcio), 
            Ferro = mean(Ferro) )
mediagrupo