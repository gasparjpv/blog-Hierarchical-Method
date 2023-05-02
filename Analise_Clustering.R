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


#AGRUPANDO LANCHES PELO METODO NAO HIERARQUICO

#Rodar o modelo
mcdonalds.k2 <- kmeans(mcdonalds.padronizado, centers = 2)

#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = mcdonalds.padronizado, main = "Cluster K2")

#Criar clusters
mcdonalds.k3 <- kmeans(mcdonalds.padronizado, centers = 3)
mcdonalds.k4 <- kmeans(mcdonalds.padronizado, centers = 4)
mcdonalds.k5 <- kmeans(mcdonalds.padronizado, centers = 5)

#Criar graficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "wss")

