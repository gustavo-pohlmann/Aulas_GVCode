library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(dplyr) # for data management

seg <- read.csv("C:/Users/Pohlmann/Documents/FGV/GVCode/Aulas2sem/Comp_Dummmies.csv",header=TRUE)
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
segNorm <- as.data.frame(lapply(seg,normalize))

cols <- colnames(segNorm)
cols <- cols[-c(1,5)] # 1 é o ID, que não interessa de qualquer maneira, e 5 é renda familiar, que não é categórico
segNorm[cols] <- lapply(segNorm[cols], as.factor) # lapply aplica uma função a um objeto várias vezes

segNorm$ID <- NULL # remove ID, porque não adiciona nada

# Calcula as distâncias entre os pontos pela métrica de Gower
# ?daisy # Para saber tudo o que a função daisy faz
gower_dist <- daisy(segNorm,
                    metric="gower") # Calcula a distância entre os pontos do objeto pelo método de Gower
summary(gower_dist) # Para ver se deu certo
gower_mat <- as.matrix(gower_dist) # Transforma o objeto em uma matriz para parsear e clusterizar
segNorm[which(gower_mat==min(gower_mat[gower_mat!=min(gower_mat)]),arr.ind = TRUE)[1, ],] #most similar pair
segNorm[which(gower_mat==max(gower_mat[gower_mat!=max(gower_mat)]),arr.ind = TRUE)[1, ],] #most dissimilar pair
sil_width <- c(NA) # para o gráfico que diz o número ideal de clusters

# particiona as distâncias pelo método PAM, Particionamento Em torno de Medoides
for(i in 2:15){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width # atualizar o gráfico do número ideal de clusters
  
}
plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width) # O pico do gráfico representa a quantidade ideal, que minimiza o erro

pam_fit <- pam(gower_dist, diss = TRUE, k = 3) # k=3 porque o nÃºmero ideal foi 3
pam_results <- segNorm %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.)) # finalmente termina o particionamento dos dados
pam_results$the_summary
med <- segNorm[pam_fit$medoids,] # cria um objeto mais permanente para ver os medoides

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE) # Resume as dimensões do problema em um objeto tsne de 2 dimensões

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)) # trabalha os dados do objeto tsne

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) + ggtitle("P.A.M.") # faz os gráficos

# Comparando com K-médias. Mesma mÃ©trica de distÃância e fator de redução de dimensões
kmn <- kmeans(gower_mat,centers = 3)
kmn$cluster <- as.factor(kmn$cluster)

tsne_data_k <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kmn$cluster))
ggplot(aes(x = X, y = Y), data = tsne_data_k) +
  geom_point(aes(color = cluster)) + ggtitle("K-Means")

# Comparando com Clustering Histográfico
clust <- hclust(gower_dist, method="ward.D2")
plot(clust)
groups <- cutree(clust, k=3) # corta para separar os clusters no número definido acima
rect.hclust(clust, k=3, border="red")

# Para ver os tamanhos dos clusters em cada mÃ©todo
table(pam_fit$clustering)
table(kmn$cluster)
table(groups)

# Por que não usar distância Euclidiana ou Manhattan para separar os dados?
# Dados categóricos complicam as análises de distância
# Com dados mistos a função daisy automaticamente roda a distância de Gower
mnht_dist <- daisy(segNorm[,-1], metric="manhattan")
mnht_mat <- as.matrix(mnht_dist)
kmn_m <- kmeans(mnht_mat,centers = 3)
kmn_m$cluster <- as.factor(kmn_m$cluster)

tsne_obj_m <- Rtsne(mnht_dist, is_distance = TRUE)

tsne_data_m <- tsne_obj_m$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kmn_m$cluster))
ggplot(aes(x = X, y = Y), data = tsne_data_m) +
  geom_point(aes(color = cluster)) + ggtitle("Manhattan")

eucl_dist <- daisy(segNorm[,-1], metric="euclidean")
eucl_mat <- as.matrix(eucl_dist)
kmn_e <- kmeans(eucl_mat,centers = 3)
kmn_e$cluster <- as.factor(kmn_e$cluster)

tsne_obj_e <- Rtsne(mnht_dist, is_distance = TRUE)

tsne_data_e <- tsne_obj_e$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kmn_e$cluster))
ggplot(aes(x = X, y = Y), data = tsne_data_e) +
  geom_point(aes(color = cluster)) + ggtitle("Euclidean")

# Comparando diferentes métodos de projeção em 2D
library(ggfortify)
pca_obj <- prcomp(gower_dist, center=TRUE, scale. = TRUE)
plot(pca_obj,type="l")
autoplot(kmn_e, data=eucl_dist,frame=TRUE,frame.type='norm') +ggtitle("Euclidean")
autoplot(kmn_m,data=mnht_dist,frame=TRUE,frame.type='norm') +ggtitle("Manhattan")
autoplot(kmn,data=gower_dist, frame=TRUE,frame.type='norm') +ggtitle("Gower")

# TSNE: https://lvdmaaten.github.io/tsne/
# PCA: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
# PCA tambÃ©m: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
# Fonte dos mÃ©todos: https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
# Fonte adicional: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4939904/