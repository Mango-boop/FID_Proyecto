# ---
# title: "Analisis de data breaches en los ultimos años. Unsupervised ML: Clustering"
# 
# author: "Dubovik Aleksandra"
# ---
#   
setwd('/Users/alexandradubovik/FID_Proyecto')
data <- read.csv("data_processed.csv", header = TRUE)

data.labels = data$Organization.type
table(data.labels)
head(data)
data_ <- data[4:5]

install.packages("factoextra")
library(factoextra)
# Library required for fviz_cluster function
install.packages("factoextra")
library(factoextra)

# Scale data
data_scaled <- scale(data_)
fviz_nbclust(data_scaled, kmeans, method = "wss") + labs(subtitle="elbow method")


km.out <- kmeans(data_scaled, centers = 4, nstart  = 100)
km.clusters <- km.out$cluster
rownames(data_scaled) <- data$Organization.type
fviz_cluster(list(data=data_scaled, cluster = km.clusters))

