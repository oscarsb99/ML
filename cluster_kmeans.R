# ML
Machine Learning methods 
#### Cluster ####
#CLUSTERING K - MEANS para base de datos de tarjetas de crédito según el saldo, pagos y limite de crédito.
#Se seleccionan las variables de interés
base_selec <- base %>% select(balance, payments,credit_limit)
#Se remueven NA´s
base_selec <- base_selec[complete.cases(base_selec),]
#Algorithm K - means. Sin la primera columna porque es el ID
base_km <- kmeans(base_selec, centers = 4) 
#Numero de clusters optimo
clustOptimo <- kmeans(base_selec, centers = 1)$betweenss
#Este loop permite simular la distancia entre grupos si se agrupa de 2 a 10 clusters
for (i in 2:10){
  clustOptimo[i] <-kmeans(base_selec, centers = i)$betweenss
}
#Plot donde se elige el numero de clusters dependiendo de la distancia entre grupos
plot(clustOptimo, type = "b" ) #El resultado es 4 
#cluster asignado como un data frame
cluster_asignado <- as.data.frame(base_km$cluster)   
#Se unen las columnas
base_ClustersFinal <- cbind(base_selec,cluster_asignado)
#cambiar el nombre de la ultima columna
colnames(base_ClustersFinal)[4] <- "cluster_asignado"
#Inspeccion de resultados 
aggregate(base_ClustersFinal, by = list(base_ClustersFinal$cluster_asignado), FUN = mean)
#Grafico de comparacion entre la variable saldo y limite de crédito  
base_ClustersFinal %>% ggplot(mapping = aes(x = balance, y = credit_limit, colour = cluster_asignado)) +
  geom_point() + labs(title = "Límite de crédito y saldo de los usuarios",
                      color = "Cluster", subtitle = "El cluster segmenta en 4 a los usuarios") +
  theme_minimal() + scale_color_continuous(type = "viridis")

#Grafico de comparacion entre la variable limite de crédito y pago 
base_ClustersFinal %>% ggplot(mapping = aes(x = credit_limit, y = payments, color = cluster_asignado)) +
  geom_point() + labs(title = "Límite de crédito y pagos",
                      color = "Cluster", subtitle = "El cluster segmenta en 4 a los usuarios") +
  theme_minimal() + scale_color_continuous(type = "viridis")

#Grafico de comparacion entre la variable limite de crédito y pagos.
base_ClustersFinal %>% ggplot(mapping = aes(x = balance, y = payments, color = cluster_asignado)) +
  geom_point() + labs(title = "Saldo y Pagos",
                      color = "Cluster", subtitle = "El cluster segmenta en 4 a los usuarios") +
  theme_minimal()
