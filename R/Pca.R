cla.pca <- prcomp(x , center = TRUE,scale. = TRUE)

summary(cla.pca)

pc1 <- cla.pca$rotation[,1]
pc2 <- cla.pca$rotation[,2]
pc3 <- cla.pca$rotation[,3]
housepc <- rbind(pc1,pc2,pc3)
median_house_value <- rbind(pc1,pc2,pc3) [,7]
house_pc <- rbind(pc1,pc2,pc3,median_house_value)

show(house_pc)
