setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(randomForest)
#read in data. longform. need a column for the random effect/grouping var
#in this data treatment/plot/pellet etc are basically the same grouping var
#so it doesnt matter. Because there is only one emf and one treatment and one 
#control plot at the site.

#read data.This is all data recorded, each group is a different size, but that
#doesnt matter for this analysis
data = read_excel('../../data/TilhillWorkingCopy.xlsx', sheet = 'GEEdata')
data = data %>% drop_na(height)


data <- data %>%
  mutate(new_treatment = ifelse(treatment == "Pellet", 1, 0))



rf_model <- randomForest(height ~ new_treatment*time, data = data)

# View the model summary
print(rf_model)

# Check the importance of each predictor
importance(rf_model)

randomForest::partialPlot(rf_model, as.data.frame(data), x.var = "new_treatment")


#kmeans??
data.kmeans =  data %>% select(c(height, time, new_treatment))

set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data.kmeans, centers = 6)

data.kmeans$cluster <- kmeans_result$cluster

# Check the distribution of treatment across clusters
table(data.kmeans$new_treatment, data.kmeans$cluster)

library(ggplot2)
ggplot(data.kmeans, aes(x = factor(cluster), fill = factor(new_treatment))) +
  geom_bar(position = "dodge") +
  labs(title = "Treatment Distribution Across Clusters",
       x = "Cluster",
       fill = "Treatment")

chisq_test <- chisq.test(table(data.kmeans$new_treatment, data.kmeans$cluster))
print(chisq_test)

pca <- prcomp(data.kmeans, center = TRUE, scale = TRUE)

# Create a data frame with PCA results
pca_data <- data.frame(pca$x, cluster = data.kmeans$cluster, treatment = data.kmeans$new_treatment)

# Plot PCA results
ggplot(pca_data, aes(x = PC1, y = PC2, color = factor(cluster), shape = factor(treatment))) +
  geom_point() +
  labs(title = "PCA of Data with Treatment and Clusters",
       x = "Principal Component 1", y = "Principal Component 2")
