library(readxl)
library(forcats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(caret)
library(factoextra)
library(cluster)

# Read in the Excel file and store the resulting dataset in a variable
whitewine_initial <- read_excel("C:/IIT assi/Whitewine_v2.xlsx")

#Structure of the dataset
str(whitewine_initial)

#Try to finding Near Zero/Missing Variance
print(sum(is.na(whitewine_initial))) # There are no missing values

#Co-relation matrix
whitewine_initial.features <- whitewine_initial
whitewine_initial.features$quality <- NULL
cor(whitewine_initial.features)

#Principal Component Analysis (PCA)
prc <- prcomp(whitewine_initial.features, scale = TRUE)
summary(prc)

# Variances for each principal component
plot(prc, type = "lines", main="Variances for Principal Component")

# Clean up the variable names using the clean_names() functionz
whitewine_initial <- janitor::clean_names(whitewine_initial)

# Create the class variable using mutate()
whitewine_initial <- whitewine_initial %>%
  mutate(class = as_factor(quality))

# Check the variable names in Whitewine_data
names(whitewine_initial)

# Get an overview of what the dataset looks like from a high-level perspective.
summary(whitewine_initial)

whitewine_new <- whitewine_initial %>% mutate(class = as_factor(
  case_when(
    quality == 5 ~ 5,
    quality == 6 ~ 6,
    quality == 7 ~ 7,
    quality == 8 ~ 8)
))
summary(whitewine_new)

#Outlier Detection

whitewine_new %>%
  pivot_longer(1:12, names_to = 'labels') %>%
  filter(class == 8) %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class:'8'")

whitewine_new %>%
  pivot_longer(1:12, names_to = 'labels') %>%
  filter(class == 7) %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class:'7'")

whitewine_new %>%
  pivot_longer(1:12, names_to = 'labels') %>%
  filter(class == 6) %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class:'6'")

whitewine_new %>%
  pivot_longer(1:12, names_to = 'labels') %>%
  filter(class == 5) %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class:'5'")

quality_8 = whitewine_new %>%
  filter(class == 8) %>%
  mutate(across(1:12, ~squish(.x, quantile(.x,c(.05, .95)))))
quality_7 = whitewine_new %>%
  filter(class == 7) %>%
  mutate(across(1:12, ~squish(.x, quantile(.x,c(.05, .95)))))
quality_6 = whitewine_new %>%
  filter(class == 6) %>%
  mutate(across(1:12, ~squish(.x, quantile(.x,c(.05, .95)))))
quality_5 = whitewine_new %>%
  filter(class == 5) %>%
  mutate(across(1:12, ~squish(.x, quantile(.x,c(.05, .95)))))

combined = bind_rows(list(quality_8, quality_7, quality_6, quality_5))
print(combined)

combined %>%
  pivot_longer(1:12,names_to = "labels") %>%
  filter(class == 8) %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: '8'")

combined %>%
  pivot_longer(1:12,names_to = "labels") %>%
  filter(class == 7) %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: '7'")

combined %>%
  pivot_longer(1:12,names_to = "labels") %>%
  filter(class == 6) %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: '6'")

combined %>%
  pivot_longer(1:12,names_to = "labels") %>%
  filter(class == 5) %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: '5'")

# Remove the quality and the class name. Both of these will be remove so that only 
# numerical data is left for the algorithm

# Define the number of cluster centres
whitewine_data_points =  combined %>%
  select(-quality, -class)

# Now that we have the "whitewine_data_points" dataset, scaling is performed
whitewine_scaled = whitewine_data_points %>%
  mutate(across(everything(), scale))

set.seed(1234)

# Perform the kmeans using the NbClust function
# Use Euclidean for distance
cluster_euclidean = NbClust(whitewine_scaled,distance="euclidean",
                            min.nc=2,max.nc=10,method="kmeans",index="all")
# Use manhattan for distance
cluster_manhattan = NbClust(whitewine_scaled,distance="manhattan",
                            min.nc=2,max.nc=15,method="kmeans",index="all")

summary(whitewine_scaled)

# Finding the Optimal number of clusters using was
# function that computes total within-cluster sum of square
fn_kemans_clust <- function(data, cluster_count) {
  kmeans(data, cluster_count, iter.max = 300, nstart = 7)
}

# Use elbow method to find optimal number of clusters
wcss <- vector()
arr_clusters <- 1: 15

for (i in arr_clusters) wcss[i] <- sum(fn_kemans_clust(whitewine_scaled, i) $withinss)

plot(arr_clusters, wcss, type ="b",
     main="Elbow Method",
     xlab="Number of Clusters",
     ylab="WCSS")

# Use elbow Silhouette to find optimal number of clusters
avg_sils <- vector()
arr_clusters <- 2: 15

fn_avg_sil <- function(data_matrix, cluster_count) {
  k.temp <- fn_kemans_clust(data_matrix, cluster_count)
  sil_values <- silhouette(k.temp$cluster, dist(whitewine_scaled))
  mean(sil_values[,3])
}
for (i in arr_clusters) avg_sils[i - 1] <- fn_avg_sil(whitewine_scaled, i)

plot(arr_clusters, avg_sils, type ="b",
     main="Silhouette Method",
     xlab = "Number of clusters K",
     ylab="Average Silhouettes")

# kmeans results
result_c2<-kmeans(whitewine_scaled, 2)
result_c3<-kmeans(whitewine_scaled, 3) 
result_c4<-kmeans(whitewine_scaled, 4) 
result_c6<-kmeans(whitewine_scaled, 6) 
result_c7<-kmeans(whitewine_scaled, 7) 
result_c8<-kmeans(whitewine_scaled, 8) 
result_c10<-kmeans(whitewine_scaled, 10)

# Confusion matrix for k-means with 2 cluster
table(whitewine_new$class, result_c2$cluster)

# Confusion matrix for k-means with 3 cluster
table(whitewine_new$class, result_c3$cluster) 

# Confusion matrix for k-means with 4 cluster
table(whitewine_new$class, result_c4$cluster)

# Confusion matrix for k-means with 6 cluster
table(whitewine_new$class, result_c6$cluster)

# Confusion matrix for k-means with 7 cluster
table(whitewine_new$class, result_c7$cluster) 

# Confusion matrix for k-means with 8 cluster
table(whitewine_new$class, result_c8$cluster)

# Confusion matrix for k-means with 10 cluster
table(whitewine_new$class, result_c10$cluster)

#calculate means of 2 clusters
result_data_points <- kmeans(whitewine_data_points,2)
table(whitewine_new$class, result_data_points$cluster)

#confusion matrix function
confusion_matrix <- function(k_data) {
  whitewineNcluster <- cbind(whitewine_new, cluster = k_data$cluster)
  whitewineNcluster_df <- union(whitewineNcluster$cluster, whitewineNcluster$class)
  #get confusion matrix table
  whitewineNcluster_df_table <- table(factor(whitewineNcluster$cluster, whitewineNcluster_df),
                                      factor(whitewineNcluster$quality, whitewineNcluster_df)) 
  confusionMatrix(whitewineNcluster_df_table)
}

confusion_matrix(result_data_points)

result_data_points

result_data_points$centers

#PLot cluster
fviz_cluster(result_data_points, whitewine_scaled)

result_data_points

#Plot next best 2 cluster
fviz_cluster(result_c3, whitewine_scaled)

#Plot next best 3 cluster
fviz_cluster(result_c4, whitewine_scaled)


fviz_cluster(result_c2, whitewine_scaled)
fviz_cluster(result_c3, whitewine_scaled)
