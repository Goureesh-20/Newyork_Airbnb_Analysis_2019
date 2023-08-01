library(tidyverse)
library(dplyr)
library(gridExtra)

#  Define summarize_numeric
summarize_numeric = function(dataset) {
  
  dataset = select_if(dataset, is.numeric)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
           'Mean' = colMeans(dataset, na.rm = TRUE),
           'Min' = apply(dataset, 2, function (x) min(x, na.rm = TRUE)),
           'Max' = apply(dataset, 2, function (x) max(x, na.rm = TRUE)),
           'SD' = apply(dataset, 2, function (x) sd(x, na.rm = TRUE))
    )
  summary.table
}

#  Define summarize_character function 
summarize_character = function(dataset) {
  
  dataset = select_if(dataset, is.character)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
    )
  summary.table
  
}

#  Define summarize_factor function
summarize_factor = function(dataset) {
  
  dataset = select_if(dataset, is.factor)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
           'Mode' = apply(dataset, 2, function (x) mode(x)),
    )
  summary.table
}

#  Initial dataset summary and conversion of attributes to appropriate types (numerics and classes)
#setwd('C:/Users/Anastasia/Documents/Anastasia/USC/ISE_535DataMining/HW/final_project')
airbnb = read_csv('Dataset 2 - New York City Air B&B.csv')
summarize_numeric(airbnb)
summarize_character(airbnb)

#  Convert numeric attributes to factors
airbnb$host_id = as.factor(airbnb$host_id)
airbnb$calculated_host_listings_count = as.factor(airbnb$calculated_host_listings_count)

#  Convert character attributes to factors
airbnb$name = as_factor(airbnb$name)
airbnb$neighbourhood_group = as_factor(airbnb$neighbourhood_group)
airbnb$neighbourhood = as_factor(airbnb$neighbourhood)
airbnb$room_type = as_factor(airbnb$room_type)
summarize_factor(airbnb)
sum(is.na(airbnb$last_review))

#Calculate the difference in days for last_review from today
install.packages("discard")
install.packages("col_factor")
library(scales)
#Assume current date is Oct 21, 2022 for the consistency
airbnb$last_review_in_days = vector = as.integer(difftime('2022-10-21',airbnb$last_review, units = "days"))
normalized_review_in_days = 1/airbnb$last_review_in_days
normalized_review_in_days = rescale(normalized_review_in_days, to = c(0.5, 1.5))

#Calculate review performance
airbnb$review_performance = as.integer(airbnb$number_of_reviews * normalized_review_in_days)
airbnb$review_performance[is.na(airbnb$review_performance)] <- airbnb$number_of_reviews[is.na(airbnb$review_performance)] 
#print(airbnb)
sum(is.na(airbnb$review_performance))

#Drop missing value rows for name and host_name
airbnb = airbnb[!is.na(airbnb$name), ]
airbnb = airbnb[!is.na(airbnb$host_name), ]

#Drop outlier
airbnb = airbnb%>%filter(reviews_per_month<22)
airbnb = airbnb%>%filter(minimum_nights<400)

#Create a response variable: price*availiability
airbnb$profit <- airbnb$price * airbnb$availability_365
airbnb2 <- airbnb[,c('latitude','longitude','room_type','reviews_per_month','review_performance','minimum_nights','neighbourhood','neighbourhood_group','calculated_host_listings_count','profit')]
airbnb3 <- airbnb2
write.csv(airbnb3,"airbnb_selected.csv",row.names = FALSE)

airbnb2$neighbourhood <- as.numeric(airbnb2$neighbourhood)
airbnb2$neighbourhood_group <- as.numeric(airbnb2$neighbourhood_group)
airbnb2$room_type <- as.numeric(airbnb2$room_type)
airbnb2$calculated_host_listings_count <- as.numeric(airbnb2$calculated_host_listings_count)

airbnb2 <- select(airbnb2,-c('price'))



# PCA
pca <- prcomp(airbnb2,center = TRUE,scale. = TRUE)
summary(pca)
plot.pc <- plot(pca, type="l")

biplot(pca)
pca$rotation

#install.packages("factoextra")
#install.packages("cluster")
library(factoextra)
library(cluster)
'''
data_scaled = scale(airbnb2 %>% select_if(is.numeric))

totwss = tibble(num_clusters = 1:10, tot_withinss = 0)
for (i in 1:10) {
  km = kmeans(data_scaled, i, nstart=10)
  totwss$tot_withinss[i] = km$tot.withinss
}
ggplot(totwss, aes(x = num_clusters, y=tot_withinss)) + geom_line() + geom_point()

fviz_nbclust(airbnb2, kmeans, method = "wss")


gap_stat <- clusGap(airbnb2,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
#fviz_gap_stat(gap_stat)
'''

