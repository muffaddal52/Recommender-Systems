library("rjson")
library(dplyr)
library(ggplot2)
library(reshape2)
library(ndjson)
library(recommenderlab)

library("ndjson")

library("data.table")
library("parallel")
library("jsonlite")

file="C:\\Users\\muffaddal.qutbuddin\\Desktop\\Musical_Instruments_5.json"

df<-stream_in(file(file))

ratings <- df[,c('reviewerID','asin','overall','summary','reviewTime')]
sample(unique(df$asin),400)->randomProducts
subset(ratings,asin%in%randomProducts) -> ratingSample

nrow(ratingSample)
length(unique(ratingSample$reviewerID))
length(unique(ratingSample$asin))




ratingSample %>% group_by(reviewerID) %>% summarise(n=n()) %>% summarise(mean=mean(n), median = median(n))

ratingSample %>% group_by(reviewerID) %>% summarise(n=n()) %>%
  ggplot()+
  geom_boxplot(aes(y=n))

ratingSample %>% group_by(reviewerID) %>% summarise(n=n()) %>%
  ggplot()+
  geom_histogram(aes(n),binwidth = 1)

ratingSample %>% group_by(asin) %>% summarise(n=n()) %>% summarise(mean=mean(n), median = median(n))

ratingSample %>% group_by(asin) %>% summarise(n=n()) %>%
  ggplot()+
  geom_boxplot(aes(y=n))

ratingSample %>% group_by(asin) %>% summarise(n=n()) %>%
  ggplot()+
  geom_histogram(aes(n),binwidth = 1)



ratingFinal <- ratingSample[,c('reviewerID','asin','overall')]
colnames(ratingFinal) <- c('user_id','product_id','rating')
ratingFinal %>% View


ratingFinal %>% group_by(user_id) %>% summarise(meanRating = mean(rating)) %>%
  ggplot()+
  geom_histogram(aes(meanRating),binwidth = 1)+
  geom_vline(aes(xintercept=mean(meanRating)), color="red")


#check is people who rate more tend to score high or low
ratingFinal %>% group_by(user_id) %>% summarise(meanRating = mean(rating),ratingCount=n()) %>%
  mutate(Rcut = cut(ratingCount, c(0,5,10,15,20,25,30,35,40,45,50,100,200))) %>%
  group_by(Rcut) %>% summarise(averageRating = mean(meanRating),medianRating = median(meanRating),totalRating = sum(ratingCount)) %>% View



#convert to rating matric
as.matrix(acast(ratingFinal, user_id~ product_id)) -> ratingMatrix
r <- as(ratingMatrix, "realRatingMatrix")


# This function shows what the sparse matrix looks like.
getRatingMatrix(r[c(1:5),c(1:8)])

# Histogram of getRatings using Normalized Scores
hist(getRatings(normalize(r)), breaks=10, xlim = c(-2,2), main = "Normalized Scores Histogram")

hist(getRatings(normalize(r, method="Z-score")), breaks = 10, xlim = c(-2,2), main = "Z-score Histogram")

# Let's calculate the average_ratings_per_user and visualize the distribution
average_ratings_per_user <- rowMeans(r)
ggplot()+
  geom_histogram(aes(x=average_ratings_per_user),binwidth = .5)


#A very strange looking distribution indeed. This is likely secondary to rating bias.

# We will look at how many foods each user has rated and what the mean rating for each food is.

hist(rowCounts(r), breaks = 50, xlim = c(0,20), main = "Histogram of Number of ratings per User", xlab = "Reviews per User", col = "blue")


hist(colMeans(r), breaks = 20, main = "Frequency of mean ratings per item", col = "lightgreen")


image(r[1:1000, 1:400], main = "Heatmap of the first rows and columns")


which_train <- sample(x = c(TRUE, FALSE), size = nrow(r), replace = TRUE, prob = c(0.8, 0.2))

# Define the training and the test sets
recc_data_train <- r[which_train, ]
recc_data_test <- r[!which_train, ]

# Let's build the recommender IBCF - cosine:
recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))

# We have now created a IBCF Recommender Model

