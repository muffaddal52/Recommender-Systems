library("rjson")
library(dplyr)
library(ggplot2)
library(ndjson)
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






