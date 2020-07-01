library(dplyr)
library(ggplot2)
library(recosystem)
library(plotly)
library(recommenderlab)

library(tidyverse)

ratings.full<-ratings
ratings<-ratings.full[1:5000,]


ratings$user_id %>% unique() %>% length()
#53k users

ratings %>% group_by(user_id) %>% 
  summarise(totalRatings = n(),
            averageRating = mean(rating),
            medianRating = median(rating)) %>%
  mutate(ratingBucket = cut(totalRatings,15)) %>%
  ggplot()+
  geom_bar(aes(x=ratingBucket))
#35k users gave less than 15 rating.


ratings %>% group_by(user_id) %>% 
  summarise(totalRatings = n(),
            averageRating = mean(rating),
            medianRating = median(rating)) %>%
  summary()
#majority of the users only gave lest than 8 ratings with average rating of around 4

ratings %>% group_by(user_id) %>% 
  summarise(totalRatings = n(),
            averageRating = mean(rating),
            medianRating = median(rating)) %>%
  mutate(ratingBucket = cut(totalRatings,20)) %>%
  ggplot()+
  geom_jitter(aes(x=ratingBucket,y=averageRating,color='blue'),alpha=0.2)+
  geom_boxplot(aes(x=ratingBucket,y=averageRating),fill="bisque",color="black",alpha=0.3)

# two points to note here
#first is that all the buckets have average of aroung 4 ratings
#seccond only people who rated less have rated low so 1 to 21 binned has low ratings while higer binned don't have 


ggplotly(ratings %>% left_join(books, by="book_id") %>% group_by(goodreads_title) %>% 
  summarise(totalRatings = n(),
            averageRating = mean(rating),
            medianRating = median(rating)) %>%
    mutate(ratingBucket = cut(totalRatings, 10)) %>% 
  ggplot()+
  geom_bar(aes(x=ratingBucket,y=totalRatings),stat = "identity")
)

#majority of the book got between 90 and 120 ratings. only 2 3 moview got more than 150 ratings and 1 movie got 400ratins

ratings %>% left_join(books, by="book_id") %>% group_by(goodreads_title) %>% 
  summarise(totalRatings = n(),
            averageRating = mean(rating),
            medianRating = median(rating)) %>%
  summary()





ggplotly(ratings %>% left_join(books, by="book_id") %>% group_by(goodreads_title) %>% 
           summarise(totalRatings = n(),
                     averageRating = mean(rating),
                     medianRating = median(rating)) %>%
           mutate(ratingBucket = cut(totalRatings, 20)) %>% 
           ggplot()+
           geom_jitter(aes(x=ratingBucket,y=averageRating,color='blue'),alpha=0.2)+
           geom_boxplot(aes(x=ratingBucket,y=averageRating),fill="bisque",color="black",alpha=0.3)
         )
## regardles of how much ratings books recieved there average rating remains around 4


train_set <- data_memory(user_index = ratings$user_id, 
                         item_index = ratings$book_id, 
                         rating = ratings$rating)

r = Reco()

opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.01, 0.02),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 4, niter = 40))
opts$min

r$train(train_set, opts = opts$min)

#########################
user = 951:953
movie = 1:20
pred = expand.grid(user = user, book_id =  books$book_id)
test_set = data_memory(pred$user, pred$book_id, rating = NULL)


#test_set = data_memory(rep(c(user),nrow(books)), books$book_id, rating = NULL)
pred<- pred %>% left_join(books,by = c('book_id'))

pred$rating = r$predict(test_set, out_memory())

#########################



#########################
ratings %>% arrange((user_id)) %>% View

userId_1 <-951           
nrOfRecommendations<- 5

test_set  =  data_memory(rep(c(userId_1),nrow(books)), books$book_id, rating = NULL)


pred = expand.grid(user = userId_1, book_id = books$book_id)
pred<- pred %>% left_join(books,by = c('book_id'))


pred$rating = r$predict(test_set, out_memory())

r$output()

#########################

ratings%>% left_join(books,by = c('book_id')) %>% View

#########################################3333
##testing recommender lab
##############################################

class(ratings$book_id)
as(ratings[,c('book_id','rating')],'matrix') -> rating.matrix
rownames(rating.matrix)<- ratings$user_id

rating.matrix %>% View

ratings.realmatrix <- as(ratings[,c('user_id','book_id','rating')],"realRatingMatrix")

ratings.realmatrix

image(ratings.realmatrix, main = "Raw Ratings")  
image(normalize(ratings.realmatrix), main = "Normalize Ratings")  

e <- evaluationScheme(ratings.realmatrix, method = "split", train = 0.8, given = 5, goodRating = 3)

IBCF_Z_P <- Recommender(ratings.realmatrix, "IBCF",
                        param=list(normalize = 'Z-score' ,method="cosine"))

print(IBCF_Z_P)
names(getModel(IBCF_Z_P))
getModel(IBCF_Z_P)$nn

recom <- predict(IBCF_Z_P, ratings.realmatrix[1:nrow(ratings.realmatrix)], type="ratings")
recom

rec_list<-as(recom,"list")
head(summary(rec_list))
#2nd user 2nd item
rec_list[[2]][2]

#prediction for 1st user
u1<-as.data.frame(rec_list[[1]])

attributes(u1)
class(u1)
u1$id<-row.names(u1)


p <- predict(IBCF_Z_P, ratings.realmatrix, type="topNList", n=5)
as(p,"list")[1:3]
as(p,"list") %>% as.data.frame() %>% t()%>% View
