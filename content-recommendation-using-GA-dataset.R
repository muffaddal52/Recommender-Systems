library(ggplot2)
library(dplyr)
library(recommenderlab)
library(recosystem)
ga$contentId<- as.character(ga$contentId)

summary(ga)

#number of users?
ga$visitorId %>% unique() %>% length()
#6601 users

#number of content?
ga$contentId %>% unique() %>% length()
#973 articles

#average duration on article
mean(ga$session_duration)
median(ga$session_duration)
ga %>% ggplot()+
  geom_histogram(aes(x=scale_session_duration),bins = 100)


ga$scale_session_duration <- scale(ga$session_duration)

ga %>% group_by(contentId) %>% 
  summarise(average_duration = mean(session_duration)) %>% 
  ggplot() +
  geom_histogram(aes(x=average_duration),bins = 100)
#majority of the content's average session duration is low while some has very high session duration


ga %>% group_by(contentId) %>% 
  summarise(average_duration = mean(session_duration))%>% 
  summarise(mean = median (average_duration))
  ggplot() +
  geom_boxplot(aes(y=average_duration))
  
#average spend on each contetent is about 86644 sec
  
  ga %>% group_by(visitorId) %>% 
    summarise(average_duration = mean(scale_session_duration)) %>% 
    ggplot() +
    geom_histogram(aes(x=average_duration),bins = 100)

  
  ga %>% group_by(visitorId) %>% 
    summarise(content_visit_count = n()) %>% mutate(bucket = cut(content_visit_count,10)) %>% 
    group_by(bucket) %>%  summarise(bu_c = n()) %>% View
    ggplot() +
    geom_bar(aes(x=bucket))
    
    
    
    
    
  
  
