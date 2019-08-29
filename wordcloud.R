#wordcloud
library(dplyr)
library(tidytext)
library(stringr)

u1 <- comments%>%
  filter(university == 'University Of California, Berkeley (UCB)')
u2 <- comments%>%
  filter(university == 'Columbia University') 
u3 <- comments%>%
  filter(university == 'University Of California, Los Angeles (UCLA)') 
u4 <- comments%>%
  filter(university == 'Stanford University') 
u5 <- comments%>%
  filter(university == 'Carnegie Mellon University (CMU)')

u1$university=c(1:nrow(u1))
u2$university=c(1:nrow(u2))
u3$university=c(1:nrow(u3))
u4$university=c(1:nrow(u4))
u5$university=c(1:nrow(u5))
names(u1)=c("line","text")
names(u2)=c('line','text')
names(u3)=c('line','text')
names(u4)=c('line','text')
names(u5)=c('line','text')
---------------------------------------------------------------
tokenize <- function(uu){
  tokens <- 
    uu %>%
    unnest_tokens(word, text)
  
  tidy_text <- tokens %>%
    anti_join(stop_words) %>%
    filter(is.na(as.numeric(word)))%>%
    filter(!word %in% c('university','school','phd','update','website','program'))%>%
    filter(str_detect(word,pattern = "^(?!.[0-9]).*$")) %>%
    filter(str_detect(word,pattern = "^(?!.*mail).*$")) %>%
    filter(str_detect(word,pattern = "^(?!.*appl).*$")) %>%
    filter(str_detect(word,pattern = "^(?!.*check).*$"))
  
  return(tidy_text)
}

tidy1=tokenize(u1)
tidy2=tokenize(u2)
tidy3=tokenize(u3)
tidy4=tokenize(u4)
tidy5=tokenize(u5)
-----------------------------------------------------------------
library(wordcloud)
tidy5 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, scale=c(2.2,0.4),max.words = 35,max_font_size=16,colors=brewer.pal("seq","Greys")[4:9]))

-----------------------------------------------
tf_cal <- function(tidy,name){
  tf <- 
    tidy %>%
    count(word, sort = TRUE) %>%
    mutate(university=name) %>%
    mutate(word = reorder(word, n)) %>%
    mutate(freq=n/nrow(tidy)) %>%
    filter(n>=5)
  return(tf)
}

tf1=tf_cal(tidy1,name='UCB')
tf2=tf_cal(tidy2,name='CU')
tf3=tf_cal(tidy3,name='UCLA')
tf4=tf_cal(tidy4,name="SF")
tf5=tf_cal(tidy5,name="CMU")
------------------------------------------
doc_size=5
corpus <- rbind(tf1,tf2,tf3,tf4,tf5) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))%>%
  mutate(idf=log(doc_size/n)) %>% 
  select(university,word,freq)%>%
------------------------------------------
cal_tfidf <- function(corpus,tf){
  trans=merge(corpus,tf,by='word',all.x=TRUE)
  na.omit(trans)
}

tfidf1=cal_tfidf(corpus[-2],tf1)
tfidf2=cal_tfidf(corpus[-2],tf2)
tfidf3=cal_tfidf(corpus[-2],tf3)
tfidf4=cal_tfidf(corpus[-2],tf4)
tfidf5=cal_tfidf(corpus[-2],tf5)

tfidf <- rbind(tfidf1,tfidf2,tfidf3,tfidf4,tfidf5) %>%
  mutate(tf_idf=freq*idf)%>%
  arrange(desc(tf_idf))


write.csv(tfidf,"dataset/tfidf.csv")
