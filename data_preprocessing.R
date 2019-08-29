#import dataset
summary(stu_uni)

library(dplyr)
mydata <- dplyr::distinct(stu_uni)
summary(mydata)

uni_name <- distinct(mydata, university)
uni_id=c(1:483)
uni_name$uni_id=uni_id


mydata=merge(uni_name,mydata,by="university")

names(mydata)=c('uni_name','')

aa=
  mydata %>%
  group_by(id) %>%
  select(id,Decision) %>%
  summarise(count=n()) %>%
  filter(count>2)

interaction=
  mydata %>%
  group_by(id,uni_id) %>%
  select(id,uni_id,Decision)
  


names(aa)=c('id','cnt')
mydata=left_join(aa,interaction,by=c("id"))

mydata[-2]
write.table(mydata[-2] ,'interaction.csv',row.names=FALSE,col.names=TRUE,sep=",")
