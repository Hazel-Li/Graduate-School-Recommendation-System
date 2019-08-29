#import dataset

library(dplyr)
mydata <- dplyr::distinct(data0607)

aa=
  mydata %>%
  group_by(id) %>%
  select(id,Decision) %>%
  summarise(count=n()) %>%
  filter(count>2)

names(aa)=c('id','cnt')
id=c(1:4057)
aa$id=stu_id

bb=
  mydata %>%
  select(id,university,major,Decision,gpa,gre_verbal,gre_quant,gre_writing,status,commons)

cc=left_join(aa,bb,by=c("id"))

cc=cc[-2]


mydata <- mydata %>%
  select(stu_id,university,Decision)

uni_name <- mydata %>%
  group_by(university) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
  
uni_id=c(1:391)
uni_name$uni_id=uni_id

mydata=merge(uni_name,mydata,by="university")

stu_name <- distinct(cc, id)
stu_id=c(1:4057)
stu_name$stu_id=stu_id

mydata=merge(stu_name,mydata,by="id")

mydata=mydata[-2]
write.table(mydata,'popular.csv',row.names=FALSE,col.names=TRUE,sep=",")

---------------------------------------------------------------------

uu=
  mydata %>%
  select(university,commons)

newcomments=na.omit(uu)

sum_com=
  newcomments %>%
  group_by(university) %>%
  summarise(count=n()) %>%
  filter(universiy %in% 'UCLA')