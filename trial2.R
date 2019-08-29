library(dplyr)
mydata <- dplyr::distinct(data0607)
mydata$major_classify <- as.factor(mydata$major_classify)
input <- mydata%>%
  select(stu_id,major_classify,gpa_adjusted,gre_quant,gre_verbal,gre_writing,status)%>%
  filter(major_classify!='Other')
  
summary(input)
input[is.na(input$gre_writing),"gre_writing"] <- median(input$gre_writing,na.rm = T)
------------------------------
  
library(caret)
dummy <- dummyVars(~major_classify,data=input[1:2],fullRank=T)
trfs<-data.frame(predict(dummy,newdata=input))

------------------------------
major_input=cbind(input[1],trfs[-5])
names(major_input)=c('stu_id','major1','major2','major3','major4','major5','major6',"major7",'major8')
major <- 
major_input %>%
  group_by(stu_id) %>%
  select(stu_id,major1,major2,major3,major4,major5,major6,major7,major8)%>%
  summarise(major1=max(major1),
            major2=max(major2),
            major3=max(major3),
            major4=max(major4),
            major5=max(major5),
            major6=max(major6),
            major7=max(major7),
            major8=max(major8))

status <- 
  input %>%
  group_by(stu_id) %>%
  select(stu_id,status)%>%
  summarise(status=max(status))

bg <- 
  input %>%
  group_by(stu_id) %>%
  select(stu_id,gre_quant,gre_verbal,gre_writing,gpa_adjusted)%>%
  summarise(gre_quant=max(gre_quant),
            gre_verbal=max(gre_verbal),
            gre_writing=max(gre_writing),
            gpa=max(gpa_adjusted))

bg$gpa <- scale(bg$gpa)
bg$gre_verbal <- scale(bg$gre_verbal)
bg$gre_quant <- scale(bg$gre_quant)
bg$gre_writing <- scale(bg$gre_writing)
------------------------------
sim_major <- function(i, j){
  
  ui = c(as.numeric(major[i,2:9]))
  uj = c(as.numeric(major[j,2:9]))
  
  pro = crossprod(ui,uj)
  abs_ui <-crossprod(ui,ui) ^ 0.5
  abs_uj <-crossprod(uj,uj) ^ 0.5
  cosi_j <- pro/(abs_ui*abs_uj)
  return(cosi_j)
}

sim_grade <- function(i, j){
  
  ui = c(bg$gpa[i],bg$gre_verbal[i],bg$gre_quant[i],bg$gre_writing[i])
  uj = c(bg$gpa[j],bg$gre_verbal[j],bg$gre_quant[j],bg$gre_writing[j])
  
  pro = crossprod(ui,uj)
  abs_ui <-crossprod(ui,ui) ^ 0.5
  abs_uj <-crossprod(uj,uj) ^ 0.5
  cosi_j <- pro/(abs_ui*abs_uj)
  return(cosi_j)
}

sim_status <- function(i,j){
  if (status[i,2]==status[j,2]){return(1)}
  else {return(0)}
}
---------------------------------------------
min_max_scale <- function (X) {
    X = (X-min(X))/(max(X)-min(X))
    return(X)
}

scale <- function (X){
  X = (X-mean(X))/(var(X)^0.5)
  return(X)
}
----------------------------------------------
interaction <- mydata%>%
  select(stu_id,uni_id,Decision)

interaction$decision = 1

library(reshape)

InterMatrix <- cast(interaction, stu_id ~ uni_id, value = "Decision")
InterMatrix <- as.matrix(InterMatrix)
-----------------------------------------------
#Split the dataset
per = 0.8  #percentage
num = 4057
train_num = round(num * per)
test_num = num - train_num

cos_matrix = c(1:train_num)
TestMatrix = InterMatrix[(train_num+1):num,]

for (i in (train_num+1):num){
  for (j in 1:train_num){
    cos_matrix[j] = mean(sim_major(i,j),sim_grade(i,j),sim_status(i,j))
  }
  for (j in 1:ncol(InterMatrix)){
    pp=crossprod(cos_matrix,InterMatrix[1:train_num,j])
    mean_cos = pp/sum(InterMatrix[1:train_num,j])
    TestMatrix[i-train_num,j] = mean_cos
  }
}


#Generating Recommendation List
a <- c(5,10,15,20,25,30,35,40,45,50,55,60)

cal_accu <- function(n){
  test_topn= matrix(nrow = test_num, ncol = n)
  for (i in 1:test_num){
    test_topn[i,] <- order(TestMatrix[i,],decreasing = TRUE)[1:n]
  }
  
  recall=c(1:test_num)
  for (i in 1:test_num){
    accu = 0
    for (j in 1:n){
      if (InterMatrix[i+train_num,test_topn[i,j]]==1){
        accu = accu+1
      }
    }
    num_all=sum(InterMatrix[i+train_num,])
    recall[i]=accu/num_all
  }
  return(recall)
}

mrecall <- c(1:12)
for (i in 1:12){
  k=cal_accu(a[i])
  mrecall[i]=mean(k[which(k!=0)])
}

mrecall

library(ggplot2)
mydata %>%
  group_by(stu_id) %>%
  summarise(min=min(status)) 

write.csv(InterMatrix,'InterMatrix.csv')