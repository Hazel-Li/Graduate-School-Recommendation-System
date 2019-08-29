#Split the dataset
per = 0.8  #percentage
num = 4057
train_num = round(num * per)
test_num = num - train_num 

popular$Decision = 1  
library(reshape)

InterMatrix <- cast(popular, stu_id ~ uni_id, value = "Decision")
InterMatrix <- as.matrix(InterMatrix)

#In the test, we recommend schools to the last 20%, if any one hits the target,
#it is considered a good recommendation.
TestMatrix = InterMatrix[(train_num+1):num,]
for (i in (train_num+1):num){
  for (j in 1:ncol(InterMatrix)){
    TestMatrix[i-train_num,j] = NaN
  }
}

#Calculating Acuuracy
a <- c(5,10,15,20,25,30,35,40,45,50,55,60)

cal_accu <- function(n){
  test_topn= matrix(nrow = test_num, ncol = n)
  for (i in 1:test_num){
    test_topn[i,] <- order(TestMatrix[i,],decreasing = TRUE)[1:n]
  }
  
  accu = c(1:test_num)
  for (i in 1:test_num){
    accu[i] = 0
    for (j in 1:n){
      if (InterMatrix[i+train_num,test_topn[i,j]]==1){
        accu[i] = 1
      }
    }
  }
  return(accu)
}

accuracy <- c(1:12)
for (i in 1:12){
  accuracy[i]=sum(cal_accu(a[i]))/test_num
  accuracy[i]
}

