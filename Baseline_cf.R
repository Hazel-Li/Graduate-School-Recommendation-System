library(reshape)
InterMatrix <- cast(rating, stu_id ~ uni_id, value = "Decision")
InterMatrix <- as.matrix(InterMatrix)

cal_pearson<- function(i, j){
  ui=InterMatrix[i,]
  uj=InterMatrix[j,]
  
  vari=sum((ui-mean(ui))^2)/(4-1)
  varj=sum((uj-mean(uj))^2)/(4-1)
  sdi=sqrt(vari)
  sdj=sqrt(varj)
  cov=sum((ui-mean(ui))*(uj-mean(uj)))/(4-1)
  cor=cov/ (sdi*sdj)
  return(cor)
}

cal_cos <- function(i, j){
  ui=InterMatrix[i,]
  uj=InterMatrix[j,]
  
  pro = crossprod(ui,uj)
  abs_ui <-crossprod(ui,ui) ^ 0.5
  abs_uj <-crossprod(uj,uj) ^ 0.5
  cosi_j <- pro/(abs_ui*abs_uj)
  return(cosi_j)
}

min_max_scale <- function (X) {
  X = (X-min(X))/(max(X)-min(X))
  return(X)
}


ResultMatrix = InterMatrix
for (i in 1:4057){
  for (j in 1:4057){
    cos_matrix[j] = cal_cos(i,j)
  }
  cos_matrix <- min_max_scale(cos_matrix)
  similarity = cos_matrix / sum(cos_matrix)
  for (j in 1:ncol(InterMatrix)){
    product = crossprod(similarity,InterMatrix[,j])
    if (ResultMatrix[i,j]<1) {
      ResultMatrix[i,j] = product
    }
  }
}

------------------------------------------------------------------------------
#Split the dataset
per = 0.8  #percentage
num = 4057
train_num = round(num * per)
test_num = num - train_num  

#In the test, we recommend schools to the last 20%, if any one hits the target,
#it is considered a good recommendation.
cos_matrix = c(1:train_num)
TestMatrix = InterMatrix[(train_num+1):num,]
for (i in (train_num+1):num){
  for (j in 1:train_num){
    cos_matrix[j] = cal_pearson(i,j)
  }
  cos_matrix <- min_max_scale(cos_matrix)
  similarity = cos_matrix / sum(cos_matrix)
  for (j in 1:ncol(InterMatrix)){
    product = crossprod(similarity,InterMatrix[1:train_num,j])
    TestMatrix[i-train_num,j] = product
  }
}

#Generating Recommendation List
n = 10
test_topn= matrix(nrow = test_num, ncol = n)
for (i in 1:test_num){
  test_topn[i,] <- order(TestMatrix[i,],decreasing = TRUE)[1:n]
}

#Calculating Acuuracy
accu = c(1:test_num)

for (i in 1:test_num){
  accu[i] = 0
  for (j in 1:n){
    if (InterMatrix[i+train_num,test_topn[i,j]]==1){
      accu[i] = 1
    }
  }
}

accuracy = sum(accu)/test_num
accuracy