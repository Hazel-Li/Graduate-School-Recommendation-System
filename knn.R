library(dplyr)
mydata <- dplyr::distinct(data0607)

bg <- 
  mydata %>%
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


distance <- function(i, j){
  
  ui = c(as.numeric(bg[i,2:5]))
  uj = c(as.numeric(bg[j,2:5]))
  
  dist=sqrt(sum((ui-uj)^2))
  return(dist)
}

#Split the dataset
per = 0.8  #percentage
num = 4057
train_num = round(num * per)
test_num = num - train_num

cos_matrix = c(1:train_num)

n=3
test_topn= matrix(nrow = test_num, ncol = n)
for (i in (train_num+1):num){
  for (j in 1:train_num){
    cos_matrix[j] = distance(i,j)
  }
    test_topn[i-train_num,]=rank(cos_matrix)[1:n]
}

----------------------------------------------------
accu=c(1:test_num)
for (j in 1:test_num){
  accu[j]=0
  for (aa in test_topn[j,1:3]){
    c=which(InterMatrix[aa,]==1)
    for (i in c){
      if (InterMatrix[j+train_num,i]==1){
        accu[j]=1
      }
  }
  }
}

accuracy=sum(accu)/test_num
accuracy