library(recommenderlab)
library(reshape)
library(ggplot2)

interaction=rating
summary(interaction)
interaction=na.omit(interaction)

vector_rating <- as.vector(interaction$Decision)
table_rating <- table(vector_rating)

vector_rating <- vector_rating[vector_rating!=0]
vector_rating <- factor(vector_rating)
qplot(vector_rating) + ggtitle('Distribution of the ratings')

# convert to matrix

matrixdata <- cast(interaction, id ~ uni_id, value = "Decision")
--------------------------------------------------------------------------------------------
class(matrixdata) <- "data.frame" 
matrixdata<- as.matrix(matrixdata)
dim(matrixdata)

#rating_data <- matrixdata[rowCounts(matrixdata) > 3,colCounts(matrixdata) > 3]
#dim(rating_data)

##convert into "realRatingMatrix", which can be processed by recommenderlab
matrixdata<- as(matrixdata, "realRatingMatrix")  
dim(matrixdata)
colnames(matrixdata) <- paste("item", 1:445, sep = "")

model.eval <- evaluationScheme(matrixdata, method = "split", train = 0.8, given = 15, goodRating = 3)
model.eval

model.pop <- Recommender(getData(model.eval, "train"), method = "popular")

predict.pop <- predict(model.pop, getData(model.eval, "known"), type = "ratings")

r1 <- Recommender ( getData ( model.eval , "train" ) , "UBCF" ,"pearson") ;
p1 <- predict ( r1 , getData ( model.eval , "known" ) , type = "ratings" ) ;
r2 <- Recommender ( getData ( model.eval , "train" ) , "IBCF" ,"cosine") ;
p2 <- predict ( r2 , getData ( model.eval , "known" ) , type = "ratings" ) 

algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF_pearson = list(name="IBCF", param=list(method='pearson')),
  UBCF_cosine =list(name="UBCF", param=list(method='cosine')),
  UBCF_pearson =list(name="UBCF", param=list(method='pearson')),
  SVD=list(name="SVD")
)

results <- evaluate(model.eval, algorithms, type = "ratings")
plot (results,ylim=c(0,500))
results@.Data
-----------------------------------------------------------------------------------------------
recc_model=Recommender(getData ( model.eval , "train" ),method="IBCF", param=list(method="Jaccard"))
model_details <- getModel(recc_model)

n_recommended <- 5
recc_predicted <- predict(object = recc_model, getData ( model.eval , "unknown" ), n = n_recommended)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(matrixdata)[x]
})

recc_matrix[, 1:4]
-----------------------------------------------------------------------------------------------
tops <- evaluationScheme (getData ( model.eval , "train" )  , method = "cross" , k = 4 , given = 10 ,  goodRating = 3 )
results <- evaluate ( tops , algorithms  , type = "topNList" ,  n = c ( 1 , 3 , 5) )
plot ( results , annotate = TRUE )


