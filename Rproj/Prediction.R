//Reading file into R dataframe
d1<-read.csv("train.csv",header = TRUE) //Reading Train data
d2<-read.csv("test.csv",header = TRUE) //Reading Test data
library(dplyr)
library(tidyr)
//Mutating The values of 1 & 0 to satisfaction value 
d1 %>% mutate(value = 1)  %>% spread(satisfaction, value,  fill = 0 )
d1<-d1 %>% mutate(value = 1)  %>% spread(Gender, value,  fill = 0 )
d1 <- d1 %>% mutate_all(funs(replace_na(.,0)))
//Replacing null with 0's   
d1$Class[d1$Class=="Eco"]=0
d1$Class
d1<-d1 %>% mutate(value = 1)  %>% spread(Gender, value,  fill = 0 )
d1$Female<-NULL
d1<-d1 %>% mutate(value = 1)  %>% spread(Customer.Type, value,  fill = 0 )
d1$`disloyal Customer`<-NULL
d1<-d1 %>% mutate(value = 1)  %>% spread(Type.of.Travel, value,  fill = 0 )
//Removing unwanted columns 
d1$Personal Travel<-NULL
d1<-d1 %>% mutate(value = 1)  %>% spread(satisfaction, value,  fill = 0 )
d1$`neutral or dissatisfied`<-NULL
//Checking the Null values in the data frame
anyNA(d2)

#Logistic regression

require("dplyr")
require("Metrics")
install.packages("MLmetrics")
require("MLmetrics")
getwd()
setwd("C:/Users/singl/Desktop/Rproj")
d1<-read.csv("trainencodedlatest.csv",header = TRUE)
d2<-read.csv("testencodedlatest.csv",header = TRUE)
head(d1[23])
head(d2[23])
d1[, -c(23)] <- scale(d1[, -c(23)])
d2[, -c(23)] <- scale(d2[, -c(23)])
model<-glm(d1$satisfied~.,family = binomial,data=d1)
head(model)
predict1<-predict(model,d2)
predict1
probabilities <- model %>% predict(d2, type = "response")
predicted <- ifelse(probabilities > 0.5, 1, 0)
predicted
F1_Score(y_pred = predicted, y_true =d2$satisfied, positive = "1")


require("MLmetrics")
d1<-read.csv("trainencodedlatest.csv",header = TRUE)
d2<-read.csv("testencodedlatest.csv",header = TRUE)
library(e1071) #contains naïve Bayes function
model<-naiveBayes(d1$satisfied~., data=d1, laplace = 1) 
model             # generates model output
results<-predict(model,d2) 
results           # provides test prediction
F1_Score(y_pred = results, y_true =d2$satisfied, positive = "1")

//RandomForest Regressor
install.packages("randomForest")
install.packages("Metrics")
model<-randomForest(d1$satisfied~.,data=d1,importance=TRUE,na.action = na.roughfix)
summary(model)
predict<-predict(model,d2)
round_df <- function(x, digits) {
+     # round all numeric variables
+     # x: data frame 
+     # digits: number of digits to round
+     numeric_columns <- sapply(x, mode) == 'numeric'
+     x[numeric_columns] <-  round(x[numeric_columns], digits)
+     x
+ }

round_df(predict,1)
round_df(predict1,0)
f1(d2$satisfied,predict1)

