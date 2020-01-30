library(dplyr)
library(ggplot2)
library(caTools)
library(ISLR)
library(class)
library(rpart) 
library(randomForest)
library(rpart.plot)
library(Amelia)
library(lattice)
library(caret)
library(mlbench)
library(MLmetrics)
library(ade4)
library(data.table)
library(corrplot)
library(corrgram)
library(class)
library(caTools)

# Train and Test set creation
train = read.csv("C:\\Users\\grigo\\Documents\\R udemy solutions\\HR Analytics - analyticsvidhya\\datasets\\train_LZdllcl.csv")
test = read.csv("C:\\Users\\grigo\\Documents\\R udemy solutions\\HR Analytics - analyticsvidhya\\datasets\\test_2umaH9m.csv") 




# Checking for NAs 
missmap(test,main="Missing Map TEST",col=c("yellow","black"),legend=F)
missmap(train,main="Missing Map TRAIN",col=c("yellow","black"),legend=F)

 
# Filling NAs in "previous_year_rating" column with the most frequent value
pl <- ggplot(train,aes(x=train$previous_year_rating))
pl <- pl + geom_bar(position = "dodge",color="blue",fill="blue")+xlab("previous year rating")+labs(title = "Train Set")
print(pl)

mf.pyr <- as.numeric(tail(names(sort(table(train$previous_year_rating))), 1))
train[c("previous_year_rating")][is.na(train[c("previous_year_rating")])] <- mf.pyr
 

pl <- ggplot(test,aes(x=test$previous_year_rating))
pl <- pl + geom_bar(position = "dodge",color="red",fill="red")+xlab("previous year rating")+labs(title = "Test Set")
print(pl)

mf.pyr <- as.numeric(tail(names(sort(table(test$previous_year_rating))), 1))
test[c("previous_year_rating")][is.na(test[c("previous_year_rating")])] <- mf.pyr
 

# Checking correlation between data characteristics
dummy.train <- train
for(i in 2:6){
  dummy.train[,i] <- as.numeric(train[,i])
}

# Filter to numeric columns for correlation
cor.data <- cor(dummy.train)
corrplot(cor.data,method="number")


# One Hot Encoding for the categorical columns
ohe_feats = c("department","region","education","gender","recruitment_channel")
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(train[f])
  train[f] = NULL
  train = cbind(train, df_all_dummy)
}

for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(test[f])
  test[f] = NULL
  test = cbind(test, df_all_dummy)
}

 
 

#     Testing Logistic Regression, Decision Tree and KNN algorithms 
# Set a random seed so your "random" results are the same 
set.seed(101) 

# Split up the sample 70-30
sample <- sample.split(train$is_promoted, SplitRatio = 0.70) 
train70 = subset(train, sample == TRUE)
test30 = subset(train, sample == FALSE)


alg_f1_scores <- NULL
y_true <- test30$is_promoted
 
# Logistic Regression
log.model <- glm(formula=is_promoted ~ . ,family=binomial(link="logit"),data=train70)
fitted.probabilities <- predict(log.model,newdata=test30, type="response")
y_pred <- ifelse(fitted.probabilities > 0.5,1,0)

print("\nLogistic Regression")
print(F1_Score(y_true,y_pred,positive = NULL))
alg_f1_scores[1] <- F1_Score(y_true,y_pred,positive = NULL)
 

# Decision Tree
tree <- rpart(is_promoted ~.,method="class",data = train70)
y_pred <- predict(tree,test30, type="class")  
print(F1_Score(y_true,y_pred, positive = NULL))
alg_f1_scores[2] <- F1_Score(y_true,y_pred,positive = NULL)

 
# KNN
predicted.purchase = NULL
F1_Scr = NULL
end=3
for(i in 1:end){
  y_pred <- knn(train70,test30,train70$is_promoted,k=i)
  F1_Scr[i] = F1_Score(y_true,y_pred, positive = NULL)
} 

print(F1_Scr)

k.values <- 1:end
F1_Scr.df <- data.frame(F1_Scr,k.values)
ggplot(F1_Scr.df,aes(x=k.values,y=F1_Scr)) + geom_point()+ geom_line(lty="dotted",color="red")

best.K <- which.max(F1_Scr)

y_pred <- knn(train70,test30,train70$is_promoted,k=best.K)
alg_f1_scores[3] <- F1_Score(y_true,y_pred,positive = NULL)

print("alg_f1_scores")
print(alg_f1_scores)



# Training a model using the best algorithm
best.alg <- which.max(alg_f1_scores)
print("best.alg")
print(best.alg)
if(best.alg==1){
  log.model <- glm(formula=is_promoted ~ . ,family=binomial(link="logit"),data=train)
  fitted.probabilities <- predict(log.model,newdata=test, type="response")
}else if(best.alg==2){
  tree <- rpart(is_promoted ~.,method="class",data = train)
  y_pred <- predict(tree,test, type="class")

}else if(best.alg==3){
  y_pred <- knn(train,test,train$is_promoted,k=best.K)
}


# Exporting results to csv file
empl_id<-test30$employee_id
dffr <-data.frame(empl_id,y_pred)
head(dffr)

write.table('employee_id,is_promoted',"C:\\Users\\grigo\\Documents\\R udemy solutions\\HR Analytics - analyticsvidhya\\results.csv" ,row.names = FALSE,col.names=FALSE,sep=",",quote = FALSE)
write.table(dffr,"C:\\Users\\grigo\\Documents\\R udemy solutions\\HR Analytics - analyticsvidhya\\results.csv",row.names = FALSE,col.names=FALSE, sep=",", append = T)


 

 