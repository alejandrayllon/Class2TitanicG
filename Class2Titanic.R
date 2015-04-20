#bring in train data
titanic = read.csv("train.csv")

#get rid of unwanted columns
titanic = titanic[c(2,3,5,6,7,8,10,12)]

#find rows with omit, and get rid of omits to find age average
na_rows = is.na(titanic$Age)
titanic_no_na = na.omit(titanic)
mean_age = mean(titanic_no_na$Age)

#replace NA's with mean age
for(n in 1:891){
  if(na_rows[n] == "TRUE"){
    titanic$Age[n] = mean_age
  }else{ 
  }
}

#use rpart to make decision tree, then go online to use a postscript viewer
install.packages("rpart")
library(rpart) 
titanic_tree <- rpart(Survived~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked, data=titanic, method="class") 
post(titanic_tree, file = "tree.ps", title = "Classification Tree Titanic")

#using 10-fold CV
install.packages("caret")
library(caret)
set.seed(24)
titanic_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)

#gbm model
set.seed(24)
gbm_fit1 <- train(Survived~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked, data=titanic, method = "gbm", trControl = titanic_control, verbose = FALSE)
gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 4), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05,.01,.005,.001))
nrow(gbmGrid)
set.seed(24)
gbm_fit2 <- train(Survived~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked, data=titanic, method = "gbm", trControl = titanic_control, verbose = FALSE, tuneGrid = gbmGrid)


#inspect model
trellis.par.set(caretTheme())
plot(gbm_fit2) 
summary(gbm_fit2)

#fill in Age NA's with average in train file
titanic_test = read.csv("test.csv")
titanic_test = titanic_test[c(2,4,5,6,7,9,11)]
na_rows_test = is.na(titanic_test$Age)

for(n in 1:418){
  if(na_rows_test[n] == "TRUE"){
    titanic_test$Age[n] = mean_age
  }else{ 
  }
}

#fill in Fare NA's with average in train file
na_rows_fare = is.na(titanic$Fare)
mean_fare = mean(titanic$Fare)

for(n in 1:418){
  if(na_rows[n] == "TRUE"){
    titanic_test$Fare[n] = mean_fare
  }else{ 
  }
}

#predict
probability_survive <- predict(gbm_fit2, newdata = titanic_test)
survive <- round(probability_survive)
print(survive)

#export
write.table (survive, file = "TitanicTest.csv", append = TRUE, col.names = FALSE, row.names = FALSE) 

