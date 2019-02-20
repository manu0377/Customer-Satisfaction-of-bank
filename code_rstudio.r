

#importing the files
train <- read.csv('train.csv')
test <- read.csv('test.csv')

sample <- read.csv('sample_submission.csv')

head(train)

dim(train)

str(train)

# data loading step completed

# Starting the business problem
# Objective : to predict the customer satisfaction
    # 1- unsatisfaied customer
    # 0- satisfied customer


head(sample);table(sample$TARGET)

table(train$TARGET)

round(3008/(nrow(train))*100,2) # proportion of unsatisfied customers are 4%

# binary dependent variable (target)

# numeric feature variables (input)


# method selection for binary classification problem

#1 Logistic regression method- since we have more than 300 variables, NOT suggested

#2 Decision tree method- work sbest if you have cateogrical variables, NOT suggested

#3 random forest - recommended

#4 neural network- works best with binary (0/1), it is recommended, but with 0-1 variables it will provide better result

#5 support vector machine- works best with smaller data, NOT suggested

#6 gradient boosting method- recommended

#7 stochastic gradient descent algorithm- recommended

#8 xtreme gradient boosting algorithm- recommended

#9 Naive Bayes Algorithm- works best with categorical data, if you have numeric, transform it to categorical and then apply

# data pre-processing
# if the missing value is > 15% theen delete the variable
# if the missing value is < 15% go for imputation
    # column mean imputation for numeric variables
    # mode imputation for categorical variables

# removing the Target variable from input data and keeping it separate
train$ID<-NULL

test.id <- test$ID

test$ID <- NULL

names(train)

names(test)

# extract the target variable
train.y <- train$TARGET

train$TARGET <- NULL

# count of zero (0) value in all the input features ( creating a custom function)
count0 <- function(x){
    return( sum(x==0))
}

# count of zeros in train and test dataset

train$n0 <- apply(train, 1, FUN =count0)

head(train$n0)

test$n0 <- apply(test, 1, FUN =count0)

# remove the constant features
cat("\n ##removing the constant features.\n")

for (f in names(train)){
    if(length(unique(train[[f]]))==1){
        train[[f]]<-NULL
        test[[f]]<-NULL
    }
}

# removing the identical features
features_pair <- combn(names(train),2,simplify=F)
toRemove<- c()

for (pair in features_pair){
    f1<- pair[1]
    f2<- pair[2]
    
    if(!(f1 %in% toRemove) & !(f2 %in% toRemove)){
        if(all(train[[f1]]==train[[f2]]))
            toRemove <- c(toRemove,f2)
    }
}

feature.names<- setdiff(names(train),toRemove)

length(feature.names)

train$var38<-log(train$var38)
test$var38<-log(test$var38)

# model train and test dataset
train <- train[,feature.names]

test <- test[,feature.names]

tc <- test

dim(train); dim(test)

library(caret)
library(ggplot2)

# model creation part
head(train$var38)

#library(Hmisc)
#describe(train$var38)


boxplot(train$var38)

box<-boxplot(train$var38)
length(box$out)

# without log transformation the number of outliers for var 38 were 6530

# Outlier removal arguments for and against


# limiting the number of vars in test dataset based on min and max values of train
print("Setting min-max lims on the test data")

for(f in colnames(train)){
    lim<-min(train[,f])
    test[test[,f]<lim,f] <- lim
    
    lim <- max(train[,f])
    test[test[,f]>lim,f] <-lim
}


head(train)

train$TARGET <- train.y

library(randomForest)

train$ID <- NULL

df <- train[1:2000,]
df$ID <- NULL

df1 <-test[1:2000,]
df1$ID <- NULL

rf <- randomForest(df$TARGET~.,data=df, ntree=50)


head(train)

# random forest tuning parameters
# ntree- number of decision tree you want to create
# mtry- number of random variables used for tree creation

# if you are not specifying the number of mtry in the model, default it will take sqrt(number of variables)


summary(rf) # model summary 

rf$confusion # classification matric or confusion matrix

rf <- randomForest(df$TARGET~.,data=df, ntree=100,mtry=30)

(rf$confusion[1]+rf$confusion[4])/nrow(df)*100 # accuracy of random forest model

rf <- randomForest(factor(df$TARGET)~.,data=df,ntree=700,mtry=20)

prob<-predict(rf,newdata = test,type='prob') # estimating the prob score


