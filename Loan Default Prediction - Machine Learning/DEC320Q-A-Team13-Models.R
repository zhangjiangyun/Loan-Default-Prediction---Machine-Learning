# We use Data Analyics Functions and Support
source("DEC320Q-A-Team13-DataAnalyticsFunctions.R")
source("DEC320Q-A-Team13-Support.R")

### Install Packages
install.packages("ggplot2")
install.packages("corrplot")
library(corrplot)
library(ggplot2)
install.packages("AUC")
library(AUC)

# Import Data
load("DEC320Q-A-Team13-Data.rda")
scan(new_DT)
detail(new_DT)

# Since we are Team 13, why not use 13 as our seed
# Divide the whole dataset into 25 fold
set.seed(13)
n<-nrow(new_DT)
nfold<- 25
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

# For saving your time, we can use 4% data to train and 4% to test
# Of course, we can also run the whole data set to train our models better
train <- which(foldid == 12 )
test <- which(foldid == 13 )

# Drop the non numerical variables for some models
drop <- c("id","member_id","term","sub_grade","grade","emp_length",
          "home_ownership","verification_status","issue_d","pymnt_plan",
          "purpose","addr_state","earliest_cr_line","initial_list_status",
          "last_pymnt_d","next_pymnt_d","last_credit_pull_d","application_type",
          "verification_status_joint")

# Drop the information for the loan in order to train the model for applicants
dropfor <-c("outout_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv",
            "total_rec_int","total_rec_late_fee","total_rec_prncp","funded_amnt_inv",
            "collection_recovery_fee","revol_bal","last_pymnt_amnt","collections_12_mths_ex_med",
            "acc_now_delinq","term","grade","initial_list_status","funded_amnt","int_rate",
            "installment","dti","pub_rec","total_acc","out_prncp","recoveries")

# Generate subset of data
DataApp <- new_DT[,!(names(new_DT) %in% dropfor)]
DataNum <- new_DT[,!(names(new_DT) %in% drop)]
DataNew <- DataNum[,!(names(DataNum) %in% dropfor)]


# Data with all variables
DataTrainNon <- new_DT[train,]
DataTestNon <- new_DT[test,]
scan(DataTrainNon)

# Data with all numerical variables
DataTrainNum <- DataNum[train,]
DataTestNum <- DataNum[test,]
scan(DataTrainNum)

# Data set only for applicants
DataTrainNonApp <- DataApp[train,]
DataTestNonApp <- DataApp[test,]
scan(DataTrainNonApp)

# Data set only for applicants with all variables
DataTrainNumApp <- DataNew[train,]
DataTestNumApp <- DataNew[test,]
scan(DataTrainNumApp)

## Data Understanding & Visualization
# Running a correlation matrix
# Drop those nonnumeric columns
cormatrix<-cor(DataTrainNum)
corrplot(cormatrix, method = "square")

######################
## Hypothesis Test
## Try to find prove some relationship by hypothesis test
## Take default and home_ownership as examples
m00 <- sum( (new_DT$default == 0) & (new_DT$home_ownership == "RENT") ) 
m01 <- sum( (new_DT$default == 0) & (new_DT$home_ownership == "OWN") ) 
m10 <- sum( (new_DT$default == 1) & (new_DT$home_ownership == "RENT") ) 
m11 <- sum( (new_DT$default == 1) & (new_DT$home_ownership == "OWN") ) 

# Construct the contingency table
ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
chisq.test(ContingencyMatrix)$p.value  # p-value of the statistical test 

## p-value is really small, so we concluded that default have strong relationship with home_ownership
plot(factor(default) ~ grade, data=new_DT, col=c(8,9), ylab="default", xlab="Grade") 
## We proved lower grade have larger probalbility to default

## Visualization Default vs SubGrade
levels(new_DT$grade)
plot(factor(default)~grade, data=new_DT, main = "Loan Status VS Grade", ylab= "default Status", col=8:9)

#############
## Model
## Supervised

## Logistic Regression
Logistic_Loan <- glm(default~., data=DataTrainNon, family="binomial")
Logistic_App <- glm(default~., data=DataTrainNonApp, family="binomial")
summary(Logistic_Loan)
summary(Logistic_App)

## Prediction
Prediction_Loan <- predict(Logistic_Loan,newdata=DataTestNon,type="response")
Prediction_App <- predict(Logistic_App,newdata=DataTestNonApp,type="response")

## we get the predicted default probalbility based on our model
smoothScatter(DataTestNon$int_rate,Prediction_Loan, xlab="Interest Rate", ylab="default")


## LASSO
library(glmnet)
## Lasso training for Loan
Mx <- model.matrix(default~.,DataTrainNon)[,-1]
My <- DataTrainNon$default
Tx <- model.matrix(default~.,DataTestNon)[,-1]
Ty <- DataTestNon$default
  
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.default <- sum(My)
w <- (num.default/num.n)*(1-(num.default/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lasso_loan<-glmnet(Mx,My,family="binomial",lambda=lambda.theory)

#Lasso testing
lasso_pred_loan<-predict(lasso_loan,newx=Tx,type="response")

## The Variables Lasso selected and their coefficient
val_bet_loan<-as.matrix(lasso_loan$beta)
val_bet_loan<- cbind(rownames(val_bet_loan),val_bet_loan)[support(lasso_loan$beta),]
rownames(val_bet_loan) <- NULL
colnames(val_bet_loan)<-c("Regressor","coefficient")
val_bet_loan

## Lasso training for Loan
Mx <- model.matrix(default~.,DataTrainNonApp)[,-1]
My <- DataTrainNonApp$default
Tx <- model.matrix(default~.,DataTestNonApp)[,-1]
Ty <- DataTestNonApp$default

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.default <- sum(My)
w <- (num.default/num.n)*(1-(num.default/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)

lasso_app<-glmnet(Mx,My,family="binomial",lambda=lambda.theory)
#Lasso testing
lasso_pred_app<-predict(lasso_app,newx=Tx,type="response")

## The Variables Lasso selected and their coefficient
val_bet_app<-as.matrix(lasso_app$beta)
val_bet_app<- cbind(rownames(val_bet_app),val_bet_app)[support(lasso_app$beta),]
rownames(val_bet_app) <- NULL
colnames(val_bet_app)<-c("Regressor","coefficient")
val_bet_app

## K-NN 
#input the packages
installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)

#scaling the data for preparing knn
x <- scale(DataTrainNum)
y <- scale(DataTestNum)
x1 <-scale(DataTrainNumApp)
y1 <- scale(DataTestNumApp)
 
#sets the target variable for the training set
g <- factor(DataTrainNum$default)
g1 <- factor(DataTrainNumApp$default)

### this is what runs k-nn for prediction.summary the 
## It's Team 13, so we chose the number of K as 13
mod13_loan <- knn(train=x, test=y, k=13, cl=g, prob=TRUE)
mod13_app <- knn(train=x1, test=y1, k=13, cl=g1, prob=TRUE)
KNN_prob_loan <- 1-attr(mod13_loan,"prob")
KNN_prob_app <- 1-attr(mod13_app,"prob")

## Summary the model
summary(mod13_loan)
summary(mod13_app)


## Classification Tree and Decision Tree
### install the required packages some packages
install.packages("tree")
install.packages("partykit")
install.packages("pROC")
library(tree)
library(partykit)
library(rpart)

## Classification Tree 
DecisionTreeLoan <- rpart(default ~ .,data=DataTrainNon, method = "class")
DecisionTreeApp <- rpart(default ~ .,data=DataTrainNonApp, method = "class")

summary(DecisionTreeLoan)
summary(DecisionTreeApp)

DT_Prediction_Loan <- predict(DecisionTreeLoan,DataTestNon,type="prob")
DT_Prediction_App <- predict(DecisionTreeApp,DataTestNonApp,type="prob")

###
### We can obtain a description of the whole tree by 
### It is the Classification Tree of Loan
DecisionTreeLoan
DecisionTreeApp

###
### We can easily plot the tree
plot(DecisionTreeLoan)
text(DecisionTreeLoan)

### using label = "yval" we obtain the recommendation
t1 <- table(predict(DecisionTreeLoan,DataTestNon,type="class"),actual=DataTestNon$default)
t2 <- table(predict(DecisionTreeApp,DataTestNon,type="class"),actual=DataTestNonApp$default)
t1
t2
## According to our result, the trees model do not apply to the information for applicants
## The reason might be that we do not have enough variables 

##
method_names<-c("logistic","decision_tree","knn","lasso")
ACC<-matrix(data=rep(0,times=length(method_names)*100,nrow=100,ncol=length(method_names)),nrow = 100)
ACC_App<-matrix(data=rep(0,times=length(method_names)*100,nrow=100,ncol=length(method_names)),nrow = 100)

## Visulization the Performances of our models by ROC curve
## It's for application for loan
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
for ( val in seq(from = 0, to = 1, by = 0.01)  ){
  
  values_dt <- FPR_TPR( ( DT_Prediction_Loan[,2] >= val), DataTestNon$default)
  points(values_dt$FPR,values_dt$TPR,col="black",pch=16) 
  ACC[val*100,1] <-values_dt$ACC
    
  values_log <- FPR_TPR( (Prediction_Loan >= val) , DataTestNon$default)
  points(values_log$FPR,values_log$TPR,col="blue",pch=16)    
  ACC[val*100,2] <-values_log$ACC
  
  values_knn <- FPR_TPR( ( KNN_prob_loan >= val), DataTestNon$default)
  points(values_knn$FPR,values_knn$TPR,col="red",pch=2)   
  ACC[val*100,3] <-values_knn$ACC
  
  values_l <- FPR_TPR( (lasso_pred_loan >= val) , DataTestNon$default)
  points(values_l$FPR,values_l$TPR,col="green",pch=16) 
  ACC[val*100,4] <-values_l$ACC
}


## For application for applicants
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)

for ( val in seq(from = 0, to = 1, by = 0.01)  ){
  
    values_dt1 <- FPR_TPR( ( DT_Prediction_App[,2] >= val), DataTestNonApp$default)
    points(values_dt1$FPR,values_dt1$TPR,col="black",pch=16) 
    ACC_App[val*100,1] <-values_dt1$ACC
    
    values_log1 <- FPR_TPR( (Prediction_App >= val) , DataTestNonApp$default)
    points(values_log1$FPR,values_log1$TPR,col="blue",pch=16)    
    ACC_App[val*100,2] <-values_log1$ACC
  
    values_knn1 <- FPR_TPR( ( KNN_prob_app >= val), DataTestNonApp$default)
    points(values_knn1$FPR,values_knn1$TPR,col="red",pch=2)   
    ACC_App[val*100,3] <-values_knn1$ACC
    
    values_l1 <- FPR_TPR( (lasso_pred_app >= val) , DataTestNonApp$default)
    points(values_l1$FPR,values_l1$TPR,col="green",pch=16)  
    ACC_App[val*100,4] <-values_l1$ACC
}

# Evaluate performance of Methods
# 97% of our dependent variable is 0, which means the loan is not defaulted
# So even if the models predict all 0, there is still a 97% accuracy
# So we think the accuracy is not that important in our evaluation
# We prefer to use AUC to evaluate 
# Build a dataframe
perf_measure<-c("AUC","AVG_ACC", "Max_ACC")
Evaluation_Loan<-matrix(data=rep(0,times=length(method_names)*length(perf_measure)),nrow=length(method_names),ncol=length(perf_measure))
Evaluation_App<-matrix(data=rep(0,times=length(method_names)*length(perf_measure)),nrow=length(method_names),ncol=length(perf_measure))


rownames(Evaluation_Loan)<-method_names
colnames(Evaluation_Loan)<-perf_measure
rownames(Evaluation_App)<-method_names
colnames(Evaluation_App)<-perf_measure

Evaluation_Loan[,"AUC"]<- c(auc(DataTestNon$default,Logistic_Loan$fitted),
                   auc(DataTestNon$default,DT_Prediction_Loan[,2] ),
                   auc(DataTestNon$default,KNN_prob_loan),
                   auc(DataTestNon$default,lasso_pred_loan))
Evaluation_Loan[,"Max_ACC"] <- c(max(ACC[,2]),max(ACC[,1]),max(ACC[,3]),max(ACC[,4]))
Evaluation_Loan[,"AVG_ACC"] <- c(mean(ACC[,2]),mean(ACC[,1]),mean(ACC[,3]),mean(ACC[,4]))
Evaluation_Loan

Evaluation_App[,"AUC"]<- c(auc(DataTestNonApp$default,Logistic_App$fitted),
                            auc(DataTestNonApp$default,DT_Prediction_App[,2]),
                            auc(DataTestNonApp$default,KNN_prob_app),
                            auc(DataTestNonApp$default,lasso_pred_app))
Evaluation_App[,"Max_ACC"] <- c(max(ACC_App[,2]),max(ACC_App[,1]),max(ACC_App[,3]),max(ACC_App[,4]))
Evaluation_App[,"AVG_ACC"] <- c(mean(ACC_App[,2]),mean(ACC_App[,1]),mean(ACC_App[,3]),mean(ACC_App[,4]))
Evaluation_App
## Model
## Unsupervised

## K-Means for Loan
x <- model.matrix(~., data=DataTrainNum)[,-1]
x.scaled <- scale(x)
five.clusters <- kmeans(x.scaled, 5, nstart=10)
tapply(DataTrainNum$default, five.clusters$cluster, table)

## Do PCA
## We will install a package just to get the data
install.packages("plfm")
library(plfm)

### Lets compute the (Full) PCA
vals <- five.clusters$cluster
pca.x <- prcomp(x, scale=FALSE)
predict.pc <- predict(pca.x)

##################################################
## Interpreting the factors
loadings <- pca.x$rotation

### For each factor lets display the top features that 
## Visualization of top 6 variables

v1<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(x)],1]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v1[1:k])%*%v1[1:k] - 1/2 )^2)
v1[1:which.min(loadingfit)]
barplot(100*v1[1:6],space=NULL,las=2,xpd=FALSE , xlab="", ylab = bquote("Percentage %"),ylim=c(0,100), main="PCA of the Cluster 1 (Loan)",col = 3)

v2<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(x)],2]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v2[1:k])%*%v2[1:k] - 1/2 )^2)
v2[1:which.min(loadingfit)]
barplot(abs(100*v2[1:6]),space=NULL,las=2,xpd=FALSE , xlab="", ylab = bquote("Percentage %"),ylim=c(0,100), main="PCA of the Cluster 2 (Loan)",col = 3)

v3<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(x)],3]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v3[1:k])%*%v3[1:k] - 1/2 )^2)
v3[1:which.min(loadingfit)]
barplot(abs(100*v3[1:6]),space=NULL,las=2,xpd=FALSE , xlab="", ylab = bquote("Percentage %"),ylim=c(0,100), main="PCA of the Cluster 3 (Loan)",col = 3)

v4<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:ncol(x)],4]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v4[1:k])%*%v4[1:k] - 1/2 )^2)
v4[1:which.min(loadingfit)]
barplot(abs(100*v4[1:6]),space=NULL,las=2,xpd=FALSE , xlab="", ylab = bquote("Percentage %"),ylim=c(0,100), main="PCA of the Cluster 4 (Loan)",col = 3)

v5<-loadings[order(abs(loadings[,5]), decreasing=TRUE)[1:ncol(x)],5]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v5[1:k])%*%v5[1:k] - 1/2 )^2)
v5[1:which.min(loadingfit)]
barplot(abs(100*v5[1:6]),space=NULL,las=2,xpd=FALSE , xlab="", ylab = bquote("Percentage %"),ylim=c(0,100), main="PCA of the Cluster 5 (Loan)",col = 3)

