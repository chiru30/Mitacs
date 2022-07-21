install.packages("party") 
library(party)                                     #For machine learning methods
library(caret)                                     #For machine learning methods
require(ggplot2)                                   #For Figures
library(glmnet)                                    #For Lasso
library(e1071)                                     #For machine learning methods
library(caTools)                                   #For machine learning methods
#DATA GENERATION---------------------------------------------------------------
set.seed(7267166) 
n <- 100                                           #Sample size
beta_imp   <- c(rep(5,10))                         #Coef for imprortant variables  
beta_un    <- c(rep(0.1,10))                       #Coef. for sparse (unimportant) var.
full_beta <- matrix(c(beta_imp,beta_un),20,1)      #Full vector of coefs.
X <- matrix(rep(runif(n*20)),n,20)                 #Full of covariates from uniform dist.
resid <- rnorm(n)

Tot <- X%*%full_beta+resid                         #Responses (non-class)
index <- c(1:n)                                    #To draw a plot
df <- data.frame(Tot,index)                        #for plot
ggplot()+geom_point(data=df,aes(x=index,y=Tot,col=ifelse(Tot<median(Tot),"Red","Blue")),pch=19)   #For plot
y <- ifelse(Tot<median(Tot),0,1)                   #Dichotomous variable involves classes
mydata <- data.frame(y,X)
#-------------------------------------------------------------------------------
#VARIABLE SELECTION WITH LASSO--------------------------------------------------
#perform k-fold cross-validation to find optimal lambda value
dimX <- dim(X)
p    <- dimX[2] 
cv_model <- cv.glmnet(X, y, alpha = 1)                         #find optimal lambda value that minimizes test MSE
opt_lam  <- cv_model$lambda.min
plot(cv_model)                                                 #To see k-fold process
lasso_model <- glmnet(X, y, alpha = 1, lambda = opt_lam)       #Estimate lasso model
lass_coef_indx   <- lasso_model$beta@i                         #Estimated coefs.
selected_X <- X[,lass_coef_indx]                               #selected Covariates
mydata2    <- data.frame(y,selected_X)
#---------------------------------------------------------------------------------
#PARTITION DATA AS TRANING AND TEST-----------------------------------------------
trainIndex = createDataPartition(mydata$y, p = 0.7)$Resample1  #70% data for traning
train = mydata[trainIndex, ]                                   
test = mydata[-trainIndex, ]                                   #30% for test
head(train)                                                    #first 6 entries of training
head(test)                                                     #first 6 entries of test
#PARTITION TO SELECTED VARIABLES------------------------------------------------
trainIndex2 = createDataPartition(mydata2$y, p = 0.7)$Resample1  #70% data for traning
train2 = mydata2[trainIndex2, ]                                   
test2 = mydata2[-trainIndex2, ]                                  #30% for test
#---------------------------------------------------------------------------------
#CLASSIFICATION WITH MACHINE LEARNING METHODS (WITH & WITHOUT VARIABLES SELECTION)
#1. Naive Bayes Classifier
NB_class  <- naiveBayes(y ~ ., data = train)                    #Classification model estimation (without model selection)
NB_class2 <- naiveBayes(y ~ ., data = train2)                   #Classification model estimation (with model selection)
NB_class                               
NB_class2
# Predicting on test data'
y_pred  <- predict(NB_class, newdata = test)                    #Predications without variable selection case
y_pred2 <- predict(NB_class2, newdata = test2)                  #Predications with variable selection case
# Confusion Matrix
cm  <- table(test$y, y_pred)                                    
cm2 <- table(test2$y, y_pred2)                                  

# Model Evaluation
confusionMatrix(cm)                                             #Confusion Matrix for the first case
confusionMatrix(cm2)                                            #Confusion Matrix for the second case

