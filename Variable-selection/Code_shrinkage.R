#VARIABLE SELECTION for CarSaLES dataset---------------------------------------

#Three shrinkage methods and one Post-Selection methods 
#are used: Lasso, Adaptive Lasso, SCAD (These are conventional), 
#PSE based weighted-ridge (Post selection - proposed by Ahmed et al.2016)
#------------------------------------------------------------------------------
#Required packages------------------------------------
library(glmnet)
library(ncvreg)
library(Rlab)
library(pracma)
library(psych)
library(pracma)
library(ggplot2)
library(gridExtra)
library(reshape)
library(corrplot)
library(MASS)
library(tidyverse)
library(GGally)
library(stats)
library(cowplot)
library(latticeExtra) 
library(lattice)
library(viridis)
library(plotly)
library(heatmaply)
#FUNCTIONS----------------------------------------------------------------------
adalasso<-function(x,y){
  #ymean <- mean(y)
  #  y <- y-mean(y)  
  #  xmean <- colMeans(x)
  #  xnorm <- sqrt(n-1)*apply(x,2,sd)
  #  x <- scale(x, center = xmean, scale = xnorm)
  
  #fit ols 
  lm.fit <- lm(y ~ x)
  beta.init <- coef(lm.fit)[-1] # exclude 0 intercept
  
  # calculate weights
  w  <- abs(beta.init)  
  x2 <- scale(x, center=FALSE, scale=1/w)  
  
  la_eq <- cv.glmnet(x2,y,nfolds=20,intercept=F,lambda=seq(3,10,length.out=100))
  coefs <- coef(la_eq,c(la_eq$lambda[la_eq$index[1]]))
  S1a  <- coefs@i+1                              #Subset of Strong Signals (index)
  plot(la_eq)
  S1r  <- coefs@i
  if ((length(S1a))>n){
    S1_beta <- coefs@x[1:n]
    S1 <- S1r[1:n]
  } else {
    S1_beta <- coefs@x
    S1 <- S1r
  }
  S1_beta <- coefs@x
  X_S1 <- x2[,c(S1)]                               #Subset of Strong signals
  X_S1c <- x[,-S1]                                #Subset of complement
  alas <- new.env()
  
  alas$X_S1    <- X_S1
  alas$X_S1c   <- X_S1c
  alas$S1_beta <- S1_beta
  alas$S1      <- S1
  alas$coefs   <- coefs
  return(alas)
}
#Lasso--------------------------------------------------------------------------
lasso   <- function(x,y){
  #ymean <- mean(y)
  #y <- y-mean(y)  
  #xmean <- colMeans(x)
  #xnorm <- sqrt(n-1)*apply(x,2,sd)
  #x <- scale(x, center = xmean, scale = xnorm)
  n <- length(y)
  la_eq <- cv.glmnet(x,(y),nfolds=10,intercept=F,lambda=seq(3,10,length.out=100))
  coefs <- coef(la_eq,c(la_eq$lambda[la_eq$index[1]]))
  plot(la_eq)
  S1a  <- coefs@i+1                   #Subset of Strong Signals (index)
  S1r  <- coefs@i
  if ((length(S1a))>n){
    S1_beta <- coefs@x[1:n]
    S1 <- S1r[1:n]
  } else {
    S1_beta <- coefs@x
    S1 <- S1r
  }
  
  
  #S1_beta <- coefs@x
  X_S1 <- x[,S1]                               #Subset of Strong signals
  X_S1c <- x[,-S1]                                #Subset of complement
  las <- new.env()
  
  las$X_S1    <- X_S1
  las$X_S1c   <- X_S1c
  las$S1_beta <- S1_beta
  las$S1      <- S1
  las$coefs   <-coefs 
  
  return(las)
}

#-------------------------------------------------------------------------------
data    <- read.csv("Car_sales.csv",header=T)
data    <- na.omit(data) 
summary(data)
y <- data$Sales_in_thousands                  #Dependent (response) variable
n <- length(y)
x <- data[,-c(3,15)]                          #Matrix for independent variables
x <- data.matrix(x)

#-------------------------------------------------------------------------------
lasso_obj    <- lasso(x,y)
ad.lasso_obj <- adalasso(x,y)

Predictors_clean_Lasso          <- lasso_obj$X_S1
Predictors_clean_Adaptive.Lasso <- lasso_obj$X_S1

write.csv(Predictors_clean_Lasso,"Lasso.csv")
write.csv(Predictors_clean_Adaptive.Lasso,"adaptive.Lasso.csv")
