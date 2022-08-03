a <- set.seed(1234)
n <- 100

#----------------------------------------
betah <- matrix(0,2,1000)
for (i in 1:1000){
x <- matrix(c(runif(n),runif(n)),n,2)
beta <- c(1,2)
resid <- rnorm(n)
y <- x%*%beta+resid
  
model <- lm(y~x)
betah[,i] <- model$coefficients[2:3]
a <- a+1
}

beta