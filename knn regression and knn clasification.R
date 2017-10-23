#Machine Learning Assignment 1
#Chaoran Zhang Session 1

#installIfAbsentAndLoad <- function(neededVector) {
#  for(thispackage in neededVector) {
#    if( ! require(thispackage, character.only = T) )
#    { install.packages(thispackage)}
#    require(thispackage, character.only = T)
#  }
#}

#needed  <-  c("class")  #class contains the knn() function
#installIfAbsentAndLoad(needed)
#require(FNN)


#####################
#knn regression
#####################
rm(list=ls())

table_hm_prices <- read.table('HomePrices.txt', sep = '\t', header = T)

mean((table_hm_prices$medv - mean(table_hm_prices$medv))^2)   #MSE predicting the mean of medv column
var(table_hm_prices$medv) * (nrow(table_hm_prices) - 1) / nrow(table_hm_prices)

table_hm_prices <- data.frame(scale(table_hm_prices[-13], center = T), table_hm_prices$medv)

set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(table_hm_prices)
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)    #setting row numbers for 3 sets

trainset <- table_hm_prices[train,]
validateset <- table_hm_prices[validate,]
testset <- table_hm_prices[test,]     #set partitioning based on random sampling with seed = 5072

train.x <- trainset[-13]
train.y <- trainset$table_hm_prices.medv
validate.x <- validateset[-13]
validate.y <- validateset$table_hm_prices.medv
test.x <- testset[-13]
test.y <- testset$table_hm_prices.medv  #model feeds, validations, and testors

print(head(train.x,1))
print(head(validate.x,1))
print(head(test.x,1))

numreps <- seq(1, 19, 2)
validate.errors <- rep(0, length(numreps))
train.errors <- rep(0, length(numreps))

for(k in numreps) {
  knn.pred <- as.vector(knn.reg(train.x, validate.x, train.y, k = k)$pred)
  validate.errors[match(k, numreps)] <- mean((validate.y - knn.pred)^2)
  
  knn.pred <- as.vector(knn.reg(train.x, train.x, train.y, k = k)$pred)
  train.errors[match(k, numreps)] <- mean((train.y - knn.pred)^2)
}

print(paste("Minimum validate set error rate occurred at k =", numreps[which.min(validate.errors)]))
print(paste("Minimum validate error rate was ", validate.errors[which.min(validate.errors)]))

print(paste("Minimum training set error rate occurred at k =", numreps[which.min(train.errors)]))
print(paste("Minimum training error rate was ", train.errors[which.min(train.errors)]))

plot(NULL,NULL,type='n',xlim=c(19,1), ylim=c(0,max(c(validate.errors, train.errors))),xlab='Increasing Flexibility (Decreasing k)',ylab='Validation Error Rate',main='Validation Error Rates as a function of \n Flexibility for KNN Prediction')
lines(rev(seq(1,19,2)),validate.errors[order(length(validate.errors):1)],type='b', pch=16, col=2)
lines(rev(seq(1,19,2)),train.errors[order(length(train.errors):1)], type='b', pch=16, col=1)
legend('topright', legend = c("Validate MSE", "Training MSE"), lty=c(1,1),lwd=c(2.5,2.5), col=c('red','black'))

knn.pred <- knn.reg(train.x, test.x,  train.y, k = numreps[which.min(validate.errors)])$pred
test.errors <- mean((knn.pred - test.y)^2)
print(paste("Test MSE for k = 3 is: ", test.errors, " comparing with the Validate MSE: ", validate.errors[which.min(validate.errors)]))


#####################
#knn classification
#####################
rm(list=ls())

table_loan <- read.csv('LoanData.csv', sep = ',')
table_loan <- data.frame(scale(table_loan[-8], center = T), table_loan$loan.repaid)
head(table_loan, 6)

print(paste('The error rate that would result from always predicting Yes is: ', nrow(table_loan[table_loan$table_loan.loan.repaid == 'No',]) / nrow(table_loan)))

set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(table_loan)
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)    #setting row numbers for 3 sets

trainset <- table_loan[train,]
validateset <- table_loan[validate,]
testset <- table_loan[test,]     #set partitioning based on random sampling with seed = 5072

train.x <- trainset[-8]
train.y <- trainset$table_loan.loan.repaid
validate.x <- validateset[-8]
validate.y <- validateset$table_loan.loan.repaid
test.x <- testset[-8]
test.y <- testset$table_loan.loan.repaid  #model feeds, validations, and testors

print(head(train.x,1))
print(head(validate.x,1))
print(head(test.x,1))

numreps <- seq(1, 19, 2)
validate.errors <- rep(0, length(numreps))
train.errors <- rep(0, length(numreps))

for(k in numreps) {
  knn.pred <- knn(train.x, validate.x, train.y, k = k)
  cf_mtx_loan <- table(validate.y, knn.pred)
  validate.errors[match(k, numreps)] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = k)
  train.errors[match(k, numreps)] <- mean(train.y != knn.pred)    # a simpler way to compute the error rate
}

print(paste("Minimum validate set error rate occurred at k =", numreps[which.min(validate.errors)]))
print(paste("Minimum validate error rate was ", validate.errors[which.min(validate.errors)]))

print(paste("Minimum training set error rate occurred at k =", numreps[which.min(train.errors)]))
print(paste("Minimum training error rate was ", train.errors[which.min(train.errors)]))

plot(NULL,NULL,type='n',xlim=c(19,1), ylim=c(0, max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)',ylab='Validation Error Rate',main='Validation Error Rates as a function of \n Flexibility for KNN Prediction')
lines(rev(seq(1,19,2)),validate.errors[order(length(validate.errors):1)],type='b', pch=16, col=2)
lines(rev(seq(1,19,2)),train.errors[order(length(train.errors):1)], type='b', pch=16, col=1)
legend('bottomleft', legend = c("Validate MSEs", "Training MSEs"), lty=c(1,1),lwd=c(2.5,2.5), col=c('red','black'))

knn.pred <- knn(train.x, test.x,  train.y, k = numreps[which.min(validate.errors)])
cf_mtx_loan <- table(test.y, knn.pred)
test.error <- (cf_mtx_loan["Yes", "No"] + cf_mtx_loan["No", "Yes"]) / sum(cf_mtx_loan)
print(paste("Test set error rate was ", test.error))

print(paste("Test MSE for k = 11 is: ", test.error, " comparing with the Validate MSE: ", validate.errors[which.min(validate.errors)]))

#####################
#investigation of MSE for first knn regression
#####################
rm(list=ls())

table_hm_prices <- read.table('HomePrices.txt', sep = '\t', header = T)
table_hm_prices <- data.frame(scale(table_hm_prices[-13], center = T), table_hm_prices$medv)

opt_vald_errors <- c()
test.errors_with_opt_k <- c()
numloops <- 50
set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(table_hm_prices)

for(i in 1:numloops){

  train <- sample(n, trainprop * n)
  validate <- sample(setdiff(1:n, train), validateprop * n)
  test <- setdiff(setdiff(1:n, train), validate)    #setting row numbers for 3 sets
  
  trainset <- table_hm_prices[train,]
  validateset <- table_hm_prices[validate,]
  testset <- table_hm_prices[test,]     #set partitioning based on random sampling with seed = 5072
  
  train.x <- trainset[-13]
  train.y <- trainset$table_hm_prices.medv
  validate.x <- validateset[-13]
  validate.y <- validateset$table_hm_prices.medv
  test.x <- testset[-13]
  test.y <- testset$table_hm_prices.medv  #model feeds, validations, and testors
  
  numreps <- seq(1, 19, 2)
  validate.errors <- rep(0, length(numreps))
  train.errors <- rep(0, length(numreps))
  
  for(k in numreps) {
    knn.pred <- as.vector(knn.reg(train.x, validate.x, train.y, k = k)$pred)
    validate.errors[match(k, numreps)] <- mean((validate.y - knn.pred)^2)
  }
  opt_k <- numreps[which.min(validate.errors)]
  opt_vald_errors[i] <- validate.errors[which.min(validate.errors)]

  knn.pred <- as.vector(knn.reg(train.x, test.x, train.y, k = opt_k)$pred)
  test.errors_with_opt_k[i] <- mean((test.y - knn.pred)^2)
  
}

print(paste('Mean of validate errors is: ', mean(opt_vald_errors)))
print(paste('Standard deviation of validate errors is: ', sd(opt_vald_errors)))
print(paste('Mean of test errors is: ', mean(test.errors_with_opt_k)))
print(paste('Standard deviation of test errors is: ', sd(test.errors_with_opt_k)))

plot(NULL,NULL,type='n',xlim=c(1, 50), ylim=c(0,max(c(opt_vald_errors, test.errors_with_opt_k))),xlab='Increasing Flexibility (Decreasing k)',ylab='Validation Error Rate',main='Validation Error Rates as a function of \n Flexibility for KNN Prediction')
lines(1:numloops, opt_vald_errors[order(1:length(opt_vald_errors))],type='b', pch=16, col=2)
lines(1:numloops, test.errors_with_opt_k[order(1:length(test.errors_with_opt_k))], type='b', pch=16, col=1)
lines(1:numloops, rep(mean(opt_vald_errors),50), type='l', pch=16, col=2)
lines(1:numloops, rep(mean(test.errors_with_opt_k),50), type='l', pch=16, col=1)
legend('topright', legend = c("Validate MSEs & Mean", "Test MSEs & Mean"), lty=c(1,1),lwd=c(1,1),cex=0.7, col=c('red','black'))


#####################
#knn regression
#####################

rm(list=ls())

college_data <- read.csv('applications.train.csv', sep = ',')

set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(college_data)
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)    #setting row numbers for 3 sets

trainset <- college_data[train,]
validateset <- college_data[validate,]
testset <- college_data[test,]     #set partitioning based on random sampling with seed = 5072

train.x <- trainset[-1]
train.y <- trainset$Applications
validate.x <- validateset[-1]
validate.y <- validateset$Applications

numreps <- 1:50
validate.errors <- c()

for(k in numreps) {
  
  knn.pred <- as.vector(knn.reg(train.x, validate.x, train.y, k = k)$pred)
  validate.errors[k] <- mean((validate.y - knn.pred)^2)
  
}

print(paste('By using all features, the minimum validate MSE is: ', 
            validate.errors[which.min(validate.errors)], ' with k = ', 
                            which.min(validate.errors),'.'))
