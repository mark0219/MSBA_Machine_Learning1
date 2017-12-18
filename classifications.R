###########
#Question1#
###########

require(MASS)
require(ISLR)
require(class)
require(boot)

rm(list=ls())

#a.
set.seed(5072)

#b.
my_table <- Weekly
log.reg.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = my_table, family = binomial)
summary(log.reg.fit)
#At the significance level of alpha = 0.05, only Lag2 variable is significant.

#c.
glm.probs <- predict(log.reg.fit, type = 'response')
glm.pred <- rep('Down', nrow(my_table))
glm.pred[glm.probs > 0.5] <- 'Up'
cf_mtx_all_predictors <- table(my_table$Direction, glm.pred)
(cf_mtx <- cf_mtx_all_predictors)

#d.
print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

#e. Logistic
train_set <- my_table[my_table$Year < 2009,]
test_set <- my_table[my_table$Year > 2008,]
log.reg.fit.only.lag2 <- glm(Direction ~ Lag2, data = train_set, family = binomial)

#f.
#Matrix is created for the comparision in step k.
result_mtx <- matrix(ncol = 6)
colnames(result_mtx) <- c('Correct_Rate', 
                          'Error_Rate', 
                          'Type-I_Error_Rate', 
                          'Type-II_Error_Rate',
                          'Power',
                          'Precision')

only.lag2.probs <- predict(log.reg.fit.only.lag2, test_set, type = 'response')
only.lag2.pred <- rep('Down', nrow(test_set))
only.lag2.pred[only.lag2.probs > 0.5] <- 'Up'
cf_mtx_lag2 <- table(test_set$Direction, only.lag2.pred)
(cf_mtx <- cf_mtx_lag2)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)
result_mtx <- result_mtx[-1,]

#g. LDA
lda.fit <- lda(Direction ~ Lag2, data = train_set)
lag2.lda.pred <- predict(lda.fit, test_set)$class
cf_mtx_lda <- table(test_set$Direction, lag2.lda.pred)
(cf_mtx <- cf_mtx_lda)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#By excuting below line of code, one can tell that the logistic regression and the LDA have the same prediction result.
only.lag2.pred == lag2.lda.pred

#h. QDA
qda.fit <- qda(Direction ~ Lag2, data = train_set)
qda.fit.pred <- predict(qda.fit, test_set)$class
cf_mtx_qda <- table(test_set$Direction, qda.fit.pred)
(cf_mtx <- cf_mtx_qda)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#i. KNN_1
knn.pred.1 <- knn(data.frame(train_set$Lag2), data.frame(test_set$Lag2), train_set$Direction, k = 1)
cf_mtx_knn.1 <- table(test_set$Direction, knn.pred.1)
(cf_mtx <- cf_mtx_knn.1)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#j. KNN_5
knn.pred.5 <- knn(data.frame(train_set$Lag2), data.frame(test_set$Lag2), train_set$Direction, k = 5)
cf_mtx_knn.5 <- table(test_set$Direction, knn.pred.5)
(cf_mtx <- cf_mtx_knn.5)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)
#Add row names

#k
rownames(result_mtx) <- c('Logistic_Regression', 'LDA', 'QDA', 'KNN_1', 'KNN_5')
result_mtx[order(-result_mtx[,1]),]
#Both Logistic Regression and LDA have the hightst overall correct rate.


###########
#Question2#
###########
rm(list=ls())
set.seed(5072)

#a.
car_table <- Auto

#b.
mpg01 <- rep(0, nrow(car_table))
mpg01[car_table$mpg > median(car_table$mpg)] <- 1
car_table$mpg01 <- mpg01
car_table <- car_table[,-1]

#c.
n <- nrow(car_table)
trainprop <- 0.8
testprop <- 0.2
train <- sample(n, trainprop * n)
test <- setdiff(1:n, train)

train_set <- car_table[train,]
test_set <- car_table[test,]

#d. Logistic
log.fit <- glm(mpg01 ~ cylinders + displacement + weight, data = train_set, family = binomial)

#e.
log.fit.probs <- predict(log.fit, test_set, type = 'response')
log.fit.pred <- rep(0, nrow(test_set))
log.fit.pred[log.fit.probs > 0.5] <- 1
cf_mtx_log <- table(test_set$mpg01, log.fit.pred)
(cf_mtx <- cf_mtx_log)

#Matrix is created for the comparision in step k.
result_mtx <- matrix(ncol = 6)
colnames(result_mtx) <- c('Correct_Rate', 
                          'Error_Rate', 
                          'Type-I_Error_Rate', 
                          'Type-II_Error_Rate',
                          'Power',
                          'Precision')

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)
result_mtx <- result_mtx[-1,]

#f. LDA
lda.fit <- lda(mpg01 ~ cylinders + displacement + weight, data = train_set)
lda.pred <- predict(lda.fit, test_set)$class
cf_mtx_lda <- table(test_set$mpg01, log.fit.pred)
(cf_mtx <- cf_mtx_lda)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#g. QDA
qda.fit <- qda(mpg01 ~ cylinders + displacement + weight, data = train_set)
qda.fit.pred <- predict(qda.fit, test_set)$class
cf_mtx_qda <- table(test_set$mpg01, qda.fit.pred)
(cf_mtx <- cf_mtx_qda)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#h. KNN_1
knn.pred.1 <- knn(data.frame(train_set$cylinders, train_set$displacement, train_set$weight),
                  data.frame(test_set$cylinders, test_set$displacement, test_set$weight), 
                  train_set$mpg01, k = 1)
cf_mtx_knn.1 <- table(test_set$mpg01, knn.pred.1)
(cf_mtx <- cf_mtx_knn.1)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#i KNN_K
error.rates <- c()
for (i in 1:50) {
  knn.pred.k <- knn(data.frame(train_set$cylinders, train_set$displacement, train_set$weight),
                    data.frame(test_set$cylinders, test_set$displacement, test_set$weight), 
                    train_set$mpg01, k = i)
  error.rates[i] <- mean(test_set$mpg01 != knn.pred.k)
}
print(paste('The minimum error rate:', error.rates[which.min(error.rates)], 'occur at K =', which.min(error.rates)))

knn.pred.3 <- knn(data.frame(train_set$cylinders, train_set$displacement, train_set$weight),
                  data.frame(test_set$cylinders, test_set$displacement, test_set$weight), 
                  train_set$mpg01, k = 3)
cf_mtx_knn.3 <- table(test_set$mpg01, knn.pred.3)
(cf_mtx <- cf_mtx_knn.3)

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)

#j
rownames(result_mtx) <- c('Logistic_Regression', 'LDA', 'QDA', 'KNN_1', 'KNN_3')
result_mtx[order(-result_mtx[,1]),]
#By inspecting the table, QDA has the highest overall correct rate.


###########
#Question3#
###########

rm(list=ls())
set.seed(5072)     #Dr. Murray's favorite number
bos_table <- Boston[c(1,5,8,9)]

bos_table[bos_table$crim > median(bos_table$crim),]$crim <- 1   #crime rate above median
bos_table[bos_table$crim != 1,]$crim <- 0   #crime rate below median
bos_table$crim <- as.factor(bos_table$crim)

#Above Median as 1, Below Median as 0.
contrasts(bos_table$crim)

n <- nrow(bos_table)
trainprop <- 0.8
testprop <- 1 - trainprop
train <- sample(n, trainprop*n)
test <- setdiff(1:n, train)

train_set <- bos_table[train,]
test_set <- bos_table[test,]

#Logistic
log.reg.fit <- glm(crim ~ ., data = train_set, family = binomial)
log.reg.probs <- predict(log.reg.fit, test_set, type = 'response')
log.reg.pred <- rep('0', nrow(test_set))
log.reg.pred[log.reg.probs > 0.5] <- 1

cf_mtx_log <- table(test_set$crim, log.reg.pred)
(cf_mtx <- cf_mtx_log)

result_mtx <- matrix(ncol = 6)
colnames(result_mtx) <- c('Correct_Rate', 
                          'Error_Rate', 
                          'Type-I_Error_Rate', 
                          'Type-II_Error_Rate',
                          'Power',
                          'Precision')

print(paste('The overall fraction of correct predictions is:', (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)))
print(paste('The overall error rate is', (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)))
print(paste('The Type-I error rate is:', cf_mtx[1,2]/sum(cf_mtx[1,])))
print(paste('The Type-II error rate is:', cf_mtx[2,1]/sum(cf_mtx[2,])))
print(paste('The Power of the model is:', cf_mtx[2,2]/sum(cf_mtx[2,])))
print(paste('The Precision of the model is:', cf_mtx[2,2]/sum(cf_mtx[,2])))

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))

result_mtx <- rbind(result_mtx, row_temp)
result_mtx <- result_mtx[-1,]

#LDA
lda.fit <- lda(crim ~ ., data = train_set)
lda.pred <- predict(lda.fit, test_set)$class
cf_mtx_lda <- table(test_set$crim, lda.pred)
(cf_mtx <- cf_mtx_lda)

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))
result_mtx <- rbind(result_mtx, row_temp)

#KNN_K
error.rates <- c()
for (i in 1:50) {
  knn.pred.k <- knn(data.frame(train_set$nox, train_set$dis, train_set$rad),
                    data.frame(test_set$nox, test_set$dis, test_set$rad), 
                    train_set$crim, k = i)
  error.rates[i] <- mean(test_set$crim != knn.pred.k)
}
print(paste('The minimum error rate:', error.rates[which.min(error.rates)], 'occur at K =', which.min(error.rates)))

knn.pred.1 <- knn(data.frame(train_set$nox, train_set$dis, train_set$rad),
                  data.frame(test_set$nox, test_set$dis, test_set$rad), 
                  train_set$crim, k = 1)

cf_mtx_knn.1 <- table(test_set$crim, knn.pred.1)
(cf_mtx <- cf_mtx_knn.1)

row_temp <- c((cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx), 
              (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx),
              cf_mtx[1,2]/sum(cf_mtx[1,]),
              cf_mtx[2,1]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[2,]),
              cf_mtx[2,2]/sum(cf_mtx[,2]))
result_mtx <- rbind(result_mtx, row_temp)

rownames(result_mtx) <- c('Logistic Regression', 'LDA', 'KNN_1')
result_mtx[order(-result_mtx[,1]),]

#By inspecting the table, the KNN classifier with K = 1 yields the highest overall correct rate; The LDA yields the second highest
#correct rate and the Logistic Regression yields the lowest correct rate.


###########
#Question4#
###########

rm(list = ls())

#a.
set.seed(5072)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

#b.
my_table <- data.frame(x, y)

#c.
plot(my_table$x, my_table$y, pch = 16)

#d.
set.seed(123)

cv.errors <- c()
for (i in 1:4) {
  poly.reg.ith <- glm(y ~ poly(x, i), data = my_table)
  cv.errors[i] <- cv.glm(my_table, poly.reg.ith)$delta[1]
}
print(paste('The minimum error:', cv.errors[which.min(cv.errors)], 'occurs with', which.min(cv.errors), 'degree of polynomial.'))

#e.
set.seed(456)

cv.errors <- c()
for (i in 1:4) {
  poly.reg.ith <- glm(y ~ poly(x, i), data = my_table)
  cv.errors[i] <- cv.glm(my_table, poly.reg.ith)$delta[1]
}
print(paste('The minimum error:', cv.errors[which.min(cv.errors)], 'occurs with', which.min(cv.errors), 'degree of polynomial.'))

#Since LOOCV method tries every single row exahstively and then take the average for all the errors to validate the model, 
#therefore, the order for which each individual row is selected does not affect the final result. Thus changing the random seed
#does not affect our test error.

#f.
#By looking at the plot, we can see that there is an obvious quadratic relationship between x and y. Therefore, I expected that
#the 2nd degree polynomial, which is a quadratic model would have the lowest test error. As my result shows, the degree for which
#the polynomial model gives the minimum test error is 2.

#g.
for (i in 1:4) {
  poly.reg.ith <- glm(y ~ poly(x, i), data = my_table)
  print(coef(summary(poly.reg.ith)))
}

#By inspecting the p-values print out for each predictor in each different degree of polynomials, we can see that the quadratic 
#term x^2 has the lowest p-value. In other words, the 2nd degree term in each model is the most statistically significant term
#at the level of confidence alpha = 0.05. This result is consistent with the result from LOOCV in step d, which shows that
#the the minimum test error occurs when the degree of polynomial is 2.
