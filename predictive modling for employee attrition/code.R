require(class)
require(glmnet)
require(MASS)
require(boot)

######################
#Attrition Prediction#
######################
rm(list=ls())

#Read file
table_hr <- read.csv('WA_Fn-UseC_-HR-Employee-Attrition.csv', sep = ',')
n <- nrow(table_hr)
table_hr <- table_hr[sample(1:n, n),]
colnames(table_hr)[1] <- 'Age'
table_hr_left <- table_hr[table_hr$Attrition == 'Yes',]

#Exploratory Analysis: Job Dimention

par(mfrow = c(2,2))

barplot(c(length(table_hr_left$JobRole[table_hr_left$JobRole == 'Sales Executive']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Laboratory Technician']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Sales Representative']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Research Scientist']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Research Director']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Human Resources']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Healthcare Representative']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Manufacturing Director']),
       length(table_hr_left$JobRole[table_hr_left$JobRole == 'Manager'])),
     names.arg = c('Sales Exe','Lab Technician','Sales Rep',
                     'Research Sci','Research Dir','H& R',
                     'Hth Rep','Manuf Dir','Manager'),
       main = 'Former Employee Job Roles')

barplot(c(length(table_hr_left$Department[table_hr_left$Department == 'Human Resources']), 
          length(table_hr_left$Department[table_hr_left$Department == 'Research & Development']),
          length(table_hr_left$Department[table_hr_left$Department == 'Sales'])), 
        names.arg = c('Human Resources', 'R & D', 'Sales'), 
        main = 'Former Employee Departments',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobLevel[table_hr_left$JobLevel == 1]), 
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 2]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 3]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 4]),
          length(table_hr_left$JobLevel[table_hr_left$JobLevel == 5])), 
        names.arg = c(1,2,3,4,5), 
        main = 'Former Employee Job Levels',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$OverTime[table_hr_left$OverTime == 'Yes']), 
          length(table_hr_left$OverTime[table_hr_left$OverTime == 'No'])), 
        names.arg = c('Yes', 'No'), 
        main = 'Former Employee Overtime Status',
        ylab = 'Frequency')

par(mfrow = c(1,1))

#Exploratory Analysis: Personal Dimention

par(mfrow = c(2,2))

hist(table_hr_left$MonthlyIncome, breaks = 50, xlab = 'Montyly Income', 
     main = 'Former Employee Monthly Income Distribution')

hist(table_hr_left$Age, breaks = 50, xlab = 'Age', main = 'Age')

barplot(c(length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Travel_Frequently']), 
          length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Travel_Rarely']),
          length(table_hr_left$BusinessTravel[table_hr_left$BusinessTravel == 'Non-Travel'])), 
        names.arg = c('Frequently', 'Rarely', 'None'), 
        main = 'Former Employee Business Travel Status',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 1]), 
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 2]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 3]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 4])), 
        names.arg = c(1,2,3,4), 
        main = 'Former Employee Job Involvement',
        ylab = 'Frequency')

par(mfrow = c(1,1))

#Exploratory Analysis: Career Development Dimention

par(mfrow = c(2,2))

barplot(c(length(table_hr_left$EducationField[table_hr_left$EducationField == 'Human Resources']), 
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Life Science']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Marketing']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Mecidal']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Other']),
          length(table_hr_left$EducationField[table_hr_left$EducationField == 'Technical Degree'])), 
        names.arg = c('H & R', 'Life Sci', 'Marketing', 'Mecidal', 'Other', 'Tech.Degree'), 
        main = 'Former Employee Education Fields',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 1]), 
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 2]),
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 3]),
          length(table_hr_left$JobSatisfaction[table_hr_left$JobSatisfaction == 4])), 
        names.arg = c(1,2,3,4), 
        main = 'Former Employee Job Satisfaction',
        ylab = 'Frequency')

barplot(c(length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 0]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 1]), 
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 2]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 3]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 4]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 5]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 6]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 7]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 8]),
          length(table_hr_left$JobInvolvement[table_hr_left$JobInvolvement == 9])), 
        names.arg = c(0,1,2,3,4,5,6,7,8,9), 
        main = 'Former Employee"s Number of Companies Worked',
        ylab = 'Frequency')

hist(table_hr_left$YearsAtCompany, breaks = 100, main = 'Former Employee"s Years With Company', xlab = NULL)

par(mfrow = c(1,1))

#Feature re-engineering
table_hr <- table_hr[, -c(9,22,27)]    #Taking out 3 features that have only one value
X <- model.matrix(Attrition ~ ., data = table_hr)[,-c(1, 25)]     #Spliting factor type features with multiple levels into dummy variables,
y <- table_hr$Attrition                                           #also removing the feature "JobRole.ResearchDirector" as it causes problem to QDA.
table_hr <- data.frame(X, y)          #Taking out the intercept column
colnames(X) <- colnames(table_hr)[-length(colnames(table_hr))]    #Fixing column names for matrix X
numfolds <- 10
fold.indices <- cut(1:n, breaks = numfolds, labels=FALSE)      #For later mannual CV usage

#Creating a table to store confution matrix for each model
result_mtx <- matrix(ncol = 8)
colnames(result_mtx) <- c('Correct_Rate', 
                          'Error_Rate', 
                          'Type-I_Error', 
                          'Type-II_Error',
                          'CV_Type_II_Std',
                          'Power',
                          'Precision',
                          'CV_Precision_Std')

######################################
#Lasso Regression (Feature Selection)#
######################################
grid <- 10 ^ seq(10, -2, length = 100)

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- X[test.indices, ]
  train.data <- X[-test.indices, ]               
  Ls.Reg <- glmnet(train.data, y[-test.indices], alpha = 1, lambda = grid, family = 'binomial')
  cv.out <- cv.glmnet(train.data, y[-test.indices], alpha = 1, family = "binomial")
  bestlam <- cv.out$lambda.min
  Ls.pred <- predict(Ls.Reg, s = bestlam, newx = test.data, type = "class")
  cf_mtx <- table(y[test.indices], Ls.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              sd(type_II),
              mean(model_power),
              mean(precision),
              sd(precision))
result_mtx <- rbind(result_mtx, row_temp)

#Fitting model over the entire dataset and extract the best subset
out <- glmnet(X, y, alpha = 1, lambda = grid, family = 'binomial')
Ls.coefs <- predict(out, type = 'coefficients', s = bestlam)[1:45,]
Ls.coefs[Ls.coefs!=0][-1]
coef_names <- names(Ls.coefs[Ls.coefs!=0][-1])

#Subsetting data set by extracted coefficient names
table_hr_new <- data.frame(table_hr[coef_names], table_hr$y)
colnames(table_hr_new)[27] <- 'Attrition'

#####################
#Logistic Regression#
#####################

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices, ]
  train.data <- table_hr_new[-test.indices, ]               
  log.fit = glm(Attrition ~ ., data = train.data, family = binomial)             
  log.pred <- rep('No', nrow(test.data))
  log.probs <- predict(log.fit, test.data, type = 'response')
  log.pred[log.probs > 0.5] <- 'Yes'
  cf_mtx <- table(test.data$Attrition, log.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              sd(type_II),
              mean(model_power),
              mean(precision),
              sd(precision))
result_mtx <- rbind(result_mtx, row_temp)

###########
#LDA Model#
###########

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices, ]
  train.data <- table_hr_new[-test.indices, ]                  
  lda.fit <- lda(Attrition ~ ., data = train.data, family = binomial)             
  lda.pred <- predict(lda.fit, test.data)$class
  cf_mtx <- table(test.data$Attrition, lda.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              sd(type_II),
              mean(model_power),
              mean(precision),
              sd(precision))
result_mtx <- rbind(result_mtx, row_temp)

###########
#QDA Model#
###########

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)        
  test.data <- table_hr_new[test.indices,]
  train.data <- table_hr_new[-test.indices,]                  
  qda.fit <- qda(Attrition ~ ., data = train.data, family = binomial)             
  qda.pred <- predict(qda.fit, test.data)$class
  cf_mtx <- table(test.data$Attrition, qda.pred)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              sd(type_II),
              mean(model_power),
              mean(precision),
              sd(precision))
result_mtx <- rbind(result_mtx, row_temp)

#####
#KNN#
#####

#Scaling data for KNN
table_hr_new_scaled <- as.matrix(scale(table_hr_new[-27], center = T))
attrition_vector <- table_hr_new$Attrition

#Fiding optimal K for KNN classifier

correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:50) {
  
  tpII_temp <- c()
  
  for (j in 1:numfolds) {
    test.indices <- which(fold.indices == j)
    test.data <- table_hr_new_scaled[test.indices, ]
    train.data <- table_hr_new_scaled[-test.indices, ]
    knn.pred.k <- knn(train.data, test.data, attrition_vector[-test.indices], k = i)
    cf_mtx <- table(y[test.indices], knn.pred.k)
    tpII_temp[j] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  }
  type_II[i] <- mean(tpII_temp)
}

print(paste('The minimum Type-II error rate:', type_II[which.min(type_II)], 'occur at K =', which.min(type_II)))
plot(type_II, pch = 16, type = 'b', xlab = 'Number of K', ylab = 'Test Mean of Type-II Error Rate')


#Running a KNN with k=1 and calculate the confusion matrix by using 10-folds cross validation
correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
model_power <- c()
precision <- c()

for (i in 1:numfolds) {
  test.indices <- which(fold.indices == i)
  test.data <- table_hr_new_scaled[test.indices, ]
  train.data <- table_hr_new_scaled[-test.indices, ]
  knn.pred.k <- knn(train.data, test.data, attrition_vector[-test.indices], k = 2)
  cf_mtx <- table(y[test.indices], knn.pred.k)
  correct_rate[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
  error_rate[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
  type_I[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
  type_II[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
  model_power[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
  precision[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
}

row_temp <- c(mean(correct_rate),
              mean(error_rate),
              mean(type_I),
              mean(type_II),
              sd(type_II),
              mean(model_power),
              mean(precision),
              sd(precision))
result_mtx <- rbind(result_mtx, row_temp)
result_mtx <- result_mtx[-1,]
rownames(result_mtx) <- c('Lasso','Logistic','LDA', 'QDA', 'KNN')
result_mtx

###################################
#Model performances interpretation#
###################################
#Overall Correct Rate: The total percentage of correct prediction.
#Overall Incorrect Rate: The total percentage of incorrect prediction.
#Type-I Error Rate: The percentage of false-positive prediction, which is the employees that would not leave but identified as leaving.
#Type-II Error Rate: The percentage of false-negative prediction, which is the employees that would leave but identified as not-leaving.
#Power: Percentage of employess whom has left that are correctly identified by the model.
#Precision: Percentage of employees that are correctly predicted as leaving.

#Visualizing compare models by type-II error
barplot(result_mtx[,4], ylab = 'Type-II Error Rate')

############Tunning The Model That Has Lowest Type-II Error###############
#####################
#Logistic Regression#
#####################
correct_rate <- c()
error_rate <- c()
type_I <- c()
type_II <- c()
CV_type_II_sd <- c()
model_power <- c()
precision <- c()
CV_precision_std <- c()
cf_mtx_logistic <- matrix(NA, ncol = 8, byrow = T, dimnames = list(c('Logistic Regression'),
                                                                                 c('Overall_Correct',
                                                                                   'Overall_Error',
                                                                                   'Type_I',
                                                                                   'Type_II',
                                                                                   'CV_Type_II_Std',
                                                                                   'Power',
                                                                                   'Precision',
                                                                                   'CV_Precision_Std')))

tuning_parameter <- seq(0.2, 0.8, 0.05)

for (j in 1:length(tuning_parameter)) {

  cor_cv <- c()
  e_r_cv <- c()
  type_I_cv <- c()
  type_II_cv <- c()
  m_p_cv <- c()
  prec_cv <- c()

  for (i in 1:numfolds) {
    test.indices <- which(fold.indices == i)        
    test.data <- table_hr_new[test.indices, ]
    train.data <- table_hr_new[-test.indices, ]               
    log.fit = glm(Attrition ~ ., data = train.data, family = binomial)             
    log.pred <- rep('No', nrow(test.data))
    log.probs <- predict(log.fit, test.data, type = 'response')
    log.pred[log.probs > tuning_parameter[j]] <- 'Yes'
    cf_mtx <- table(test.data$Attrition, log.pred)
    cor_cv[i] <- (cf_mtx[1,1] + cf_mtx[2,2])/sum(cf_mtx)
    e_r_cv[i] <- (cf_mtx[1,2] + cf_mtx[2,1])/sum(cf_mtx)
    type_I_cv[i] <- cf_mtx[1,2]/sum(cf_mtx[1,])
    type_II_cv[i] <- cf_mtx[2,1]/sum(cf_mtx[2,])
    m_p_cv[i] <- cf_mtx[2,2]/sum(cf_mtx[2,])
    prec_cv[i] <- cf_mtx[2,2]/sum(cf_mtx[,2])
  }
  
  cf_mtx_logistic <- rbind(cf_mtx_logistic, c(mean(cor_cv), mean(e_r_cv), mean(type_I_cv), mean(type_II_cv), sd(type_II_cv),  mean(m_p_cv), mean(prec_cv), sd(prec_cv)))
                           
  correct_rate[j] <- mean(cor_cv)
  error_rate[j] <- mean(e_r_cv)
  type_I[j] <- mean(type_I_cv)
  type_II[j] <- mean(type_II_cv)
  CV_type_II_sd[j] <- sd(type_II_cv)
  model_power[j] <- mean(m_p_cv)
  precision[j] <- mean(prec_cv)
  CV_precision_std[j] <- sd(prec_cv)
}

#Assigning row names for table of confusion matrix created while tuning the Logistic Regression model
cf_mtx_logistic <- cf_mtx_logistic[-1,]
rownames(cf_mtx_logistic) <- as.character(tuning_parameter)

#Plot for type-II vs overall correct rate
plot(NULL,NULL, type='n', xlim=c(0.2, 0.8), ylim=c(0,max(c(type_II, correct_rate))),xlab='Threshhold', ylab = 'Percentage Rate')
lines(tuning_parameter,type_II,type='b', pch=16, col=2)
lines(tuning_parameter,correct_rate, type='b', pch=16, col=1)
legend('bottomright', legend = c("Type-II", "Overall Correct"), lty=c(1,1),lwd=c(2.5,2.5), col=c('red','black'))

#Plot for precision vs overall correct rate
plot(NULL,NULL, type='n', xlim=c(0.2, 0.8), ylim=c(0,max(c(precision, correct_rate))),xlab='Threshhold', ylab = 'Percentage Rate')
lines(tuning_parameter,precision,type='b', pch=16, col=2)
lines(tuning_parameter,correct_rate, type='b', pch=16, col=1)
legend('bottomright', legend = c("Precision", "Overall Correct"), lty=c(1,1),lwd=c(2.5,2.5), col=c('red','black'))

