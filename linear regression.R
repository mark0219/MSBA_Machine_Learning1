#####################
#### QUESTION 1 ####
#####################
require(MASS)

rm(list=ls())

#a.
set.seed(5072)

#b.
x <- rnorm(100,0,1)

#c.
eps <- rnorm(100,0,sqrt(0.25))

#c-a.
y <- -1 + 0.5*x + eps      #beta0 = -1, beta1 = 0.5

#d.
length(y)

#e.
#Population beta0 is -1, population beta1 is 0.5

#f.
plot(x,y,pch=20)

#g.
#x and y has a relatively strong positive relationship. as x increases, y tends to increase 
#correspondently; the degree of linearity is pretty high, and variability is minor.

#h.
reg.lm_1 <- lm(y ~ x)

#i.
reg.summary <- summary(reg.lm_1)
beta0_hat <- coef(reg.summary)[1,1]
beta1_hat <- coef(reg.summary)[2,1]
print(paste('Beta0-hat for the linear regression model is: ', beta0_hat, ', and beta0 is: -1.'))
print(paste('Beta1-hat for the linear regression model is: ', beta1_hat, ', and beta1 is: 0.5.'))

#j.
abline(reg.lm_1)

#k.
y_true <- -1 + 0.5*x
abline(lm(y_true ~ x), col = 'red')

#l.
legend('topleft', legend = c("Population Regression", "Least Square Regression"), lty=c(1,1),lwd=c(2.5,2.5), col=c('black','red'))

#m.
x_squared <- x^2
reg.poly <- lm(y ~ x + x_squared)

#n.
anova(reg.lm_1, reg.poly)
#Since p-value is greter than significance level alpha=0.05, which means there is no improvement.

#o.
x <- rnorm(100,0,1)
eps <- rnorm(100,0,sqrt(0.1))
y <- -1 + 0.5*x + eps      #beta0 = -1, beta1 = 0.5
length(y)
plot(x,y,pch=20)
#x and y has a very strong positive relationship. as x increases, y increases as well; 
# the degree of linearity is high, and variability is minor.
reg.lm_2 <- lm(y ~ x)
reg.summary <- summary(reg.lm_2)
beta0_hat <- coef(reg.summary)[1,1]
beta1_hat <- coef(reg.summary)[2,1]
print(paste('Beta0-hat for the linear regression model is: ', beta0_hat, ', and beta0 is: -1.'))
print(paste('Beta1-hat for the linear regression model is: ', beta1_hat, ', and beta1 is: 0.5.'))
abline(reg.lm_2)
y_true <- -1 + 0.5*x
abline(lm(y_true ~ x), col = 'red')
legend('topleft', legend = c("Population Regression", "Least Square Regression"), lty=c(1,1),lwd=c(2.5,2.5), col=c('black','red'))
print('By changing the variability of epsilon to a smaller number, we can see that the least square regression line is more close to the ture regression line.')

#p.
x <- rnorm(100,0,1)
eps <- rnorm(100,0,sqrt(0.5))
y <- -1 + 0.5*x + eps      #beta0 = -1, beta1 = 0.5
length(y)
plot(x,y,pch=20)
#x and y has a very strong positive relationship. as x increases, y increases as well; 
# the degree of linearity is high, and variability is minor.
reg.lm_3 <- lm(y ~ x)
reg.summary <- summary(reg.lm_3)
beta0_hat <- coef(reg.summary)[1,1]
beta1_hat <- coef(reg.summary)[2,1]
print(paste('Beta0-hat for the linear regression model is: ', beta0_hat, ', and beta0 is: -1.'))
print(paste('Beta1-hat for the linear regression model is: ', beta1_hat, ', and beta1 is: 0.5.'))
abline(reg.lm_3)
y_true <- -1 + 0.5*x
abline(lm(y_true ~ x), col = 'red')
legend('topleft', legend = c("Population Regression", "Least Square Regression"), lty=c(1,1),lwd=c(2.5,2.5), col=c('black','red'))
print('With a larger variance of epsilon, the least square line stayed further from the true regression line this time.')

#q.
#The least square line is closest to the true regression line when variance of epsilon is smallest, 
#and is farest to the true regerssion line when variance of epsilon is greatest.

#r.
confint(reg.lm_1, level = 0.95)
confint(reg.lm_2, level = 0.95)
confint(reg.lm_3, level = 0.95)

#s.
#The observation to the different confidence intervals are: Smaller epsilon variance, 
#tighter confidence interval; larger epsilon variance, wider confidence interval. 
#The reason being this is because as the epsilon variance increase, the standard error of coefficients 
#will also increase, thus making a wider confidence interval.


#####################
#### QUESTION 2 ####
#####################
rm(list=ls())

#a.
set.seed (5072)
x1=runif (100)
x2 = 0.5 * x1 + rnorm (100) /10
y= 2 + 2*x1 + 0.3*x2 + rnorm (100)

#b.
#Beta0 = 2, Beta1 = 2, and Beta2 = 0.3

#c.
variable_mtx <- matrix(c(x1, x2, y), nrow = 100, ncol = 3)
colnames(variable_mtx) <- c('x1', 'x2', 'y')
cor(variable_mtx)

#d.
pairs(variable_mtx)

#e.
#x1 and y has a moderate linear relationship, x2 and y shows a weak linear relationship. 
#But x1 and x2 shows a strong linear relationship.

#f.
lm.fit <- lm(y ~ x1 + x2)

#g.
summary(lm.fit)

#h.
#Both intercept and x1_hat are statistically significant, but x2_hat is statistically insignificant
#since its p-value is greater than 0.05.

#i.
#By using alpha = 0.05, we can reject the null hypothesis that x1 = 0, but we cannot reject the 
#null hypothesis that x2 = 0 because the p-value of x2 is greater than our level of significance.

#j.
lm.fit.justx1 <- lm(y ~ x1)
summary(lm.fit.justx1)

#k.
#We can reject the null hypothesis that x1 = 0 at a level of significance of 0.01, which is 
#greater than the p-value of x1.

#l.
lm.fit.justx2 <- lm(y ~ x2)
summary(lm.fit.justx2)

#m.
#We can reject the null hypothesis that x2 = 0 at a level of significance of 0.01, which is greater
#than the p-value of x2.

#n.
cor(x1,x2)
#Yes, the results obtained in j-m contradict the results from f-i because there is a collinearity between
#x1 and x2. As we can see that the correlation coefficient of x1 and x2 is 0.844, which indicates a
#strong relationship. Therefore, when both x1 and x2 are used in a multiple regression model, the variable
#that is a strong dependent of another variable will be insignificant. On the other hand, if we run two
#different simple linear regression by using two different variables one at each time, both variables
#would be significant since the influence of collinearity between two variables are gone.

#o.
x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)

#p.
lm.fit_new <- lm(y ~ x1 + x2)
lm.fit.justx1_new <- lm(y ~ x1)
lm.fit.justx2_new <- lm(y ~ x2)

#q.
coef(summary(lm.fit))
coef(summary(lm.fit_new))
coef(summary(lm.fit.justx1))
coef(summary(lm.fit.justx1_new))
coef(summary(lm.fit.justx2))
coef(summary(lm.fit.justx2_new))
#By contrasting different model summaries for 3 different models, it is obvious that the newly 
#added point reduces the p-value for both x1 and x2.

#r
#By looking at the plots for both (x1,y) and (x2,y), we can see that (0.1, 6) is an outlier, and (0.8, 6)
#is a leverage point since the first one has unusual y value and the second one has unusual x value.
plot(x1, y)
plot(x2, y)
par(mfrow=c(2,2))
plot(lm.fit_new)
plot(lm.fit.justx1_new)
plot(lm.fit.justx2_new)
par(mfrow=c(1,1))


#####################
#### QUESTION 3 ####
#####################
rm(list=ls())

#a.
set.seed(5072)
my_data <- Boston
predictors_table <- data.frame()
for (i in 2:14) {
  
  lm.fit_ith <- lm(Boston$crim ~ Boston[, i])
  row_ith <- c(summary(lm.fit_ith)$fstatistic[1], anova(lm.fit_ith)$'Pr(>F)'[1], coef(summary(lm.fit_ith))[1,1], coef(summary(lm.fit_ith))[2,1])
  predictors_table <- rbind(predictors_table, row_ith)
  
}
colnames(predictors_table) <- c('F-Statistic', 'P-Value', 'Y-Intercept', 'Beta1-hat')
rownames(predictors_table) <- c('zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 
                                'rad', 'tax', 'ptratio', 'black', 'lstat', 'medv')
predictors_table

#b.
predictors_table[predictors_table$`P-Value` < 0.05,]

#c.
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
predictor_vec <- rownames(predictors_table[predictors_table$`P-Value` < 0.05,])
for (i in predictor_vec) {
  
  plot(Boston[i][,1], Boston$crim, main = i)
  abline(lm(Boston$crim ~ Boston[i][,1]), col='red')
  
}
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))

#d.
lm.fit.multiple <- lm(Boston$crim ~ Boston$zn + Boston$indus + Boston$chas + Boston$nox + Boston$rm + Boston$age + Boston$dis + Boston$rad + Boston$tax + Boston$ptratio + Boston$black + Boston$lstat + Boston$medv)

#e.
model_coef_info <- coef(summary(lm.fit.multiple))
model_coef_info[model_coef_info[,4]<0.05,]

#f.
plot(predictors_table$'Beta1-hat', model_coef_info[-1,1], pch = 16, xlab = 'Predictors Used in Univariate Models', ylab = 'Predictors in Multiple Regression Model')
#Which approach produces the most accurate reflection of the population parameters?
for(i in 2:14){
  
  x=anova(lm.fit.multiple, lm(Boston$crim ~ Boston[, i]))
  print(x$RSS)
  
}

#g.
poly_table <- data.frame()

for (i in 2:14) {
  
  x <- my_data[,i]
  x_sq <- x^2
  x_cub <- x^3
  lm.fit.simp_ith <- lm(my_data$crim ~ x)
  lm.fit.poly_ith <- lm(my_data$crim ~ x + x_sq + x_cub)
  row_ith <- data.frame()
  row_ith[1,1] <- names(my_data[i])
  row_ith[1,2] <- anova(lm.fit.simp_ith, lm.fit.poly_ith)$F[2]
  row_ith[1,3] <- anova(lm.fit.simp_ith, lm.fit.poly_ith)$'Pr(>F)'[2]
  poly_table <- rbind(poly_table, row_ith)
  
}
colnames(poly_table) <- c('predictor', 'fstat', 'pvalueofFstat')
poly_table <- na.omit(poly_table[order(-poly_table[,2]),])

#The following table contains the predictors whose p value indicates that there is a difference between
#the two models at the significance level of alpha=0.05, which means that each predictor has a non-linear
#relationship with the response variable except the variable 'black'.
poly_table[poly_table$pvalueofFstat<0.05,]