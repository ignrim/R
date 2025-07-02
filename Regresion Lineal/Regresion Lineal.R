# Load required packages
#library(ggplot2)
library(car)
library(caret)
library(corrplot)


#Loading data
data(mtcars)  

# Looking at variables
str(mtcars)


head(mtcars)

# To see the distribution of the variables, submit summary() function.
summary(mtcars)



mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)


#Dropping dependent variable for calculating Multicollinearity
mtcars_a = subset(mtcars, select = -c(mpg))

#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)



# Visualize Correlation Matrix
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))


# Checking Variables that are highly correlated
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorCol


#Remove highly correlated variables and create a new dataset
dat3 = mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)


# At this step, we are building multiple linear regression model.

#Build Linear Regression Model
fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff
anova(fit)

par(mfrow=c(2,2))
plot(fit)


summary(fit)




#Extracting R-squared value
summary(fit)$r.squared


#Extracting Adjusted R-squared value
summary(fit)$adj.r.squared


AIC(fit)


BIC(fit)


#Stepwise Selection based on AIC
library(MASS)
step <- stepAIC(fit, direction="both")
summary(step)


#Backward Selection based on AIC
step <- stepAIC(fit, direction="backward")
summary(step)


#Forward Selection based on AIC
step <- stepAIC(fit, direction="forward")
summary(step)


#Stepwise Selection with BIC
n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)


#Standardised coefficients
library(QuantPsyc)
lm.beta(stepBIC)


#R Function : Manual Calculation of Standardised coefficients
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
sy <- sapply(regmodel$model[1], sd)
beta <- b * sx / sy
return(beta)
}

std.Coeff = data.frame(Standardized.Coeff = stdz.coff(stepBIC))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL



#Autocorrelation Test
durbinWatsonTest(stepBIC)

#Normality Of Residuals (Should be > 0.05)
res=residuals(stepBIC,type="pearson")
shapiro.test(res)

#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC)

#Outliers – Bonferonni test
outlierTest(stepBIC)

#See Residuals
resid = residuals(stepBIC)

#Relative Importance
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC)


#See Predicted Value
pred = predict(stepBIC,dat3)
#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))


#Calculating RMSE
rmse = sqrt(mean((dat3$mpg - pred)^2))
print(rmse)

#Calculating Rsquared manually
y = dat3[,c("mpg")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#Calculating Adj. Rsquared manually
n = dim(dat3)[1]
p = dim(summary(stepBIC)$coeff)[1] - 1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)

#Box Cox Transformation
library(lmSupport)
modelBoxCox(stepBIC)


library(DAAG)
kfold = cv.lm(data=dat3, stepBIC, m=5)



linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)


summary(linearMod)  # model summary


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)


# print(modelSummary)
# print(modelCoeffs)
print(beta.estimate)


# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
print(lmMod)
distPred <- predict(lmMod, testData)  # predict distance
print(distPred)


summary (lmMod)  # model summary


actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
print(correlation_accuracy)
head(actuals_preds)


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(min_max_accuracy)
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
print(mape)
# => 48.38%, mean absolute percentage deviation


