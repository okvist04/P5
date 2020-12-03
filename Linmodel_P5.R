library(readxl)
my.data <- read_excel("my.data.xlsx")

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
model <- lm(Deaths ~ . -Country -Deaths, data = my.data)
multi.col <- vif(model); multi.col
max <- max(multi.col); max

model1 <- lm(Deaths ~ . -Country -Deaths -CKD, data = my.data)
multi.col1 <- vif(model1); multi.col1
max1 <- max(multi.col1); max1

model2 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older`, data = my.data)
multi.col2 <- vif(model2); multi.col2
max2 <- max(multi.col2); max2

model3 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -`70_older`, data = my.data)
multi.col3 <- vif(model3); multi.col3
max3 <- max(multi.col3); max3

model4 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -`70_older`-Median_age, data = my.data)
multi.col4 <- vif(model4); multi.col4
max4 <- max(multi.col4); max4

model5 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -Organ_transplant 
             -Median_age -`70_older`, data = my.data)
multi.col5 <- vif(model5); multi.col5
max5 <- max(multi.col5); max5

model6 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -Organ_transplant 
             -Median_age -`70_older` -HDI, data = my.data) 
multi.col6 <- vif(model6); multi.col6
max6 <- max(multi.col6); max6
summary(model6)
res6 <- residuals(model6)
stand.res6 <- rstandard(model6)
stud.res6 <- rstudent(model6)

par(mfrow = c(2,2))
qqnorm(res6); qqline(res6, col = "red")
hist(res6, prob = TRUE, xlab = "Residuals"); 
curve(dnorm(x, mean(res6), sd(res6)), add = TRUE, col = "green")
boxplot(predict(model6), main = "Boxplot of Fitted Values")
plot(stand.res6, main = "Residuals")

yhat6 <- predict(model6) #Negative values, tranform y
plot(yhat6, stand.res6) #Trumpet shape, i.e. log-transform y

#model where deaths is log-transformed
log.model <- lm(log(Deaths) ~ . -Country -Deaths -CKD -`65_older` -`70_older` 
                -Median_age -Organ_transplant -HDI, data = my.data)
summary(log.model)
log.res <- residuals(log.model)
stand.logres <- rstandard(log.model)
stud.logres <- rstudent(log.model)

#Checking residual plot, and Q-Q plot
par(mfrow = c(2,1))
qqnorm(stand.logres); qqline(stand.logres, col = "red")
plot(predict(log.model), stand.logres)

#Plotting all explanatory variables against residuals, to see if some needs to be transformed
par(mfrow = c(3,3))
plot(my.data$Population, stand.logres)
plot(my.data$Pop.density, stand.logres)
plot(my.data$GDP, stand.logres)
plot(my.data$Cardiovasc, stand.logres)
plot(my.data$Diabetes, stand.logres)
plot(my.data$Smokers, stand.logres)
plot(my.data$HDI, stand.logres)
plot(my.data$COPD, stand.logres)
plot(my.data$Health_exp, stand.logres)
plot(my.data$Obesity, stand.logres)
plot(my.data$Asthma, stand.logres)
plot(my.data$Cancer, stand.logres)
plot(my.data$Gini, stand.logres)

#Population, pop.density, and cancer changes depends on the residuals, log-transform of these 
log.model1 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Diabetes + Smokers + COPD + Health_exp + Obesity + Asthma + Cancer + Gini,
                 data = my.data)
summary(log.model1)
log.res1 <- residuals(log.model1)
stand.logres1 <- rstandard(log.model1)
stud.logres1 <- rstudent(log.model1)

par(mfrow = c(2,1))
qqnorm(stand.logres1); qqline(stand.logres1, col = "red")
plot(predict(log.model1), stand.logres1)

#Plots of residuals and explanatory variables 
par(mfrow = c(3,4))
plot(log(my.data$Population), stand.logres1)
plot(log(my.data$Pop.density), stand.logres1)
plot(my.data$GDP, stand.logres1)
plot(my.data$Cardiovasc, stand.logres1)
plot(my.data$Diabetes, stand.logres1)
plot(my.data$Smokers, stand.logres1)
plot(my.data$HDI, stand.logres1)
plot(my.data$COPD, stand.logres1)
plot(my.data$Health_exp, stand.logres1)
plot(my.data$Obesity, stand.logres1)
plot(my.data$Asthma, stand.logres1)
plot(log(my.data$Cancer), stand.logres1)
plot(my.data$Gini, stand.logres1) #The dependence is gone 

#Histograms with normal curve is plotted for deaths and log(deaths)
hist(log(my.data$Deaths), prob = TRUE); 
curve(dnorm(x, mean(log(my.data$Deaths)), sd(log(my.data$Deaths))), add = TRUE, col = "green")

hist(my.data$Deaths, prob = TRUE); 
curve(dnorm(x, mean(my.data$Deaths), sd(my.data$Deaths)), add = TRUE, col = "green")
#log(deaths) is approximately more normal distributed than deaths 

#The Q-Q plot is made, to see if the above statement is correct
qqnorm(log(my.data$Deaths)); qqline(log(my.data$Deaths), col = "red")
qqnorm(my.data$Deaths); qqline(my.data$Deaths, col = "red") #It is

#The Q-Q plot of the standard residuals is made, to check if the above also holds for the residuals
qqnorm(stand.res6); qqline(stand.res6, col = "red")
qqnorm(stand.logres1); qqline(stand.logres1, col = "red")

#Diagnostics plots for the log-model
plot(log.model1)
plot(rstudent(log.model1))
options(max.print = 1500)
influence.measures(log.model1)
cooks.distance(log.model1)
plot(cooks.distance(log.model1))
influence(log.model1)
leveragePlots(log.model1) #No. 53 is an outlier with much leverage, a new model with this country
#removed is made

#Log-model without US, i.e. country no. 53
log.model2 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Diabetes + Smokers + COPD + Health_exp + Obesity + Asthma + Cancer + Gini,
                 data = my.data[c(-53),])
summary(log.model2)

hist(residuals(log.model2), prob = TRUE); 
curve(dnorm(x, mean(residuals(log.model2)),sd(residuals(log.model2))), add = TRUE, col = "green")





