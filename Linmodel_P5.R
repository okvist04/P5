library(readxl)
my.data <- read_excel("my.data.xlsx")

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
vifmodel <- lm(Deaths ~ . -Country -Deaths, data = my.data)
all_vifs <- car::vif(vifmodel); all_vifs
signif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("Deaths ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  model3 <- lm(myForm, data=my.data)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
summary(model3)
car::vif(model3)
res3 <- residuals(model3)
stand.res3 <- rstandard(model3)
stud.res3 <- rstudent(model3)

par(mfrow = c(2,2))
qqnorm(res3); qqline(res3, col = "red")
hist(res3, prob = TRUE, xlab = "Residuals"); 
curve(dnorm(x, mean(res3), sd(res3)), add = TRUE, col = "green")
boxplot(predict(model3), main = "Boxplot of Fitted Values")
plot(stand.res3, main = "Residuals")

yhat3 <- predict(model3) #Negative values, tranform y
plot(yhat3, stand.res3) #Trumpet shape, i.e. log-transform y

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





