library(readxl)
my.data <- read_excel("my.data.xlsx")

#Packages needed for the code
library(car); library(carData)

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
vifmodel <- lm(Deaths ~ . -Country -Deaths, data = my.data)
all_vifs <- vif(vifmodel); all_vifs
signifvif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signifvif_all <- signifvif_all[!(signifvif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("Deaths ~ ", paste (signifvif_all, collapse=" + "), sep=""))  # new formula
  model3 <- lm(myForm, data=my.data)  # re-build model with new formula
  all_vifs <- vif(model3)
}
summary(model3)
vif(model3)

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
par(mfrow = c(1,1))
plot(yhat3, stand.res3) #Trumpet shape, i.e. log-transform y

#model where deaths is log-transformed
log.model <- lm(log(Deaths) ~ . -Country -Deaths -CKD -`65_older` -`70_older` 
                -Median_age -Organ_transplant -HDI, data = my.data)
summary(log.model)
log.res <- residuals(log.model)
stand.logres <- rstandard(log.model)
stud.logres <- rstudent(log.model)

#Checking residual plot, and Q-Q plot
qqnorm(stand.logres); qqline(stand.logres, col = "red")
plot(predict(log.model), stand.logres)

#Plotting all explanatory variables against residuals, to see if some needs to be transformed
par(mfrow = c(3,4))
plot(my.data$Population, stand.logres)
plot(my.data$Pop.density, stand.logres)
plot(my.data$GDP, stand.logres)
plot(my.data$Cardiovasc, stand.logres)
plot(my.data$Diabetes, stand.logres)
plot(my.data$Smokers, stand.logres)
plot(my.data$COPD, stand.logres)
plot(my.data$Health_exp, stand.logres)
plot(my.data$Obesity, stand.logres)
plot(my.data$Asthma, stand.logres)
plot(my.data$Cancer, stand.logres)
plot(my.data$Gini, stand.logres)

#Population, pop.density, and cancer changes depends on the residuals, log-transform of these 
log.model1 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Diabetes + Smokers + COPD + Health_exp + Obesity + Asthma + Gini,
                 data = my.data)
summary(log.model1)
log.res1 <- residuals(log.model1)
stand.logres1 <- rstandard(log.model1)
stud.logres1 <- rstudent(log.model1)

par(mfrow = c(1,1))
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
plot(my.data$COPD, stand.logres1)
plot(my.data$Health_exp, stand.logres1)
plot(my.data$Obesity, stand.logres1)
plot(my.data$Asthma, stand.logres1)
plot(log(my.data$Cancer), stand.logres1)
plot(my.data$Gini, stand.logres1) #The dependence is gone 

#Histograms with normal curve is plotted for deaths and log(deaths)
par(mfrow = c(1,2))
hist(log(my.data$Deaths), prob = TRUE); 
curve(dnorm(x, mean(log(my.data$Deaths)), sd(log(my.data$Deaths))), add = TRUE, col = "green")
hist(my.data$Deaths, prob = TRUE); 
curve(dnorm(x, mean(my.data$Deaths), sd(my.data$Deaths)), add = TRUE, col = "green")
#log(deaths) is approximately more normal distributed than deaths 

#The Q-Q plot is made, to see if the above statement is correct
par(mfrow = c(2,1))
qqnorm(log(my.data$Deaths)); qqline(log(my.data$Deaths), col = "red")
qqnorm(my.data$Deaths); qqline(my.data$Deaths, col = "red") #It is

#The Q-Q plot of the standard residuals is made, to check if the above also holds for the residuals
par(mfrow = c(2,1))
qqnorm(stand.res3); qqline(stand.res3, col = "red")
qqnorm(stand.logres1); qqline(stand.logres1, col = "red")

#Diagnostics plots for the log-model
par(mfrow = c(2,2))
plot(log.model1)
par(mfrow = c(1,1))
plot(stud.logres1)

#Next, it's checked if the model can be reduced by removing insignificant p-values one by one
first.p <- which.max(summary(log.model1)$coefficients[,4]); first.p #Diabetes biggest, removed

log.mod <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Obesity + Smokers + COPD + Health_exp + Asthma + Gini,
                 data = my.data)
sec.p <- which.max(summary(log.mod)$coefficients[,4]); sec.p #Health_exp biggest, removed

log.mod1 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
              + Obesity + Smokers + COPD + Asthma + Gini, data = my.data)
third.p <- which.max(summary(log.mod1)$coefficients[,4]); third.p #Smokers, p-val = 0.9055
summary(log.mod1)

log.mod2 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
               + Obesity + COPD + Asthma + Gini, data = my.data)
fourth.p <- which.max(summary(log.mod2)$coefficients[,4]); fourth.p #GDP
summary(log.mod2)

log.mod3 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + Cardiovasc
               + Obesity + COPD + Asthma + Gini, data = my.data)
fifth.p <- which.max(summary(log.mod3)$coefficients[,4]); fifth.p #Asthma
summary(log.mod3)

log.mod4 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + Cardiovasc
               + Obesity + COPD + Gini, data = my.data)
sixth.p <- which.max(summary(log.mod4)$coefficients[,4]); sixth.p #Cardiovasc, p-val = 0.6674
summary(log.mod4)

log.mod5 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer)
               + Obesity + COPD + Gini, data = my.data)
sev.p <- which.max(summary(log.mod5)$coefficients[,4]); sev.p #log(Cancer), p-val = 0.3713
summary(log.mod5)

log.mod6 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + Obesity + COPD + Gini,
               data = my.data)
eigth.p <- which.max(summary(log.mod6)$coefficients[,4]); eigth.p #COPD, p-val = 0.1508
summary(log.mod6)

log.mod7 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + Obesity + Gini,
               data = my.data)
ninth.p <- which.max(summary(log.mod7)$coefficients[,4]); ninth.p #Gini, p-val = 0.0847
summary(log.mod7)

log.mod8 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + Obesity, data = my.data)
ten.p <- which.max(summary(log.mod8)$coefficients[,4]); ten.p #log(Pop.density), p-val = 0.0598
summary(log.mod8)

log.mod9 <- lm(log(Deaths) ~ log(Population) + Obesity, data = my.data)
elev.p <- which.max(summary(log.mod9)$coefficients[,4]); elev.p 
summary(log.mod9) #All p-val < 0.05

predict(log.mod9)
par(mfrow = c(2,2))
plot(log.mod9)
anova(log.model1, log.mod, log.mod1, log.mod2, log.mod3, log.mod4, log.mod5, 
      log.mod6, log.mod7, log.mod8, log.mod9)
