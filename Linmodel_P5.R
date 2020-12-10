library(readxl)
my.data <- read_excel("my.data.xlsx")
#Packages needed for the code
library(car); library(carData); library(Metrics)

#Checking for outliers
model <- lm(deaths_per ~. -Country -deaths_per -Population, data = my.data)
stud.res <- rstudent(model) 
plot(predict(model), stud.res) 
qqnorm(stud.res); qqline(stud.res, col = "red")
outlier <- which.max(stud.res); stud.res[outlier] #Outlier is no. 40 = 3.0404
par(mfrow = c(2,2))
plot(model)

#Model defined without Peru, i.e. no. 40
model1 <- lm(deaths_per ~. -Country -deaths_per -Population, data = my.data[c(-40),])
stud.res1 <- rstudent(model1)
par(mfrow = c(2,2))
plot(model1)
plot(predict(model1), stud.res1)
qqnorm(stud.res1); qqline(stud.res1, col = "red")
outlier1 <- which.max(stud.res1); stud.res1[outlier1] #Outlier no. 6 = 3.1296
hatvalues(model1)

#Model defined without Belgium, i.e. no. 6
model2 <- lm(deaths_per ~. -Country -deaths_per -Population, data = my.data[c(-40,-6),])
stud.res2 <- rstudent(model2)
plot(predict(model2), stud.res2)
qqnorm(stud.res2); qqline(stud.res2, col = "red")
outlier2 <- which.max(stud.res2); stud.res2[outlier2] #QQ-plot was better with Belgium than without
#Model1 is used.

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
vifmodel <- lm(deaths_per ~ . -Country -deaths_per -Population, data = my.data[c(-40),])
all_vifs <- vif(vifmodel); all_vifs
signifvif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signifvif_all <- signifvif_all[!(signifvif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("deaths_per ~ ", 
                             paste (signifvif_all, collapse=" + "), sep=""))  # new formula
  model3 <- lm(myForm, data=my.data[c(-40),])  # re-build model with new formula
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

yhat3 <- predict(model3); yhat3 
par(mfrow = c(1,1))
plot(yhat3, stand.res3) #Looks randomnized
#Diagnostics plots 
par(mfrow = c(2,2))
plot(model3)
#Fitted values 
summary(model3)

#Plotting all explanatory variables against residuals, to see if some needs to be transformed
par(mfrow = c(2,2))
plot(my.data[c(-40),]$Pop.density, stand.res3)
plot(my.data[c(-40),]$GDP, stand.res3)
plot(my.data[c(-40),]$cardiovasc_per, stand.res3)
plot(my.data[c(-40),]$diabetes_per, stand.res3)
plot(my.data[c(-40),]$Smokers, stand.res3)
plot(my.data[c(-40),]$COPD_per, stand.res3)
plot(my.data[c(-40),]$CKD_per, stand.res3)
plot(my.data[c(-40),]$health_exp, stand.res3)
plot(my.data[c(-40),]$Obesity, stand.res3)
plot(my.data[c(-40),]$organ_per, stand.res3)
plot(my.data[c(-40),]$asthma_per, stand.res3)
plot(my.data[c(-40),]$Gini, stand.res3)

#Histograms with normal curve is plotted for deaths
par(mfrow = c(1,1))
hist(my.data[c(-40),]$deaths_per, prob = TRUE, xlim = c(-0.15,0.15)); 
curve(dnorm(x, mean(my.data[c(-40),]$deaths_per), sd(my.data[c(-40),]$deaths_per)), 
      add = TRUE, col = "green") #The distribution of deaths is right-skewed

#The Q-Q plot of the standard residuals is made, to check if the residuals are approx. normally dist.
par(mfrow = c(1,1))
qqnorm(stand.res3); qqline(stand.res3, col = "red") #Looks fine 
hist(stand.res3, prob = TRUE); curve(dnorm(x, mean(stand.res3), sd(stand.res3)), 
                                     add = TRUE, col = "green") #Shows some heavy tails 

#Next, it's checked if the model can be reduced by removing insignificant p-values one by one
first.p <- which.max(summary(model3)$coefficients[,4]); 
summary(model3)$coefficients[,4][first.p] #Obesity biggest, removed, = 0.6460

model4 <- lm(deaths_per ~ Pop.density + GDP + cardiovasc_per + diabetes_per
             + Smokers + COPD_per + CKD_per + health_exp + organ_per
             + asthma_per + Gini, data = my.data[c(-40),])
sec.p <- which.max(summary(model4)$coefficients[,4]); 
summary(model4)$coefficients[,4][sec.p] #Pop.density biggest, removed, = 0.5822

model5 <- lm(deaths_per ~ GDP + cardiovasc_per + diabetes_per + Smokers + COPD_per + CKD_per 
             + health_exp + organ_per + asthma_per + Gini, data = my.data[c(-40),])
third.p <- which.max(summary(model5)$coefficients[,4]); 
summary(model5)$coefficients[,4][third.p] #asthma_per, p-val = 0.5812

model6 <- lm(deaths_per ~ GDP + cardiovasc_per + diabetes_per + Smokers + COPD_per + CKD_per 
             + health_exp + organ_per + Gini, data = my.data[c(-40),])
fourth.p <- which.max(summary(model6)$coefficients[,4]); 
summary(model6)$coefficients[,4][fourth.p] #health_exp, p-val = 0.5791

model7 <- lm(deaths_per ~ GDP + cardiovasc_per + diabetes_per + Smokers + COPD_per + CKD_per 
             + organ_per + Gini, data = my.data[c(-40),])
fifth.p <- which.max(summary(model7)$coefficients[,4]); 
summary(model7)$coefficients[,4][fifth.p] #Smokers, p-val = 0.5627

model8 <- lm(deaths_per ~ GDP + cardiovasc_per + diabetes_per + COPD_per + CKD_per 
             + Gini, data = my.data[c(-40),])
sixth.p <- which.max(summary(model8)$coefficients[,4]); 
summary(model8)$coefficients[,4][sixth.p] #cardiovsc_per, p-val = 0.8815

model9 <- lm(deaths_per ~ GDP + diabetes_per + COPD_per + CKD_per + Gini, 
               data = my.data[c(-40),])
sev.p <- which.max(summary(model9)$coefficients[,4]); 
summary(model9)$coefficients[,4][sev.p] #GDP, p-val = 0.4213

mod <- lm(deaths_per ~ diabetes_per + COPD_per + CKD_per + Gini,
               data = my.data[c(-40),])
eigth.p <- which.max(summary(mod)$coefficients[,4]); 
summary(mod)$coefficients[,4][eigth.p] #diabetes_per, p-val = 0.2973

mod1 <- lm(deaths_per ~ COPD_per + CKD_per + Gini, data = my.data[c(-40),])
ninth.p <- which.max(summary(mod1)$coefficients[,4]); 
summary(mod1)$coefficients[,4][ninth.p] #CKD_per, p-val = 0.1676

mod2 <- lm(deaths_per ~ COPD_per + Gini, data = my.data[c(-40),])
ten.p <- which.max(summary(mod2)$coefficients[,4]); 
summary(mod2)$coefficients[,4][ten.p] #All has p-val < 0.05, i.e. this is the final reduced model
summary(mod2)

predict(mod2) #Fitted values
par(mfrow = c(2,2))
plot(mod2)
anova(model3, model4, model5, model6, model7, model8, model9, mod1, mod2) #H_0 hypothesis is accepted
anova(model3, mod2)
