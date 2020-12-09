library(readxl)
my.data <- read_excel("my.data.xlsx")
install.packages("Metrics")
#Packages needed for the code
library(car); library(carData); library(Metrics)

#Checking for outliers
model <- lm(deaths_per ~. -Country -deaths_per, data = my.data)
stud.res <- rstudent(model) 
plot(predict(model), stud.res) 
qqnorm(stud.res); qqline(stud.res, col = "red")
outlier <- which.max(stud.res); stud.res[outlier] #Outlier is no. 6 = 3.0053
deaths <- as.vector(my.data$deaths_per)
cov1 <- cov(coef(model))
mse <- rmse(deaths, predict(model))

matrix <- as.matrix(my.data[,-1])
hat <- matrix%*%inv((t(matrix)%*%matrix))%*%t(matrix)

#Model defined without Belgium, i.e. no. 6
model1 <- lm(deaths_per ~. -Country -deaths_per, data = my.data[c(-6),])
stud.res1 <- rstudent(model1)
plot(predict(model1), stud.res1)
qqnorm(stud.res1); qqline(stud.res1, col = "red")
outlier1 <- which.max(stud.res1); stud.res1[outlier1] #Outlier no. 40 = 3.0566

#Model defined without Pery, i.e. no. 40
model2 <- lm(deaths_per ~. -Country -deaths_per, data = my.data[c(-6,-40),])
stud.res2 <- rstudent(model2)
plot(predict(model2), stud.res2)
qqnorm(stud.res2); qqline(stud.res2, col = "red")
outlier2 <- which.max(stud.res2); stud.res2[outlier2] #QQ-plot was better with Norway that without
#Model1 is used.

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
vifmodel <- lm(deaths_per ~ . -Country -deaths_per, data = my.data[c(-6),])
all_vifs <- vif(vifmodel); all_vifs
signifvif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signifvif_all <- signifvif_all[!(signifvif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("deaths_per ~ ", 
                             paste (signifvif_all, collapse=" + "), sep=""))  # new formula
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

yhat3 <- predict(model3); yhat3 
par(mfrow = c(1,1))
plot(yhat3, stand.res3) #Looks randomnized

#Plotting all explanatory variables against residuals, to see if some needs to be transformed
par(mfrow = c(3,4))
plot(my.data$Population, stand.res3)
plot(my.data$Pop.density, stand.res3)
plot(my.data$GDP, stand.res3)
plot(my.data$cardiovasc_per, stand.res3)
plot(my.data$diabetes_per, stand.res3)
plot(my.data$Smokers, stand.res3)
plot(my.data$COPD_per, stand.res3)
plot(my.data$CKD_per, stand.res3)
plot(my.data$health_exp, stand.res3)
plot(my.data$Obesity, stand.res3)
plot(my.data$organ_per, stand.res3)
plot(my.data$asthma_per, stand.res3)
plot(my.data$Gini, stand.res3)

#Population and pop.density depends on the residuals, i.e. log-transform these 
log.model <- lm(deaths_per ~ log(Population) + log(Pop.density) + GDP + cardiovasc_per
                 + diabetes_per + Smokers + COPD_per + CKD_per + health_exp + Obesity + asthma_per + Gini,
                 data = my.data[c(-6),])
summary(log.model)
log.res <- residuals(log.model)
stand.logres <- rstandard(log.model)
stud.logres <- rstudent(log.model)

par(mfrow = c(1,1))
qqnorm(stand.logres); qqline(stand.logres, col = "red")
plot(predict(log.model), stand.logres)

#Plots of residuals and explanatory variables 
par(mfrow = c(3,4))
plot(log(my.data[c(-6),]$Population), stand.logres)
plot(log(my.data[c(-6),]$Pop.density), stand.logres)
plot(my.data[c(-6),]$GDP, stand.logres)
plot(my.data[c(-6),]$cardiovasc_per, stand.logres)
plot(my.data[c(-6),]$diabetes_per, stand.logres)
plot(my.data[c(-6),]$Smokers, stand.logres)
plot(my.data[c(-6),]$COPD_per, stand.logres)
plot(my.data[c(-6),]$CKD_per, stand.logres)
plot(my.data[c(-6),]$health_exp, stand.logres)
plot(my.data[c(-6),]$Obesity, stand.logres)
plot(my.data[c(-6),]$organ_per, stand.logres)
plot(my.data[c(-6),]$asthma_per, stand.logres)
plot(my.data[c(-6),]$Gini, stand.logres) #The dependence is gone 

#Histograms with normal curve is plotted for deaths and log(deaths)
par(mfrow = c(1,2))
hist(my.data[c(-6),]$deaths_per, prob = TRUE, xlim = c(-0.15,0.15)); 
curve(dnorm(x, mean(my.data[c(-6),]$deaths_per), sd(my.data[c(-6),]$deaths_per)), 
      add = TRUE, col = "green") #The distribution of deaths is right-skewed

#The Q-Q plot of the standard residuals is made, to check if the residuals are approx. normally dist.
par(mfrow = c(2,1))
qqnorm(stand.res3); qqline(stand.res3, col = "red")
qqnorm(stand.logres); qqline(stand.logres, col = "red")

#Diagnostics plots for the log-model
par(mfrow = c(2,2))
plot(log.model)
par(mfrow = c(1,1))
plot(stud.logres)

#Next, it's checked if the model can be reduced by removing insignificant p-values one by one
first.p <- which.max(summary(log.model)$coefficients[,4]); 
summary(log.model)$coefficients[,4][first.p] #Obesity biggest, removed

log.mod <- lm(deaths_per ~ log(Population) + log(Pop.density) + GDP + cardiovasc_per
                 + diabetes_per + Smokers + COPD_per + CKD_per + health_exp + asthma_per + Gini,
                 data = my.data[c(-6),])
sec.p <- which.max(summary(log.mod)$coefficients[,4]); 
summary(log.mod)$coefficients[,4][sec.p] #COPD_per biggest, removed

log.mod1 <- lm(deaths_per ~ log(Population) + log(Pop.density) + GDP + cardiovasc_per
              + diabetes_per + Smokers + CKD_per + health_exp + asthma_per + Gini, data = my.data[c(-6),])
third.p <- which.max(summary(log.mod1)$coefficients[,4]); 
summary(log.mod1)$coefficients[,4][third.p] #Smokers, p-val = 0.7129

log.mod2 <- lm(deaths_per ~ log(Population) + log(Pop.density) + GDP + cardiovasc_per
               + diabetes_per + CKD_per + health_exp + asthma_per + Gini, data = my.data[c(-6),])
fourth.p <- which.max(summary(log.mod2)$coefficients[,4]); 
summary(log.mod2)$coefficients[,4][fourth.p] #cardiovasc, p-val = 0.7369

log.mod3 <- lm(deaths_per ~ log(Population) + log(Pop.density) + GDP + diabetes_per
               + CKD_per + health_exp + asthma_per + Gini, data = my.data[c(-6),])
fifth.p <- which.max(summary(log.mod3)$coefficients[,4]); 
summary(log.mod3)$coefficients[,4][fifth.p] #log(pop.density), p-val = 0.7711

log.mod4 <- lm(deaths_per ~ log(Population) + GDP + diabetes_per
               + CKD_per + health_exp + asthma_per + Gini, data = my.data[c(-6),])
sixth.p <- which.max(summary(log.mod4)$coefficients[,4]); 
summary(log.mod4)$coefficients[,4][sixth.p] #log(population), p-val = 0.4178

log.mod5 <- lm(deaths_per ~ GDP + diabetes_per + CKD_per + health_exp + asthma_per + Gini, 
               data = my.data[c(-6),])
sev.p <- which.max(summary(log.mod5)$coefficients[,4]); 
summary(log.mod5)$coefficients[,4][sev.p] #asthma_per, p-val = 0.3262

log.mod6 <- lm(deaths_per ~ GDP + diabetes_per + CKD_per + health_exp + Gini,
               data = my.data[c(-6),])
eigth.p <- which.max(summary(log.mod6)$coefficients[,4]); 
summary(log.mod6)$coefficients[,4][eigth.p] #GDP, p-val = 0.4460

log.mod7 <- lm(deaths_per ~ diabetes_per + CKD_per + health_exp + Gini, data = my.data[c(-6),])
ninth.p <- which.max(summary(log.mod7)$coefficients[,4]); 
summary(log.mod7)$coefficients[,4][ninth.p] #diabetes_per, p-val = 0.18

log.mod8 <- lm(deaths_per ~ CKD_per + health_exp + Gini, data = my.data[c(-6),])
ten.p <- which.max(summary(log.mod8)$coefficients[,4]); 
summary(log.mod8)$coefficients[,4][ten.p] #intercept, this isn't removed, hence CKD removed instead
summary(log.mod8)

log.mod9 <- lm(deaths_per ~ health_exp + Gini, data = my.data[c(-6),])
elev.p <- which.max(summary(log.mod9)$coefficients[,4]); elev.p 
summary(log.mod9) #All p-val < 0.08

predict(log.mod9)
par(mfrow = c(2,2))
plot(log.mod9)
anova(log.model, log.mod, log.mod1, log.mod2, log.mod3, log.mod4, log.mod5, 
      log.mod6, log.mod7, log.mod8, log.mod9) #H_0 hypothesis is accepted
