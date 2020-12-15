library(readxl)
my.data <- read_excel("my.data.xlsx")
#Packages needed for the code
library(car); library(carData); library(Metrics); library(plm)

#Checking for outliers
model00 <- lm(deaths_per ~. -Country -deaths_per, data = my.data)
stud.res00 <- rstudent(model00) 
plot(predict(model00), stud.res00) 
qqnorm(stud.res00); qqline(stud.res00, col = "red")
outlier <- which.max(stud.res00); stud.res00[outlier] #Outlier is no. 40 = 3.0404 (Peru)
par(mfrow = c(2,2))
plot(model00)
hatvalues(model00)

#Model defined without Peru, i.e. no. 40
model01 <- lm(deaths_per ~. -Country -deaths_per, data = my.data[c(-40),])
stud.res01 <- rstudent(model01)
par(mfrow = c(2,2))
plot(model01)
par(mfrow = c(1,1))
plot(predict(model01), stud.res01)
qqnorm(stud.res01); qqline(stud.res01, col = "red")
outlier01 <- which.max(stud.res01); stud.res01[outlier01] #Outlier no. 6 = 3.1296 (Belgium)
hatvalues(model01)

#Model defined without Belgium, i.e. no. 6
model02 <- lm(deaths_per ~. -Country -deaths_per, 
              data = my.data[c(-40,-6),])
stud.res02 <- rstudent(model02)
par(mfrow = c(1,1))
plot(predict(model02), stud.res02)
qqnorm(stud.res02); qqline(stud.res02, col = "red")
outlier2 <- which.max(stud.res02); stud.res02[outlier2] #No more outliers 
#Residual plot was better without no. 6 and 40

#Checking for VIF > 5, and removing one by one until all has VIF <= 5
vifmodel <- lm(deaths_per ~ . -Country -deaths_per, data = my.data[c(-40, -6),])
all_vifs <- vif(vifmodel); all_vifs
signifvif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signifvif_all <- signifvif_all[!(signifvif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("deaths_per ~ ", 
                             paste (signifvif_all, collapse=" + "), sep=""))  # new formula
  model1 <- lm(myForm, data=my.data[c(-40, -6),])  # re-build model with new formula
  all_vifs <- vif(model1)
}
summary(model1)
vif(model1)

res1 <- residuals(model1)
stand.res1 <- rstandard(model1)
stud.res1 <- rstudent(model1)

par(mfrow = c(2,2))
qqnorm(res1); qqline(res1, col = "red")
hist(res1, prob = TRUE, xlab = "Residuals"); 
curve(dnorm(x, mean(res1), sd(res1)), add = TRUE, col = "green")
boxplot(predict(model1), main = "Boxplot of Fitted Values")
plot(stand.res1, main = "Residuals")

yhat1 <- predict(model1); yhat1
par(mfrow = c(1,1))
plot(yhat1, stand.res1) #Looks randomnized
#Diagnostics plots 
par(mfrow = c(2,2))
plot(model1)
#Fitted values 
summary(model1)

#Plotting all explanatory variables against residuals, to see if some needs to be transformed
par(mfrow = c(2,2))
plot(my.data[c(-40, -6),]$Pop.density, stand.res1, xlab = "Pop.density")
plot(my.data[c(-40, -6),]$`70_older`, stand.res1, xlab = "70_older")
plot(my.data[c(-40, -6),]$GDP, stand.res1, xlab = "GDP")
plot(my.data[c(-40, -6),]$diabetes_per, stand.res1, xlab = "Diabetes")
plot(my.data[c(-40, -6),]$Smokers, stand.res1, xlab = "Smokers")
plot(my.data[c(-40, -6),]$COPD_per, stand.res1, xlab = "COPD")
plot(my.data[c(-40, -6),]$CKD_per, stand.res1, xlab = "CKD")
plot(my.data[c(-40, -6),]$health_exp, stand.res1, xlab = "Health_exp")
plot(my.data[c(-40, -6),]$Obesity, stand.res1, xlab = "Obesity")
plot(my.data[c(-40, -6),]$organ_per, stand.res1, xlab = "Organ_transp")
plot(my.data[c(-40, -6),]$asthma_per, stand.res1, xlab = "Asthma")
plot(my.data[c(-40, -6),]$Gini, stand.res1, xlab = "Gini")

#Histograms with normal curve is plotted for deaths
par(mfrow = c(1,1))
hist(my.data[c(-40, -6),]$deaths_per, prob = TRUE, xlim = c(-0.15,0.15));
curve(dnorm(x, mean(my.data[c(-40, -6),]$deaths_per), sd(my.data[c(-40, -6),]$deaths_per)), 
      add = TRUE, col = "green") #The distribution of deaths is right-skewed

#The Q-Q plot of the standard residuals is made, to check if the residuals are approx. normally dist.
par(mfrow = c(1,1))
qqnorm(stand.res1); qqline(stand.res1, col = "red") #Looks fine 
hist(stand.res1, prob = TRUE); curve(dnorm(x, mean(stand.res1), sd(stand.res1)), 
                                     add = TRUE, col = "green") #Shows some heavy tails 

#Next, it's checked if the model can be reduced by removing insignificant p-values one by one
first.p <- which.max(summary(model1)$coefficients[,4]); 
summary(model1)$coefficients[,4][first.p] #Pop.density biggest, removed, = 0.9794

model2 <- lm(deaths_per ~ `70_older` + GDP + diabetes_per + Smokers 
             + COPD_per + CKD_per + health_exp + Obesity + organ_per
             + asthma_per + Gini, data = my.data[c(-40, -6),])
sec.p <- which.max(summary(model2)$coefficients[,4]); 
summary(model2)$coefficients[,4][sec.p] #health_exp biggest, removed, = 0.8774

model3 <- lm(deaths_per ~ `70_older` + GDP + diabetes_per + Smokers 
             + COPD_per + CKD_per + Obesity + organ_per + asthma_per + Gini, 
             data = my.data[c(-40, -6),])
third.p <- which.max(summary(model3)$coefficients[,4]); 
summary(model3)$coefficients[,4][third.p] #COPD_per, p-val = 0.8358

model4 <- lm(deaths_per ~ `70_older` + GDP + diabetes_per + Smokers + CKD_per 
             + Obesity + organ_per + asthma_per + Gini, data = my.data[c(-40, -6),])
fourth.p <- which.max(summary(model4)$coefficients[,4]); 
summary(model4)$coefficients[,4][fourth.p] #`70_older`, p-val = 0.6918

model5 <- lm(deaths_per ~ GDP + diabetes_per + Smokers + CKD_per + Obesity
             + organ_per + asthma_per + Gini, data = my.data[c(-40, -6),])
fifth.p <- which.max(summary(model5)$coefficients[,4]); 
summary(model5)$coefficients[,4][fifth.p] #Smokers, p-val = 0.5578

model6 <- lm(deaths_per ~ GDP + diabetes_per + CKD_per + Obesity 
             + organ_per + asthma_per + Gini, data = my.data[c(-40, -6),])
sixth.p <- which.max(summary(model6)$coefficients[,4]); 
summary(model6)$coefficients[,4][sixth.p] #asthma_per, p-val = 0.4858

model7 <- lm(deaths_per ~ GDP + diabetes_per + CKD_per + Obesity + organ_per + Gini, 
               data = my.data[c(-40, -6),])
sev.p <- which.max(summary(model7)$coefficients[,4]); 
summary(model7)$coefficients[,4][sev.p] #GDP, p-val = 0.5876

model8 <- lm(deaths_per ~ diabetes_per + CKD_per + Obesity + organ_per + Gini,
               data = my.data[c(-40, -6),])
eigth.p <- which.max(summary(model8)$coefficients[,4]); 
summary(model8)$coefficients[,4][eigth.p] #Obesity, p-val = 0.3581

model9 <- lm(deaths_per ~ diabetes_per + CKD_per + organ_per + Gini, data = my.data[c(-40, -6),])
ninth.p <- which.max(summary(model9)$coefficients[,4]); 
summary(model9)$coefficients[,4][ninth.p] #CKD_per, p-val = 0.2213

model10 <- lm(deaths_per ~ diabetes_per + organ_per + Gini, data = my.data[c(-40, -6),])
ten.p <- which.max(summary(model10)$coefficients[,4]); 
summary(model10)$coefficients[,4][ten.p] #diabetes_per, p-val = 0.0937

model11 <- lm(deaths_per ~ organ_per + Gini, data = my.data[c(-40, -6),])
elev.p <- which.max(summary(model11)$coefficients[,4]); 
summary(model11)$coefficients[,4][elev.p]
summary(model11) #All has p-values < 0.05

predict(model11) #Fitted values
residuals(model11)
par(mfrow = c(2,2))
plot(model11)
plot(predict(model11), rstandard(model11))
hist(rstandard(model11), prob = TRUE); curve(dnorm(x, mean(rstandard(model11)), 
                                                  sd(rstandard(model11))), add = TRUE, 
                                            col = "green")
anova(model1, model2, model3, model4, model5, model6, model7, model8,
      model9, model10, model11) #H_0 hypothesis is accepted
anova(lm(deaths_per~1, data = my.data[c(-6, -40),]), model11) #The null model vs. the reduced model 

#R^2 adjusted is extracted from each model, to compare these
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared
summary(model4)$adj.r.squared
summary(model5)$adj.r.squared
summary(model6)$adj.r.squared
summary(model7)$adj.r.squared
summary(model8)$adj.r.squared
summary(model9)$adj.r.squared
summary(model10)$adj.r.squared
summary(model11)$adj.r.squared



