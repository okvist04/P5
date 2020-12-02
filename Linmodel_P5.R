model <- lm(Deaths ~ . -Country -Deaths, data = my.data)
multi.col <- vif(model); multi.col

model1 <- lm(Deaths ~ . -Country -Deaths -CKD, data = my.data)
multi.col1 <- vif(model1); multi.col1

model2 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older`, data = my.data)
multi.col2 <- vif(model2); multi.col2
max <- max(multi.col2); max

model3 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -`70_older`, data = my.data)
multi.col3 <- vif(model3); multi.col3
max1 <- max(multi.col3); max1

model4 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -`70_older`-Median_age, data = my.data)
multi.col4 <- vif(model4); multi.col4
max2 <- max(multi.col4); max2

model5 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -Organ_transplant 
             -Median_age -`70_older`, data = my.data)
multi.col5 <- vif(model5); multi.col5
max3 <- max(multi.col5); max3

model8 <- lm(Deaths ~ . -Country -Deaths -CKD -`65_older` -Organ_transplant 
             -Median_age -`70_older` -HDI, data = my.data)
multi.col8 <- vif(model8); multi.col8
info8 <- summary(model8); info8
res8 <- residuals(model8)
stand.res8 <- rstandard(model8)
par(mfrow = c(2,2))
qqnorm(res8); qqline(res8, col = "red")
hist(res8, prob = TRUE, xlab = "Residuals"); 
curve(dnorm(x, mean(res8), sd(res8)), add = TRUE, col = "green")
boxplot(predict(model8), main = "Boxplot of Fitted Values")
plot(stand.res8, main = "Residuals")

yhat8 <- predict(model8)
plot(yhat8, stand.res8) #Trumpet shape, i.e. log-transform y

log.model <- lm(log(Deaths) ~ . -Country -Deaths -CKD -`65_older` -`70_older` 
                -Median_age -Organ_transplant -HDI, data = my.data)
info.log <- summary(log.model); info.log #Obesity p=0.9407
log.res <- residuals(log.model)
stand.logres <- rstandard(log.model)
par(mfrow = c(2,1))
qqnorm(stand.logres); qqline(stand.logres, col = "red")
plot(predict(log.model), stand.logres)
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

log.model1 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Diabetes + Smokers + COPD + Health_exp + Obesity + Asthma + Cancer + Gini,
                 data = my.data)
info.log1 <- summary(log.model1); info.log1
stand.logres1 <- rstandard(log.model1)
qqnorm(stand.logres1); qqline(stand.logres1, col = "red")
plot(predict(log.model1), stand.logres1)
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
plot(my.data$Gini, stand.logres1)

hist(log(my.data$Deaths), prob = TRUE); curve(dnorm(x, mean(log(my.data$Deaths)), sd(log(my.data$Deaths))), 
                                              add = TRUE, col = "green")

hist(my.data$Deaths, prob = TRUE); curve(dnorm(x, mean(my.data$Deaths), sd(my.data$Deaths)), 
                                              add = TRUE, col = "green")

qqnorm(log(my.data$Deaths)); qqline(log(my.data$Deaths), col = "red")
qqnorm(my.data$Deaths); qqline(my.data$Deaths, col = "red")
qqnorm(stand.logres1); qqline(stand.logres1, col = "red")

#Evt brug død antal pr. 100.000 indbyggere, find egenværdier og egenvektorer for X^\TX, hvis en af dem
#er 0, så er der mindst en der skal fjernes, forholdet mellem den største og mindste egeneværdi
#standardiser alle x-variablerne

plot(log.model1)

plot(rstudent(log.model1))
options(max.print = 1500)
influence.measures(log.model1)
cooks.distance(log.model1)
plot(cooks.distance(log.model1))
influence(log.model1)
leveragePlots(log.model1)

log.model2 <- lm(log(Deaths) ~ log(Population) + log(Pop.density) + log(Cancer) + GDP + Cardiovasc
                 + Diabetes + Smokers + COPD + Health_exp + Obesity + Asthma + Cancer + Gini,
                 data = my.data[c(-53),])
summary(log.model2)

hist(residuals(log.model2), prob = TRUE); curve(dnorm(x, mean(residuals(log.model2)),
                                                              sd(residuals(log.model2))), add = TRUE,
                                                                 col = "green")

global_model = lm(Deaths ~ . -Country -Deaths, data = my.data, na.action = "na.fail")
all_models = lapply(MuMIn::dredge(global_model, evaluate = FALSE), eval)
