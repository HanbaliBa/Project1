# DATA 
library(readr)
data <- read_csv("F:/data_in_cloud/project/data/Ecommerce_users")
data <- data[c("Avg. Session Length",
               "Time on App",
               "Length of Membership",
               "Yearly Amount Spent")]
summary(data)

# 
library(ggplot2)

# pairplot of all variables --> 
#### length of membership seems the most correlated
pairs(data,
      pch = 16,
      labels = c("Avg Session Length", 
                 "Time on App", 
                 "Time on website",
                 "Length of Membership",
                 "Yearly spent"),
      main = "Pairplot of all variables")

# EXPLORING THE VARIABLES

### is the variable normally distributed?
hist(data$`Length of Membership`)


# check distribution with boxplot
boxplot(data$`Length of Membership`)


# MODEL


lmodel <- lm(`Yearly Amount Spent`~.,data=data)

summary(lmodel)


# EVALUATE THE MODEL
# ---------------------------------------------------------------------------

# create a random training and a testing set
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

# estimate the linear fit with the training set
lmodel.train <- lm(`Yearly Amount Spent`~.,data=train)
summary(lmodel.train)

# predict on testing set
prediction <- predict(lmodel.train, newdata = test)
err <- prediction - test$`Yearly Amount Spent`
rmse <- sqrt(mean(err^2))
mape <- mean(abs(err/test$`Yearly Amount Spent`))

c(RMSE=rmse,mape=mape,R2=summary(lmodel.train)$r.squared)






############## Test de Breusch-Pagan pour l'homosc�dasticit�

# Calcul des r�sidus
residuals <- residuals(lmodel.train)

# Tracer le graphique des r�sidus par rapport aux valeurs pr�dites
plot(lmodel.train$fitted.values, residuals, 
     xlab = "Valeurs pr�dites", ylab = "R�sidus",
     main = "Graphique des r�sidus par rapport aux valeurs pr�dites")
abline(h = 0, col = "red")  # Ajouter une ligne horizontale � y = 0

library(lmtest)
bp_test <- bptest(lmodel.train)
bp_test

############## Calcul du VIF pour d�tecter la multicolin�arit� (remove Age and Overall.rating )
library(car)
vif_values <- vif(lmodel.train)
print(vif_values)

##################### Test de Shapiro-Wilk pour la normalit� des r�sidus
# Tracer l'histogramme des r�sidus
hist(residuals, breaks = 20,
     main = "Histogramme des r�sidus",
     xlab = "R�sidus", ylab = "Fr�quence")


shapiro_test <- shapiro.test(residuals(lmodel.train))
shapiro_test


# Test de Durbin-Watson pour l'autocorr�lation des r�sidus
dw_test <- dwtest(lmodel.train)
dw_test
durbinWatsonTest(lmodel.train)



# Effectuez le test de Ramsey RESET
reset_test <- resettest(lmodel.train, power = 3)  # Test avec des termes polynomiaux d'ordre 3

# Affichez les r�sultats du test
reset_test


### model performance
library(performance)
check_model(lmodel.train)

#### model diagnostic
par(mfrow=c(2,2))
plot(lmodel.train)
par(mfrow=c(1,1))