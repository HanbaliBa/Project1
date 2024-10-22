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






############## Test de Breusch-Pagan pour l'homoscédasticité

# Calcul des résidus
residuals <- residuals(lmodel.train)

# Tracer le graphique des résidus par rapport aux valeurs prédites
plot(lmodel.train$fitted.values, residuals, 
     xlab = "Valeurs prédites", ylab = "Résidus",
     main = "Graphique des résidus par rapport aux valeurs prédites")
abline(h = 0, col = "red")  # Ajouter une ligne horizontale à y = 0

library(lmtest)
bp_test <- bptest(lmodel.train)
bp_test

############## Calcul du VIF pour détecter la multicolinéarité (remove Age and Overall.rating )
library(car)
vif_values <- vif(lmodel.train)
print(vif_values)

##################### Test de Shapiro-Wilk pour la normalité des résidus
# Tracer l'histogramme des résidus
hist(residuals, breaks = 20,
     main = "Histogramme des résidus",
     xlab = "Résidus", ylab = "Fréquence")


shapiro_test <- shapiro.test(residuals(lmodel.train))
shapiro_test


# Test de Durbin-Watson pour l'autocorrélation des résidus
dw_test <- dwtest(lmodel.train)
dw_test
durbinWatsonTest(lmodel.train)



# Effectuez le test de Ramsey RESET
reset_test <- resettest(lmodel.train, power = 3)  # Test avec des termes polynomiaux d'ordre 3

# Affichez les résultats du test
reset_test


### model performance
library(performance)
check_model(lmodel.train)

#### model diagnostic
par(mfrow=c(2,2))
plot(lmodel.train)
par(mfrow=c(1,1))