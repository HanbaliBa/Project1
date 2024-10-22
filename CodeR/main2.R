residuals <- residuals(lmodel.train)

# Graphique des résidus du modèle d'entraînement
ggplot(train, aes(x=1:nrow(train), y=residuals)) +
  geom_point() +
  ggtitle("Graphique des résidus du modèle d'entraînement") +
  xlab("Index des observations") +
  ylab("Résidus") +
  theme_minimal()


# Histogramme des résidus de la régression
ggplot(train, aes(x=residuals)) +
  geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogramme des résidus de la régression") +
  xlab("Résidus") +
  ylab("Fréquence") +
  theme_minimal()

# Distribution des résidus de la régression
ggplot() +
  geom_density(aes(x=residuals), fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des résidus de la régression",
       x = "Résidus",
       y = "Densité") +
  theme_minimal()


# R^2 , RMSE , MAE

library(caret)
predictions <- predict(lmodel.train, newdata = test)

print(paste("Coefficient de détermination (R^2) :", R2(pred = predictions, obs = test$`Yearly Amount Spent`)))
print(paste("Erreur quadratique moyenne (RMSE) :", RMSE(predictions, test$`Yearly Amount Spent`)))
print(paste("Erreur absolue moyenne (MAE) :", MAE(predictions,test$`Yearly Amount Spent`)))
