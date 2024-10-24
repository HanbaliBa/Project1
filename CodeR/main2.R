residuals <- residuals(lmodel.train)

# Graphique des r�sidus du mod�le d'entra�nement
ggplot(train, aes(x=1:nrow(train), y=residuals)) +
  geom_point() +
  ggtitle("Graphique des r�sidus du mod�le d'entra�nement") +
  xlab("Index des observations") +
  ylab("R�sidus") +
  theme_minimal()


# Histogramme des r�sidus de la r�gression
ggplot(train, aes(x=residuals)) +
  geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogramme des r�sidus de la r�gression") +
  xlab("R�sidus") +
  ylab("Fr�quence") +
  theme_minimal()

# Distribution des r�sidus de la r�gression
ggplot() +
  geom_density(aes(x=residuals), fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des r�sidus de la r�gression",
       x = "R�sidus",
       y = "Densit�") +
  theme_minimal()


# R^2 , RMSE , MAE

library(caret)
predictions <- predict(lmodel.train, newdata = test)

print(paste("Coefficient de d�termination (R^2) :", R2(pred = predictions, obs = test$`Yearly Amount Spent`)))
print(paste("Erreur quadratique moyenne (RMSE) :", RMSE(predictions, test$`Yearly Amount Spent`)))
print(paste("Erreur absolue moyenne (MAE) :", MAE(predictions,test$`Yearly Amount Spent`)))
