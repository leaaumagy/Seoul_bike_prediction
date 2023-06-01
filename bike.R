# Authors : 
# Léa Aumagy
# Abdeldjallil Boukhalfa

#Project for Master 1 Economics in Aix Marseille School of Economics


library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(corrplot)
library(rpart)
library(rpart.plot) 
library(zoo)
library(tibble)
library(xgboost)
library(vars)
library(lubridate)

setwd("/home/leaaumagy/M1/R/projet")

#importation de la base de données
bike <- read.csv("SeoulBikeData.csv")

#Date : year-month-day
#Rented Bike count - Count of bikes rented at each hour
#Hour - Hour of he day
#Temperature-Temperature in Celsius
#Humidity - %
#Windspeed - m/s
#Visibility - 10m
#Dew point temperature - Celsius
#Solar radiation - MJ/m2
#Rainfall - mm
#Snowfall - cm
#Seasons - Winter, Spring, Summer, Autumn
#Holiday - Holiday/No holiday
#Functional Day - NoFunc(Non Functional Hours), Fun(Functional hours)

#information sur la base de données
dim(bike)
str(bike)
as_tibble(bike)
summary(bike)


#Préparation des données

#Je transforme ma colonne date d'une chaîne de caractère à une date.
bike = mutate(bike,
            Date = lubridate::dmy(Date))

#Binarisation de la variable Functioning.Day avec des valeurs de 0 et 1,
bike = mutate(bike, Functioning.Day = ifelse(Functioning.Day=="Yes", 0,1))
# je supprime les lignes ayant comme valeur 0 à Functioning.Day car ce sont les jours où la location ne fonctionne pas
bike = bike %>% filter(Functioning.Day !=1)
#Puis je supprime la colonne functioning.Day qui ne me sers plus.
bike$Functioning.Day = NULL




# Visualisation et analyse des variables

#On étudie le nombre moyen de location par jour de l'année.
mean_day = tapply(bike$Rented.Bike.Count, bike$Date, mean)
plot(mean_day,
     xlab="Day", 
     ylab="Mean Rented Bikes",
     main="Mean rented Bikes by days")

barplot(mean_day,
        xlab = "Date",
        ylab = "Nombre de vélos moyen loués",
        main = "Moyenne du nombre de vélo loué en fonction de la date",
        col = "blue",
        border = NA)
# Calculer la moyenne du nombre de vélos loués par jour pour les observations avec une faible visibilité
weak_visibility <- aggregate(Rented.Bike.Count ~ Date, 
                               data = bike[bike$Visibility..10m. < 500,], mean)
# Calculer la moyenne du nombre de vélos loués par jour pour les observations avec une bonne visibilité
good_visibility <- aggregate(Rented.Bike.Count ~ Date, 
                              data = bike[bike$Visibility..10m. >= 500,], mean)

# Tracer les deux graphiques
par(mfrow = c(2,1))
plot(weak_visibility, xlab = "Year", 
     ylab = "Mean Rented Bikes", 
     main = "Mean Rented Bikes on weak visibility in 2018")
plot(good_visibility, xlab = "Year", 
     ylab = "Mean Rented Bikes", 
     main = "Mean Rented Bikes on good visibility in 2018")



#Influence du jour de la semaine sur le nombre de location.
bike$Day = format(bike$Date, format ="%A")

# Calculer la moyenne du nombre de vélos loués par jour de la semaine
mean_week <- tapply(bike$Rented.Bike.Count, bike$Day, mean)
mean_week <- mean_week[order(factor(names(mean_week), levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))]
my_colors <- colorRampPalette(c("#77DD77", "#FFC0CB"))(length(mean_week))
# Tracer le graphique
barplot(mean_week, 
        xlab="Day", 
        ylab="Mean Rented Bikes", 
        main="Mean rented Bikes by day", 
        ylim=c(0, max(mean_week)*1.2), 
        col=my_colors)
#on observe peu de différence

#évolution des locations selon l'heure chaque jours
bike %>% 
  ggplot(aes(Hour, Rented.Bike.Count, color = Day)) +
  geom_smooth(se = F, size = 1) +
  scale_color_discrete("") +
  xlab("Hour") + 
  ylab("Mean Rented Bike") + 
  ggtitle("Bike rental per hour depending on the day") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#on observe que le samedi et le dimanche il y a un tendance différentes des autres jours, dû aux heures de pointes


#On étudie le nombre moyen de location par heure de la journée.
mean_hour <- tapply(bike$Rented.Bike.Count, bike$Hour, mean)
my_colors <- colorRampPalette(c("lightblue", "purple"))(length(mean_hour))
barplot(mean_hour, xlab = "Hour", 
        ylab = "Mean Rented Bikes", 
        main = "Mean rented Bikes by Hour",
        col=my_colors)



#On compare le nombre de location moyen journalier quand il neige et quand il ne neige pas.
# Sélectionner les données avec de la neige et celles sans neige
bike_snow <- bike[bike$Snowfall..cm. != 0, ]
bike_nosnow <- bike[bike$Snowfall..cm. == 0, ]

# Calculer la moyenne du nombre de vélos loués par jour pour chaque groupe
mean_snow <- tapply(bike_snow$Rented.Bike.Count, bike_snow$Date, mean)
mean_noswnow <- tapply(bike_nosnow$Rented.Bike.Count, bike_nosnow$Date, mean)

# Tracer les deux graphiques
par(mfrow = c(2, 1))
barplot(mean_snow, 
        xlab = "Days", 
        ylab = "Mean Rented Bikes", 
        main = "Mean rented Bikes on snowing days",
        col = "lightblue")
barplot(mean_noswnow, 
        xlab = "Days", 
        ylab = "Mean Rented Bikes", 
        main = "Mean rented Bikes on days without snow",
        col = "lightblue")


#On procéde de même pour la pluie.
# Sélectionner les données avec de la pluie et celles sans pluie
bike_rain <- bike[bike$Rainfall.mm. != 0, ]
bike_norain <- bike[bike$Rainfall.mm. == 0, ]

# Calculer la moyenne du nombre de vélos loués par jour pour chaque groupe
mean_rain <- tapply(bike_rain$Rented.Bike.Count, bike_rain$Date, mean)
mean_norain <- tapply(bike_norain$Rented.Bike.Count, bike_norain$Date, mean)

# Tracer les deux graphiques
par(mfrow = c(2, 1))
barplot(mean_rain, 
        xlab = "Days", 
        ylab = "Mean Rented Bikes", 
        main = "Mean rented Bikes on rainning days",
        col = "lightblue")
barplot(mean_norain, 
        xlab = "Days", 
        ylab = "Mean Rented Bikes", 
        main = "Mean rented Bikes on days without rain",
        col = "lightblue")


#Analyse de la radiation

ggplot(bike, aes(x = Hour, y = Rented.Bike.Count)) +
  geom_point(aes(color = Solar.Radiation..MJ.m2.), size = 2) +
  labs(x = "Hour", y = "Rented.Bike", color = "Solar Radiation") +
  ggtitle("Bike rental according to solar radiation every hour")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#Oon constate que l'après midi est le moment où il y a le plus de radiation solaire et le nombre de location augmente
#mais rien ne nous prouve sur la relation entre ces deux variables


#On étudie la température moyenne durant l'année.
#moyenne pour chaque date
mean_temperature = tapply(bike$Temperature.C., bike$Date, mean)
barplot(mean_temperature,
        ylab="Mean Temperature", 
        main="Mean Temperature by days in 2018")
#évolution de la location de vélo selon la température
bike %>% 
  ggplot(aes(Temperature.C., Rented.Bike.Count)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F) +
  xlab("Temperature (°C)") + 
  ylab("Rented Bike") + 
  ggtitle("Bike rental depending on the temperature")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#On observe une tendance commune entre nombre de location et la température
#mais ça diminue au dela de 27 degré car il fait trop chaud pour faire du vélo


#On étudier le nombre de location selon si c'est une période de vacance ou non
ggplot(bike, aes(x = Holiday, y = Rented.Bike.Count, fill = Holiday)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Rented bike by holidays", x = "", y = "Rented Bike") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# il y a moins de vélo loués en période de vacances


#On représente le nombre de location moyenne par saison, en fonction des températures.
mean_seasons <- bike %>%
  group_by(Seasons) %>%
  summarize(mean_rentals = mean(Rented.Bike.Count), 
            mean_temperature = mean(Temperature.C.))

ggplot(mean_seasons, aes(x = factor(Seasons, levels = c("Winter", "Spring",
                                                        "Summer", "Autumn")),
                         y = mean_rentals, fill = mean_temperature)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(x = "Season", y = "Mean Bike Rentals", fill = "Temperature (°C)") +
  ggtitle("Mean Bike Rentals by Season and Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
#on voit que l'été il y a une plus forte moyenne de location de vélo


#Analyse de la corrélation des variables


##Test d'independance  : Test ANOVA
test_anova <- aov(Rented.Bike.Count ~ Temperature.C. + 
                  Hour +
                  Seasons +
                  Humidity... + 
                  Wind.speed..m.s. + 
                  Rainfall.mm. + 
                  Snowfall..cm.+
                  Solar.Radiation..MJ.m2. +
                  Visibility..10m.+
                  Dew.point.temperature.C., 
                data = bike)
summary(test_anova)
#Les variable "snowfall", "Wind.speed..m.s." et "Visibility..10m." ne sont pas significative


# Analyse de la corrélation entre les différentes variables
bike = mutate(bike, Seasons = case_when(Seasons == "Winter" ~ 1,
                                       Seasons == "Spring" ~ 2,
                                       Seasons == "Summer" ~ 3,
                                       Seasons == "Autumn" ~ 4))

correlation_matrix <- cor(bike[, c("Hour", "Temperature.C.", "Humidity...", 
                                   "Wind.speed..m.s.", "Seasons", 
                                   "Solar.Radiation..MJ.m2.", "Rainfall.mm.")])

corrplot(correlation_matrix, type = "upper", 
         method = "color", 
         tl.col = "black",
         addCoef.col = "black",
         diag=FALSE)
#La saison est corrélée avec la température 
#"Dew.point.temperature.C." est corrélé avec la température

#Dans notre modèle de regression nous pouvons donc exclure les variables qui sont non significatives
#On peut également exclure les variables qui ont des corrélations comme "Seasons" et "Dew.point.temperature.C."


# Analyse économétrique : Modèle MCO

#J'effectue une régression linéaire avec comme variable dépendante le nombre de vélos loué par heures 
model_OLS = lm(Rented.Bike.Count ~ Hour + Temperature.C. + 
                 Humidity... + Rainfall.mm. + 
                 Solar.Radiation..MJ.m2. , data = bike)
summary(model_OLS)

#A partir de ce model OLS, on voit que toute les variables du modèle sont significative 
#on voit aussi leur effet sur le nombre de location de vélo




#Prédiction

#Decision Tree

# Créer un modele "rpart" à partir du modèle OLS
model_tree <- rpart(Rented.Bike.Count ~ Hour + Temperature.C. + 
                      Humidity... + Rainfall.mm. + Solar.Radiation..MJ.m2., 
                    data = bike)

# Afficher l'arbre de décision avec rpart.plot
rpart.plot(model_tree, main = "Decision Tree for Rented.Bike.Count", 
           under = TRUE, 
           cex.main = 1.2, 
           box.col = "cyan", 
           branch.lty = 3, 
           shadow.col = "black", 
           nn = TRUE)

# Ajouter un résumé de l'arbre de décision
summary(model_tree)

#Les variables les plus importantes pour la prédiction sont la température et l'heure, suivies de l'humidité, de la radiation solaire et de la pluie.



# Prédiction du nombre de location selon la valeur des variables
#on suppose qu'il est 10H, où il fait 20°, un taux d'humidité de 20%, pas de pluie, et un indice de radiation solaire de 1
predic_hight <- data.frame(Hour = 10, Temperature.C. = 20, 
                           Humidity... = 70, Rainfall.mm. = 0, 
                           Solar.Radiation..MJ.m2. = 1)
predicted_hight <- predict(model_tree, predic_hight)
# Afficher les prédictions
predicted_hight


#Si on change les valeurs, où on augmente l'heure de la journée à 21h, une température de 5°, un taux d'humidité à 60%, présence de 3mm de pluie, et un indice de radiation solaire à 0.
#La prédiction du nombre de location de vélo devrait fortement diminuer
#on obtient ce nouveau model:
predic_low <- data.frame(Hour = 21, Temperature.C. = 5, 
                         Humidity... = 60, Rainfall.mm. = 3, 
                         Solar.Radiation..MJ.m2. = 0)
predicted_low <- predict(model_tree, predic_low)
# Afficher les prédictions
predicted_low


# Nous pouvons utiliser une nouvelle base de données, qui contient les informations météorologique de 2017 à 2023
# Nous pouvons prédire le nombre de location de vélo indépendemment d'autres effets extérieur

# Charger la nouvelle base de données
Seoul_bike <- read.csv("météo_Seoul.csv")

#préparation des données, pour avoir le même format que la base précédente "bike"

#convertir la colonne "Date" en date et sépérer l'heure dans une nouvelle colonne
Seoul_bike <- separate(Seoul_bike, col = "Date", into = c("Date", "Hour"), sep = " ")
Seoul_bike = mutate(Seoul_bike,
              Date = lubridate::ymd(Date))
#on garde seulement l'heure de la colonne heure
Seoul_bike$Hour <- as.numeric(substr(Seoul_bike$Hour, 1, 2))

#On remplace les valeurs manquantes sous format NA et on convertir les colonnes dans un bon format
#On remplace les valeurs manquantes sous format NA et on convertit les colonnes dans un bon format
Seoul_bike$Rainfall.mm. <- as.numeric(gsub(",", ".", Seoul_bike$Rainfall.mm.), na.rm = TRUE)
Seoul_bike$Hour <- as.numeric(Seoul_bike$Hour, na.rm = TRUE)
Seoul_bike$Wind.speed..m.s. <- as.numeric(gsub(",", ".", Seoul_bike$Wind.speed..m.s.), na.rm = TRUE)
Seoul_bike$Humidity... <- as.numeric(gsub(",", ".", Seoul_bike$Humidity...), na.rm = TRUE)
Seoul_bike$Solar.Radiation..MJ.m2. <- as.numeric(gsub(",", ".", Seoul_bike$Solar.Radiation..MJ.m2.), na.rm = TRUE)
Seoul_bike$Visibility..10m. <- as.numeric(gsub(",", ".", Seoul_bike$Visibility..10m.), na.rm = TRUE)
Seoul_bike$Temperature.C. <- as.numeric(gsub(",", ".", Seoul_bike$Temperature.C.))

#on supprimer la colonne snowfall
Seoul_bike$Snowfall.cm. <- NULL

#on analyse les valeurs manquantes dans chaque colonnes
colSums(is.na(Seoul_bike))

#on supprimer les lignes de la colonne "Date" qui ne possède pas de Date car se n'est pas des lignes d'observation
Seoul_bike <- Seoul_bike[complete.cases(Seoul_bike$Date), ]

#on remplace les valeurs manquante de Rainfall, Wind.spee, humidity, solar.radiatio par des 0
Seoul_bike$Temperature.C. <- ifelse(is.na(Seoul_bike$Temperature.C.), 0, Seoul_bike$Temperature.C.)
Seoul_bike$Rainfall.mm. <- ifelse(is.na(Seoul_bike$Rainfall.mm.), 0, Seoul_bike$Rainfall.mm.)
Seoul_bike$Wind.speed..m.s. <- ifelse(is.na(Seoul_bike$Wind.speed..m.s.), 0, Seoul_bike$Wind.speed..m.s.)
Seoul_bike$Humidity... <- ifelse(is.na(Seoul_bike$Humidity...), 0, Seoul_bike$Humidity...)
Seoul_bike$Solar.Radiation..MJ.m2. <- ifelse(is.na(Seoul_bike$Solar.Radiation..MJ.m2.), 0, Seoul_bike$Solar.Radiation..MJ.m2.)

#Notre deuxième base de données est similaire à notre première base
View(Seoul_bike)



#Prédiction avec l'abre de décision

# Extraire les variables utilisées dans l'arbre de décision
vars <- c("Hour", "Temperature.C.", "Humidity...", "Rainfall.mm.", "Solar.Radiation..MJ.m2.")

# Prédire les valeurs de "Rented.Bike.Count" à partir de l'arbre de décision
Seoul_bike$Rented.Bike.Predict <- predict(model_tree, newdata = Seoul_bike[vars])

#On arrondi le nombre de location de vélo louée en nombre entier
Seoul_bike$Rented.Bike.Predict <- round(Seoul_bike$Rented.Bike.Predict, 0)

#Graphique pour visualiser la prédiction
mean_day_pred = tapply(Seoul_bike$Rented.Bike.Predict, Seoul_bike$Date, mean)
barplot(mean_day_pred,
     xlab="Day", 
     ylab="Mean Rented Bikes predict",
     main="Mean rented Bikes predict by days",
     ylim=c(0, max(mean_day_pred)*1.1))


#Graphique avec moyenne mobile par mois pour une meilleure visualisation

# on compute le nombre de location par jours
daily_counts_predict <- aggregate(Rented.Bike.Predict ~ Date, data = Seoul_bike, mean)
names(daily_counts_predict)[2] <- "daily_counts_predict"

Seoul_bike <- merge(Seoul_bike, daily_counts_predict, by = "Date")
Seoul_bike$daily_counts_predict <- round(Seoul_bike$daily_counts_predict, 0)

#graphique
Seoul_bike %>%
  mutate(rolling_avg = rollmean(daily_counts_predict, k = 360, align = "right", fill = NA)) %>%
  ggplot() +
  geom_line(mapping = aes(x = Date, y = rolling_avg), color = "blue") +
  xlab("Date") + 
  ylab("Nombre de vélo loués") + 
  ggtitle("Moyenne mobile par an du nombre de vélo loué prédit de 2017 à 2023") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#On peut observer des tendance lié à la saisonnalité
#Des effets sur le longterme peuvent influence le nombre de location  Covid, inflation, prix du gazoil, ...




#comparaison de la prédiction et ce qui a été réellement dans la database "bike" avec la moyenne mobile
#fusion des base
bike <- merge(bike, Seoul_bike[, c("Date", "Hour", "Rented.Bike.Predict", "daily_counts_predict")], by = c("Date", "Hour"), all.x = TRUE)

#moyenne mobile des location de vélo réel
daily_counts <- aggregate(Rented.Bike.Count ~ Date, data = bike, mean)
names(daily_counts)[2] <- "daily_counts"

# Merge daily_counts with Seoul_bike based on the Date column
bike <- merge(bike, daily_counts, by = "Date")
bike$daily_counts <- round(bike$daily_counts, 0)


# Création du graphique
ggplot(bike, aes(x = Date)) + 
  geom_line(aes(y = daily_counts, color = "Observed")) + 
  geom_line(aes(y = daily_counts_predict, color = "Predicted")) + 
  labs(title = "Evolution du nombre de vélo loués prédit et réel", 
       x = "Date", y = "Nombre de vélo loués", color = "Data Source") +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#Modele Xgboost

# Préparation des données pour le modèle
train_matrix <- model.matrix(Rented.Bike.Count ~ Hour + Temperature.C. + Humidity... + Rainfall.mm. + Solar.Radiation..MJ.m2., data = bike)
train_label <- bike$Rented.Bike.Count

# Créer un modèle XGBoost
xgb_model <- xgboost(data = train_matrix, label = train_label, max.depth = 6, eta = 0.3, nthread = 2, nrounds = 50, objective = "reg:linear")
  
# Faire des prédictions sur de nouvelles données
test_matrix <- model.matrix(Rented.Bike.Count ~ Hour + Temperature.C. + Humidity... + Rainfall.mm. + Solar.Radiation..MJ.m2., data = bike)
bike$predictions_xgboost <- predict(xgb_model, test_matrix)
bike$predictions_xgboost <- round(bike$predictions_xgboost, 0)

#Graphique pour visualiser la prédiction
mean_day_pred_xgboost = tapply(bike$predictions_xgboost, bike$Date, mean)
barplot(mean_day_pred_xgboost,
        xlab="Day", 
        ylab="Mean Rented Bikes predict",
        main="Mean rented Bikes predict by days")


#Graphique avec comparaison entre la réalité et la prédiction avec xgboost

# Compute the daily number of Rented.Bike.predict
daily_counts_predict_xgboost <- aggregate(predictions_xgboost ~ Date, data = bike, mean)
names(daily_counts_predict_xgboost)[2] <- "daily_counts_predict_xgboost"

# Merge daily_counts with Seoul_bike based on the Date column
bike <- merge(bike, daily_counts_predict_xgboost, by = "Date")
bike$daily_counts_predict_xgboost <- round(bike$daily_counts_predict_xgboost, 0)

# Création du graphique
ggplot(bike, aes(x = Date)) + 
  geom_line(aes(y = daily_counts, color = "Observed")) + 
  geom_line(aes(y = daily_counts_predict_xgboost, color = "Predicted")) + 
  labs(title = "Evolution of Rented.Bike and Rented.Bike.Predict with xgboost", 
       x = "Date", y = "Nombre de vélo loués", color = "Data Source") +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#on observe une meilleur prédiction avec la méthode xgboost


# Prédiction sur la base Séoul_bike

# Créer une matrice des variables heures, température, humidité, précipitations et rayonnement solaire
hour_matrix <- data.frame(Hour = Seoul_bike$Hour, 
                          Temperature.C. = Seoul_bike$Temperature.C., 
                          Humidity... = Seoul_bike$Humidity..., 
                          Rainfall.mm. = Seoul_bike$Rainfall.mm., 
                          Solar.Radiation..MJ.m2. = Seoul_bike$Solar.Radiation..MJ.m2.)

# Transformer la matrice en matrice modèle pour la prédiction
hour_matrix <- model.matrix(~ ., data = hour_matrix)

# Faire des prédictions pour chaque heure de la journée à partir du modèle XGBoost
Seoul_bike$Rented.Bike.xgboost <- predict(xgb_model, hour_matrix)

#On arrondi le nombre de location de vélo louée en nombre entier
Seoul_bike$Rented.Bike.xgboost <- round(Seoul_bike$Rented.Bike.xgboost, 0)


#Graphique pour visualiser la prédiction
mean_day_pred = tapply(Seoul_bike$Rented.Bike.xgboost, Seoul_bike$Date, mean)
barplot(mean_day_pred,
        xlab="Day", 
        ylab="Mean Rented Bikes predict by xgboost",
        main="Mean rented Bikes predict per days by xgboost",
        ylim=c(0, max(mean_day_pred)*1.1))



# Tracer la courbe des prédictions en fonction de la date

daily_counts_xgboost <- aggregate(Rented.Bike.xgboost ~ Date, data = Seoul_bike, mean)
names(daily_counts_xgboost)[2] <- "daily_counts_xgboost"

Seoul_bike <- merge(Seoul_bike, daily_counts_xgboost, by = "Date")
Seoul_bike$daily_counts_xgboost <- round(Seoul_bike$daily_counts_xgboost, 0)

ggplot(data = Seoul_bike, aes(x = Date)) +
  geom_line(aes(y = daily_counts_xgboost)) +
  labs(x = "Date", y = "Rented Bike Count") +
  ggtitle("Predicted Rented Bike Count with xgboost") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Graphique avec moyenne mobile par mois pour une meilleure visualisation

#graphique
Seoul_bike %>%
  mutate(rolling_avg = rollmean(daily_counts_xgboost, k = 360, align = "right", fill = NA))%>%
  ggplot +
  geom_line(mapping = aes(x = Date, y = rolling_avg), color = "blue")+
  xlab("Date") + 
  ylab("Rented.Bike.xgboost") + 
  ggtitle("Rollmean per year on Rent bike predict from 2017 to 2023") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




#Visualisation pour le contrefactuel du covid pour l'année 2020

# Filtrer les observations pour l'année 2020
Seoul_bike_2020 <- Seoul_bike %>% filter(year(Date) == 2020)

# Tracer la courbe des prédictions en fonction de la date pour l'année 2020
ggplot(data = Seoul_bike_2020, aes(x = Date)) +
  geom_line(aes(y = daily_counts_xgboost), color = "darkgreen") +
  labs(x = "Date", y = "Rented Bike Count") +
  ggtitle("Predicted Rented Bike Count with xgboost for 2020") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Prédictions pour l'année future

tail(Seoul_bike$Date, n = 1)
#notre base de donnée se termine le 29 mars 2023
# nous allons donc faire des prédiction du 30 mars 2023 au 30 mars 2024
df <- Seoul_bike
dates <- seq(as.Date("2023-03-30"), by = "day", length.out = 365)
hours <- 0:23
datetime <- expand.grid(Date = dates, Hour = hours)

predictions_df <- data.frame(Date = datetime$Date, Hour = rep(hours, length(dates)),
                             Temperature.C. = rep(NA, nrow(datetime)), 
                             Rainfall.mm. = rep(NA, nrow(datetime)), 
                             Wind.speed..m.s. = rep(NA, nrow(datetime)),
                             Humidity... = rep(NA, nrow(datetime)),
                             Solar.Radiation..MJ.m2. = rep(NA, nrow(datetime)),
                             Visibility..10m. = rep(NA, nrow(datetime)))

for (i in 1:nrow(predictions_df)) {
  # extraire le mois et le jour de la date de la ligne i de predictions_df
  month_day_i <- format(predictions_df$Date[i], "%m-%d")
  
  # extraire l'heure de la ligne i de predictions_df
  hour_i <- predictions_df$Hour[i]
  
  # extraire les lignes de df qui correspondent au mois/jour/heure de la ligne i de predictions_df
  matching_rows <- df[format(df$Date, "%m-%d") == month_day_i & df$Hour == hour_i, ]
  
  # calculer la moyenne de chaque variable pour les lignes correspondantes dans df
  mean_temp <- mean(matching_rows$Temperature.C.)
  mean_rain <- mean(matching_rows$Rainfall.mm.)
  mean_wind <- mean(matching_rows$Wind.speed..m.s.)
  mean_humidity <- mean(matching_rows$Humidity...)
  mean_solar <- mean(matching_rows$Solar.Radiation..MJ.m2.)
  mean_visibility <- mean(matching_rows$Visibility..10m.)
  
  # remplacer les valeurs correspondantes dans predictions_df par les moyennes calculées
  predictions_df$Temperature.C.[i] <- mean_temp
  predictions_df$Rainfall.mm.[i] <- mean_rain
  predictions_df$Wind.speed..m.s.[i] <- mean_wind
  predictions_df$Humidity...[i] <- mean_humidity
  predictions_df$Solar.Radiation..MJ.m2.[i] <- mean_solar
  predictions_df$Visibility..10m.[i] <- mean_visibility
}

predictions_df = mutate(predictions_df,
                        Date = lubridate::ymd(Date))
#on garde seulement l'heure de la colonne heure
predictions_df$Hour <- as.numeric(substr(predictions_df$Hour, 1, 2))
# Supprimer les lignes contenant des NA pour la variable Temperature.C.
predictions_df <- na.omit(predictions_df, cols = "Temperature.C.")
colnames(predictions_df) <- c("Date", "Hour", "Temperature.C.", "Rainfall.mm.", "Wind.speed..m.s", "Humidity...", "SolarRadiation..MJ.m2.", "Visibility..10m.")

hour_matrix <- data.frame(Hour = predictions_df$Hour, 
                          Temperature.C. = predictions_df$Temperature.C., 
                          Humidity... = predictions_df$Humidity..., 
                          Rainfall.mm. = predictions_df$Rainfall.mm., 
                          Solar.Radiation..MJ.m2. = predictions_df$SolarRadiation..MJ.m2.)

hour_matrix <- model.matrix(~ ., data = hour_matrix)

 hour_predictions <- predict(xgb_model, hour_matrix)
hour_predictions <- pmax(hour_predictions, 0)
hourly_rentals <- data.frame(Hour = predictions_df$Hour, Predicted_Rentals = hour_predictions)

predictions_df <- cbind(predictions_df, hourly_rentals$Predicted_Rentals)

names(predictions_df)[9] <- "predictions"

ggplot(data = predictions_df, aes(x = Date)) +
  geom_line(aes(y = predictions, color = "Predicted")) +
  labs(x = "Date", y = "Rented Bike Count", color = "Legend") +
  ggtitle("Predicted Rented Bike Count")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Agréger les données par jour en prenant la moyenne journalière
daily_predictions <- aggregate(predictions_df$predictions, 
                               by = list(Date = predictions_df$Date), 
                               FUN = mean)

# Tracer le graphique des prédictions journalières
ggplot(data = daily_predictions, aes(x = Date)) +
  geom_line(aes(y = x), color = "red") +
  labs(x = "Date", y = "Moyenne journalière des locations de vélo") +
  ggtitle("Prédiction des location de vélo de Mars 2023 à Mars 2024") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

