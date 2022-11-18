### R à QC 2019 - Prévision de séries temporelles ###
### 2019-05-13
###

library(forecast)
library(ggplot2)

## Chapitre 1 - Exercices
##

## 1 Importez le fichier "tute1.csv". Il s'agit de ventes trimestrielles d'une petite compagnie (variable "Ventes") 
## entre 1981 et 1995. Il y a aussi une variable Budget Publicitaire ("BudgPubl") et le Produit Intérieur Brut ("PIB").
tute1 <- read.csv("./donnees/tute1.csv", header = TRUE)
str(tute1)
head(tute1)

## 2 Convertissez-les en séries temporelles, après avoir ôté la première colonne inutile ici.
tute_ts <- ts(tute1[, -1], start = c(1981), frequency = 4)
summary(tute_ts)


## 3 Tracez les trois séries temporelles.
autoplot(tute_ts[,"Ventes"])+
  ggtitle("Nombre trimestriel de ventes")+
  xlab("Trimestre")+
  ylab("Nombre de ventes")

autoplot(tute_ts[,"BudgPubl"])+
  ggtitle("Budget public")+
  xlab("Trimestre")+
  ylab("$")

autoplot(tute_ts[,"PIB"])+
  ggtitle("PIB")+
  xlab("Trimestre")+
  ylab("$")

autoplot(tute_ts, facets = TRUE)

## 4 Décomposez la série Ventes en tendance, saison et résidus. Tracez également l'autocorrélogramme.
tute_ts[,"Ventes"]  %>%
  decompose(type="multiplicative") %>%
  autoplot() + xlab("Temps") +
  ggtitle("Décomposition multiplicative de la série Ventes")

ggAcf(tute_ts[,"Ventes"])

## Chapitre 2 - Exercices
##

## 1 Importez la série mensuelle "lait.dat" démarrant en janvier 2005. Tracez-là.
lait <- read.table("./donnees/lait.dat", header = TRUE)
summary(lait)

lait_ts <- ts(lait, start = c(2005, 1), frequency = 12)

lait_ts %>% autoplot()

## 2 Séparez les données en deux parties ( la première "lait.train" allant de janvier 2005 à décembre 2017, 
##   et l'autre "lait.test", démarrant en janvier 2018). Tracez-les.
lait.train <- window(lait_ts, start = c(2005, 1), end = c(2017, 12))
lait.test <- window(lait_ts, start = c(2018, 1))

autoplot(lait.train) +
  autolayer(lait.train, series="Train") + 
  autolayer(lait.test, series="Test")

## 3 Calculez les prévisions saisonnières naïves sur 12 mois à partir de la série "lait.train". Faites un graphique.
lait.snaive <- snaive(lait.train, h = 12)

autoplot(lait.train) +
  autolayer(lait.test, series="Réalisé") +
  autolayer(lait.snaive, series="Prévision", PI=FALSE)

## 4 Comparez la pertinence de vos prévisions vis-à-vis du réalisé a posteriori.
accuracy(lait.snaive, lait.test)

## 5 Examinez les résidus. Commentez.
checkresiduals(lait.snaive)

## Chapitre 3 - Exercices
##

## Reprendre la série "lait" de l'exercice du chapitre 2.
## 1 Utilisez la commande "ets" pour prévoir à horizon 12 la série "lait". Faites un graphique.
lait.ets <- ets(lait_ts, model = "ZZZ")
autoplot(lait.ets)

lait.ets %>% forecast(h = 12) %>% autoplot()

## 2 Reprendre la série "lait.train" pour faire des prévisions sur une année via l'approche ets. Faites un 
##   graphique illustratif pour comparer les prévisions et le réalisé a posteriori.
lait.train.ets <- ets(lait.train, model = "ZZZ")
lait.train.ets %>% forecast(h = 12) %>% autoplot()
lait.train.ets <- forecast(lait.train.ets, h = 12)
autoplot(lait_ts) +
  autolayer(lait.train.ets, series = "Prévision") +
  autolayer(lait.test, series = "Test")

## 3 Comparez les pertinences des prévisions faites via "snaïve" et via "ets".
accuracy(lait.snaive, lait.test)
accuracy(lait.train.ets, lait.test)

## Chapitre 4 - Exercices
##

## 1 Importez la série "electricity.dat", série mensuelle débutant en janvier 1986. 
##   Faites-en un graphique.
elect <- read.table("./donnees/electricity.dat", header = TRUE)
summary(elect)

elect_ts <- ts(elect, start = c(1986, 1), frequency = 12)
elect_ts %>% autoplot() + ggtitle("Série Electricity")

## 2 En utilisant un modèle SARIMA, faites des prévisions sur 2 ans.
elect.arima <- elect_ts %>% auto.arima() %>% forecast(h = 24)
summary(elect.arima)
elect.arima %>% autoplot()

## 3 Tronquez fictivement les 60 dernières observations et faites des prévisions via 
##   un modèle SARIMA et une approche "ets".
elect.train <- window(elect_ts, end = c(2013, 12))
elect.test<- window(elect_ts, start = 2014)

elect.train.arima <- elect.train %>% auto.arima() %>% forecast(h = 60)
elect.train.ets <- elect.train %>% ets(model = "ZZZ") %>% forecast(h = 60)

elect.train.arima %>% autoplot()
elect.train.ets %>% autoplot()

## 4 Comparez l'efficacité prévisionelle des deux approches.
accuracy(elect.train.arima, elect.test)
accuracy(elect.train.ets, elect.test)

## Chapitre 5 - Exercices
##

# 1 Avec la série "électricité" déjà utilisée, tronquez fictivement les 60 dernières 
#   observations et faites des prévisions sur 5 ans avec ets, stl, arima, nnar et tbats, 
#   puis une combinaison de toutes. Tracez-les sur un même graphique.
elect.train <- window(elect_ts, end = c(2013, 12))
elect.test <- window(elect_ts, start = 2014)

h <- length(elect_ts) - length(elect.train)
elect.train.ets <- ets(elect.train, model = "ZZZ") %>% forecast(h = h)
elect.train.stl <- stlf(elect.train, lambda=0, h=h, biasadj=TRUE)
elect.train.arima <- auto.arima(elect.train, lambda=0, biasadj=TRUE) %>% forecast(h = h)
elect.train.nnar <- nnetar(elect.train) %>% forecast(h = h)
elect.train.tbats <- tbats(elect.train, biasadj=TRUE) %>% forecast(h = h)

elect.train.combin <- (elect.train.ets[["mean"]] + 
                  elect.train.stl[["mean"]] +
                  elect.train.arima[["mean"]] + 
                  elect.train.nnar[["mean"]] + 
                  elect.train.tbats[["mean"]])/5

autoplot(elect.train) +
  autolayer(elect.test, series="Réalisé", PI=FALSE) +
  autolayer(elect.train.ets, series="ETS", PI=FALSE) +
  autolayer(elect.train.stl, series="STL", PI=FALSE) +
  autolayer(elect.train.arima, series="ARIMA", PI=FALSE) +
  autolayer(elect.train.nnar, series="NNAR", PI=FALSE) +
  autolayer(elect.train.tbats, series="TBATS", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Prévisions pour la production d'electricite")

## 2 Faites un zoom du graphe précédent pour mieux distinguer les diverses prévisions.
autoplot(window(elect.train, start = 2008)) +
  autolayer(elect.test, series="Réalisé") +
  autolayer(elect.train.ets, series="ETS", PI=FALSE) +
  autolayer(elect.train.stl, series="STL", PI=FALSE) +
  autolayer(elect.train.arima, series="ARIMA", PI=FALSE) +
  autolayer(elect.train.nnar, series="NNAR", PI=FALSE) +
  autolayer(elect.train.tbats, series="TBATS", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Prévisions pour la production d'electricite")


## 3 Calculez quelques mesures d'erreurs de prévisions avec les différentes approches. 
##   Qu'en concluez-vous ?
c(ETS = accuracy(elect.train.ets, elect.test)["Test set","RMSE"],
  STL = accuracy(elect.train.stl, elect.test)["Test set","RMSE"],
  ARIMA = accuracy(elect.train.arima, elect.test)["Test set","RMSE"],
  NNAR = accuracy(elect.train.nnar, elect.test)["Test set","RMSE"],
  TBATS = accuracy(elect.train.tbats, elect.test)["Test set","RMSE"],
  COMBIN = accuracy(elect.train.combin, elect.test)["Test set","RMSE"])

autoplot(window(elect.train, start = 2008)) +
  autolayer(elect.test, series="Réalisé", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Prévisions pour la production d'electricite")

