### R � QC 2019 - Pr�vision de s�ries temporelles ###
### 2019-05-13
###

library(forecast)
library(ggplot2)

## Chapitre 1 - Exercices
##

## 1 Importez le fichier "tute1.csv". Il s'agit de ventes trimestrielles d'une petite compagnie (variable "Ventes") 
## entre 1981 et 1995. Il y a aussi une variable Budget Publicitaire ("BudgPubl") et le Produit Int�rieur Brut ("PIB").
tute1 <- read.csv("./donnees/tute1.csv", header = TRUE)
str(tute1)
head(tute1)

## 2 Convertissez-les en s�ries temporelles, apr�s avoir �t� la premi�re colonne inutile ici.
tute_ts <- ts(tute1[, -1], start = c(1981), frequency = 4)
summary(tute_ts)


## 3 Tracez les trois s�ries temporelles.
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

## 4 D�composez la s�rie Ventes en tendance, saison et r�sidus. Tracez �galement l'autocorr�logramme.
tute_ts[,"Ventes"]  %>%
  decompose(type="multiplicative") %>%
  autoplot() + xlab("Temps") +
  ggtitle("D�composition multiplicative de la s�rie Ventes")

ggAcf(tute_ts[,"Ventes"])

## Chapitre 2 - Exercices
##

## 1 Importez la s�rie mensuelle "lait.dat" d�marrant en janvier 2005. Tracez-l�.
lait <- read.table("./donnees/lait.dat", header = TRUE)
summary(lait)

lait_ts <- ts(lait, start = c(2005, 1), frequency = 12)

lait_ts %>% autoplot()

## 2 S�parez les donn�es en deux parties ( la premi�re "lait.train" allant de janvier 2005 � d�cembre 2017, 
##   et l'autre "lait.test", d�marrant en janvier 2018). Tracez-les.
lait.train <- window(lait_ts, start = c(2005, 1), end = c(2017, 12))
lait.test <- window(lait_ts, start = c(2018, 1))

autoplot(lait.train) +
  autolayer(lait.train, series="Train") + 
  autolayer(lait.test, series="Test")

## 3 Calculez les pr�visions saisonni�res na�ves sur 12 mois � partir de la s�rie "lait.train". Faites un graphique.
lait.snaive <- snaive(lait.train, h = 12)

autoplot(lait.train) +
  autolayer(lait.test, series="R�alis�") +
  autolayer(lait.snaive, series="Pr�vision", PI=FALSE)

## 4 Comparez la pertinence de vos pr�visions vis-�-vis du r�alis� a posteriori.
accuracy(lait.snaive, lait.test)

## 5 Examinez les r�sidus. Commentez.
checkresiduals(lait.snaive)

## Chapitre 3 - Exercices
##

## Reprendre la s�rie "lait" de l'exercice du chapitre 2.
## 1 Utilisez la commande "ets" pour pr�voir � horizon 12 la s�rie "lait". Faites un graphique.
lait.ets <- ets(lait_ts, model = "ZZZ")
autoplot(lait.ets)

lait.ets %>% forecast(h = 12) %>% autoplot()

## 2 Reprendre la s�rie "lait.train" pour faire des pr�visions sur une ann�e via l'approche ets. Faites un 
##   graphique illustratif pour comparer les pr�visions et le r�alis� a posteriori.
lait.train.ets <- ets(lait.train, model = "ZZZ")
lait.train.ets %>% forecast(h = 12) %>% autoplot()
lait.train.ets <- forecast(lait.train.ets, h = 12)
autoplot(lait_ts) +
  autolayer(lait.train.ets, series = "Pr�vision") +
  autolayer(lait.test, series = "Test")

## 3 Comparez les pertinences des pr�visions faites via "sna�ve" et via "ets".
accuracy(lait.snaive, lait.test)
accuracy(lait.train.ets, lait.test)

## Chapitre 4 - Exercices
##

## 1 Importez la s�rie "electricity.dat", s�rie mensuelle d�butant en janvier 1986. 
##   Faites-en un graphique.
elect <- read.table("./donnees/electricity.dat", header = TRUE)
summary(elect)

elect_ts <- ts(elect, start = c(1986, 1), frequency = 12)
elect_ts %>% autoplot() + ggtitle("S�rie Electricity")

## 2 En utilisant un mod�le SARIMA, faites des pr�visions sur 2 ans.
elect.arima <- elect_ts %>% auto.arima() %>% forecast(h = 24)
summary(elect.arima)
elect.arima %>% autoplot()

## 3 Tronquez fictivement les 60 derni�res observations et faites des pr�visions via 
##   un mod�le SARIMA et une approche "ets".
elect.train <- window(elect_ts, end = c(2013, 12))
elect.test<- window(elect_ts, start = 2014)

elect.train.arima <- elect.train %>% auto.arima() %>% forecast(h = 60)
elect.train.ets <- elect.train %>% ets(model = "ZZZ") %>% forecast(h = 60)

elect.train.arima %>% autoplot()
elect.train.ets %>% autoplot()

## 4 Comparez l'efficacit� pr�visionelle des deux approches.
accuracy(elect.train.arima, elect.test)
accuracy(elect.train.ets, elect.test)

## Chapitre 5 - Exercices
##

# 1 Avec la s�rie "�lectricit�" d�j� utilis�e, tronquez fictivement les 60 derni�res 
#   observations et faites des pr�visions sur 5 ans avec ets, stl, arima, nnar et tbats, 
#   puis une combinaison de toutes. Tracez-les sur un m�me graphique.
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
  autolayer(elect.test, series="R�alis�", PI=FALSE) +
  autolayer(elect.train.ets, series="ETS", PI=FALSE) +
  autolayer(elect.train.stl, series="STL", PI=FALSE) +
  autolayer(elect.train.arima, series="ARIMA", PI=FALSE) +
  autolayer(elect.train.nnar, series="NNAR", PI=FALSE) +
  autolayer(elect.train.tbats, series="TBATS", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Pr�visions pour la production d'electricite")

## 2 Faites un zoom du graphe pr�c�dent pour mieux distinguer les diverses pr�visions.
autoplot(window(elect.train, start = 2008)) +
  autolayer(elect.test, series="R�alis�") +
  autolayer(elect.train.ets, series="ETS", PI=FALSE) +
  autolayer(elect.train.stl, series="STL", PI=FALSE) +
  autolayer(elect.train.arima, series="ARIMA", PI=FALSE) +
  autolayer(elect.train.nnar, series="NNAR", PI=FALSE) +
  autolayer(elect.train.tbats, series="TBATS", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Pr�visions pour la production d'electricite")


## 3 Calculez quelques mesures d'erreurs de pr�visions avec les diff�rentes approches. 
##   Qu'en concluez-vous ?
c(ETS = accuracy(elect.train.ets, elect.test)["Test set","RMSE"],
  STL = accuracy(elect.train.stl, elect.test)["Test set","RMSE"],
  ARIMA = accuracy(elect.train.arima, elect.test)["Test set","RMSE"],
  NNAR = accuracy(elect.train.nnar, elect.test)["Test set","RMSE"],
  TBATS = accuracy(elect.train.tbats, elect.test)["Test set","RMSE"],
  COMBIN = accuracy(elect.train.combin, elect.test)["Test set","RMSE"])

autoplot(window(elect.train, start = 2008)) +
  autolayer(elect.test, series="R�alis�", PI=FALSE) +
  autolayer(elect.train.combin, series="Combinaison") +
  xlab("Temps") + ylab("Electricity") +
  ggtitle("Pr�visions pour la production d'electricite")

