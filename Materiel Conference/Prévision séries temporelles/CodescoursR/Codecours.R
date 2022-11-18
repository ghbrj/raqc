pckgs<-c("tseries","fpp2","xts","seasonal","astsa","caschrono","ggplot2",
         "TSA","forecast","timeSeries","dynlm","fma","nlme","expsmooth",
         "fpp","urca","caret","KFAS","fma")
install.packages(pckgs)

library(forecast);library(datasets);library(fpp2);library(ggplot2);library(astsa)
library(urca);library(tseries)


####################
#                  #
#    Chapitre 1    #
#                  #
####################

autoplot(h02)+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")

diab1 <- window(h02, start=2000)
fit <- ets(diab1)

fit %>% forecast(h=24) %>%
  autoplot() +
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques et prévisions sur deux ans")

# Faire des séries temporelles
(mydata<-c(1,2,3,2,1))  # données
(mydata<-as.ts(mydata))  # on en fait une série temporelle

# Faire une série temporelle annuelle qui débute en 2010 
(mydata<-ts(mydata,start=2010))

# Faire une série temporelle trimestielle débutant en 2010-3
(mydata<-ts(mydata,start=c(2010,3),frequency=4))

# Faire une série temporelle mensuelle débutant en 2010-9 
(mydata<-ts(mydata,start=c(2010,9),frequency=12))

# Série exemple du cours
(x<-ts(c(123,45,49,107,153),start=2014))

# Exemple - ne pas oublier de changer de répertoire
(airline<- read.table("./donnees/airline49.dat"))
(airline<-ts(airline,start=c(1949,1),frequency=12))
autoplot(airline)+
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")

# Série airline et tracé
plot.ts(airline,main="Nombre mensuel de passagers sur les vols
aériens internationaux",xlab="Temps",ylab="Nombre de passagers
(en milliers)",col="blue")

# Exemple de série avec cycle
autoplot(lynx)+
  ggtitle("Nombre annuel de prises de lynx au Canada")+
  xlab("Années")+
  ylab("Nombre de lynx")

# Décomposition de la série "airline"
airline %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Temps") +
  ggtitle("Décomposition multiplicative de la série airline")

# Graphe de saison 
ggseasonplot(airline, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Nombre de passagers par milliers") + xlab("Mois")+
  ggtitle("Graphe de saison : série airline")

# Graphe polaire de saison
ggseasonplot(airline, polar=TRUE) +
  ylab("Nombre de passagers en milliers") + xlab("Mois")+
  ggtitle("Graphe polaire de saison: série airline")

# Sous-série saisonnière 
ggsubseriesplot(airline) +
  ylab("Nombre de passagers en milliers") + xlab("Mois")+
  ggtitle("sous séries saisonnières: airline")


# Bruit blanc généré + Autocorrélation
set.seed(3)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("Bruit blanc")+xlab("Temps")
ggAcf(y)

#Test de Ljung et Box
Box.test(y,lag=24,fitdf=0,type="Lj")

# Porcs abattus + Autocorrélation
cochon<-window(pigs,start=1990)
autoplot(cochon/1000)+
  ggtitle("Série mensuelle du nombre de porcs abattus dans une province")+
  xlab("Temps (Années)")+
  ylab("Nombre de porcs (par milliers)")
ggAcf(cochon)


#Test de Ljung et Box
Box.test(cochon,lag=24,fitdf=0,type="Lj")

# Série Bière + Autocorrélation
biere<-window(ausbeer,start=1992)
autoplot(biere)+
  ggtitle("Production trimestrielle de bière")+
  xlab("Temps (Années)")+
  ylab("Nombre de mégalitres de bière")
ggAcf(biere)

# Autocorrélation de la série Airline
ggAcf(airline,lag=48)


####################
#                  #
#    Chapitre 2    #
#                  #
####################



# Exemple de la série trimestrielle bière
biere2 <- window(biere,start=1992,end=c(2007,4))
# Graphe pour la série biere
autoplot(biere2) +
  autolayer(meanf(biere2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(biere2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(biere2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Prévisions pour la série trimestrielle de bière") +
  xlab("Années") + ylab("Mégalitres") +
  guides(colour=guide_legend(title="Prévisions"))


goog150 <- window(goog,start=850,end=1000)
autoplot(goog150) +
  autolayer(meanf(goog150, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog150, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog150, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Cotation journalière de Google (fin :  6 déc. 2013)") +
  xlab("Jour") + ylab("Prix à la clôture (US$)") +
  guides(colour=guide_legend(title="Prévisions"))

# Transformations de Box et Cox sur la série airline
(lambda <- BoxCox.lambda(airline))
autoplot(BoxCox(airline,lambda))+ ylab("Nombre transformé de passagers en milliers") + xlab("Mois")+
  ggtitle("Série transformée par Box et Cox : airline")

fit<-snaive(airline,lambda=-0.3)
autoplot(fit)+ ylab("Nombre de passagers en milliers") +
  xlab("Mois")+
  ggtitle("Prévisions de la série airline par la méthode saisonnière naïve")

# Série goog150 et résidus
autoplot(goog150) +
  xlab("Jour") + ylab("Prix de l'action à la clôture (US$)") +
  ggtitle("Action Google (fin le  6 déc. 2013)")

# Série goog150 et valeurs ajustées
fits <- fitted(naive(goog150))
autoplot(goog150, series="Données") +
  autolayer(fits, series="Ajustées") +
  xlab("Jour") + ylab("Prix de l'action à la clôture (en dollars US)") +
  ggtitle("Action Google (fin le 6 déc. 2013)")

# Série goog150 et résidus
res <- residuals(naive(goog150))
autoplot(res) + xlab("Jour") + ylab("") +
  ggtitle("Résidus obtenus par la méthode naïve")

checkresiduals(naive(goog150)) 
 
# Série bière et mesures de précision
biere2 <- window(biere,start=1992,end=c(2007,4))
bierefit1 <- meanf(biere2,h=10)
bierefit2 <- rwf(biere2,h=10)
bierefit3 <- snaive(biere2,h=10)
autoplot(window(biere, start=1992)) +
  autolayer(bierefit1, series="Moyenne", PI=FALSE) +
  autolayer(bierefit2, series="Naïve", PI=FALSE) +
  autolayer(bierefit3, series="Naïve saison.", PI=FALSE) +
  xlab("Années") + ylab("Mégalitres") +
  ggtitle("Prévisions pour la production trimestrielle de bière") +
  guides(colour=guide_legend(title="Prévisions"))

biere3 <- window(biere, start=2008)
accuracy(bierefit1, biere3)
accuracy(bierefit2, biere3)
accuracy(bierefit3, biere3)

# Intervalles de prévision
res_sd <- sqrt(mean(res^2, na.rm=TRUE))
# c(tail(goog150,1)) + 1.96 * res_sd * c(-1,1)
naive(goog150, level=95)
autoplot(naive(goog150)) + xlab("Jour") +
  ylab("Prix de l'action à la clôture (US$)") +
  ggtitle("Action Google avec prévisions et intervalles de prévision  par la méthode naïve")



####################
#                  #
#    Chapitre 3    #
#                  #
####################


# Série écarts de température
temp<-ts(globtemp,start=1884)
head(temp)
autoplot(temp)+
  ggtitle("Ecarts globaux annuels de température")+
  xlab("Années")+
  ylab("Ecarts de température")


# Série écarts de température et lissage exponentiel simple  
temp1<-ses(temp,h=10)  
autoplot(temp)+
  autolayer(temp1,PI=FALSE)+
  autolayer(fitted(temp1))+
  ggtitle("Prévisions des écarts de température (lissage exp.simple)")+
  ylab("Ecarts de température") + xlab("Années")


# Série des écarts de température et lissage de Holt
temp2 <- holt(temp, h=15)
temp3<-holt(temp,damped=TRUE,phi=0.97,h=15)
autoplot(temp)+
  autolayer(fitted(temp2))+
  autolayer(temp2, series="Méthode de Holt", PI=FALSE) +
  autolayer(temp3, series="Méthode de Holt amortie", PI=FALSE) +
  ggtitle("Prévisions de par la méthode de Holt") + xlab("Années") +
  ylab("Ecarts de température") +
  guides(colour=guide_legend(title="Prévisions"))


# Série champagne et lissage exponentiel de Holt-Winters saisonnier additif
champ<-scan("champ.dat")
champ<-ts(champ,start=c(1962,1),frequency=12)
champ1<- hw(champ,seasonal="additive",h=12)
autoplot(champ) +
  autolayer(champ1, series="Holt-Winters additif", PI=FALSE) +
  xlab("Années") +
  ylab("Ventes de champagne") +
  ggtitle("Prévisions des ventes de champagne par Holt-Winters") +
  guides(colour=guide_legend(title="Prévisions"))


# Série Airline et lissage exponentiel de Holt-Winters saisonnier multiplicatif
airlinemult<-hw(airline,seasonal="multiplicative",h=12)
summary(airlinemult)  
autoplot(airline)+
  autolayer(airlinemult, series="Holt-Winters multiplicatif", PI=FALSE) +
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux et prévisions")+
  xlab("Années")+
  ylab("Nombre de passagers (en milliers)")


#  Commande "ETS" sur la série "diabète"
diab <- window(h02, start=2000)
autoplot(diab)+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")

ets(diab,model="AAA")%>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")
fit1diab<-ets(diab,model="AAA")
summary(fit1diab)


#  Commande "ETS" automatisée sur la série "diabète"
ets(diab,model="ZZZ")%>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")
fit2diab<-ets(diab,model="ZZZ")
summary(fit2diab)


# Mesures de comparaison des deux approches précédentes
diab%>%ets(model="AAA")%>%accuracy()
diab%>%ets()%>%accuracy()

# Etude des résidus du modèle précédent
diab2<-ets(diab,model="ZZZ")
cbind('Residus' = residuals(diab2),
      'Erreurs de prévision' = residuals(diab2,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Années") + ylab("")

checkresiduals(fit2diab)
shapiro.test(residuals(fit2diab))
Box.test(residuals(fit2diab),lag=24,fitdf=0,type="Lj")



####################
#                  #
#    Chapitre 4    #
#                  #
####################


# Série "google" non stationnaire
goog150 <- window(goog,start=850,end=1000)
autoplot(goog150)+
  xlab("Jour") + ylab("Prix de l'action à la clôture (en dollars US)") +
  ggtitle("Action Google (fin le 6 déc. 2013)")


# Série médicaments antidiabétiques
autoplot(h02)+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")


# Bruit blanc généré
set.seed(3)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("Bruit blanc")+xlab("Temps")


# Série "google" différenciée
dgoog<-diff(goog150)
autoplot(dgoog)+
  xlab("Jour") + ylab("Ecarts des prix de l'action à la clôture (en dollars US)") +
  ggtitle("Action Google différenciée (fin le 6 déc. 2013)")

ggAcf(goog150,lag=20)
ggAcf(dgoog,lag=20)


# Série "médicaments antidiabétiques"
cbind("Nb. mens. d'antidiabétiques" = h02,
      "log du Nb. mens. d'antidiabétiques" = log(h02),
      "Chang. ann. du log du Nb.d'antidiab." = diff(log(h02),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Années") + ylab("") +
  ggtitle("Ventes mensuelles de médicaments antidiabétiques")


# Tests ADF et KPSS 
adf.test(goog150)
summary(ur.kpss(goog150))



# Série prix journalier de l'or - Valeurs manquantes
gold1 <- na.interp(gold)
autoplot(gold1, series="Interpolated") +
  autolayer(gold, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="blue"))+
  xlab("Jour") + ylab("Prix journalier de l'or (en dollars US)") +
  ggtitle("Prix journalier de l'or (1 janv. 1985 à 1 mars 1989")



# Série prix journalier de l'or - Valeurs aberrantes
tsoutliers(gold)  # pour repérer les valeurs aberrantes
gold[768:772] # pour savoir quelle était la valeur aberrante


# Série prix journalier de l'or - nettoyage des valeurs aberrantes et manquantes
gold %>% tsclean()%>% autoplot()


# Série commerce de détail trimestriel
autoplot(euretail) + ylab("Index commercial") + xlab("Années")+
  ggtitle("Commerce de détail trimestriel dans la zone euro")

euretail %>% auto.arima() %>% forecast(h=12) %>% autoplot() +
  ylab("Index commercial") + xlab("Années")+
  ggtitle("Commerce de détail trimestriel dans la zone euro et prévisions")



# Série médicaments antidiabétiques
autoplot(h02)+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")

h02 %>% auto.arima() 
h02 %>% auto.arima() %>% forecast(h=24) %>% autoplot()+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques et prévisions")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")

fit.arima<-auto.arima(h02)
checkresiduals(fit.arima) # analyse des résidus

train <- window(h02,end=c(2005,12)) # mesures d'erreur
(fit.arima<-auto.arima(train)) 
diab1<-fit.arima %>% forecast(h=24)%>%
  accuracy(h02)
diab1[,c("RMSE","MAPE","MASE")]

h<-length(h02)-length(train)
ETS<- forecast(ets(train),h=h)
ARIMA<-forecast(auto.arima(train),h=h)
autoplot(h02) +
  autolayer(ARIMA,series="ARIMA",PI=FALSE)+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")


airline %>% auto.arima(lambda="auto") %>% forecast(h=24)%>%
  autoplot()+
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux et prévisions")+
  xlab("Temps")+ylab("Nombre de passagers (en milliers)")


fit1.arima<-auto.arima(airline,lambda="auto")
summary(fit1.arima)
checkresiduals(fit1.arima)


####################
#                  #
#    Chapitre 5    #
#                  #
####################

# Série médicaments antidiabétiques et TBATS
h02.fit<-tbats(h02)
autoplot(forecast(h02.fit))+
  ggtitle("Nombre mensuel total de médicaments antidiabétiques et prévisions par TBATS")+
  xlab("Temps")+
  ylab("Nombre de médicaments en millions")



# Série Airline
(airline<- read.table("./donnees/airline49.dat"))
(airline<-ts(airline,start=c(1949,1),frequency=12))
autoplot(airline)
airline.tbats<-tbats(airline)
autoplot(forecast(airline.tbats))+
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux et prévisions TBATS")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")


autoplot(airline)+
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")

airline %>% auto.arima() %>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux et prévisions")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")
  
fit1.arima<-auto.arima(airline,lambda=lambda)
checkresiduals(fit1.arima) # analyse des résidus


# Série airline et STL
fit.airline<-stlf(airline,lambda=0)
autoplot(fit.airline) +
  ggtitle("Nombre mensuel de passagers sur les vols aériens internationaux et prévisions STL")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")



#Série taches solaires et réseaux de neurones
fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))+
  xlab("Années")+
  ylab("Taches solaires")+
  ggtitle("Prévisions de la série taches solaires avec un NNAR(10,6)")

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)+
  xlab("Années")+
  ylab("Taches solaires")+
  ggtitle("Prévisions de la série taches solaires avec un NNAR(10,6) et intervalles de prévision")


# Série production de bière
train <- window(ausbeer, end=c(2002,12))
h <- length(ausbeer) - length(train)
ETS <- forecast(ets(train), h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),h=h)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(ausbeer) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combinaison") +
  xlab("Années") + ylab("Mégalitres") +
  ggtitle("Prévisions pour la production trimestrielle de bière")

auscut<-window(ausbeer,start=c(1995,4))
autoplot(auscut)
autoplot(auscut) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combinaison") +
  xlab("Années") + ylab("Mégalitres") +
  ggtitle("Prévisions pour la production trimestrielle de bière")



c(ETS = accuracy(ETS, ausbeer)["Test set","RMSE"],
  STL = accuracy(STL, ausbeer)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, ausbeer)["Test set","RMSE"],
  NNAR = accuracy(NNAR, ausbeer)["Test set","RMSE"],
  TBATS = accuracy(TBATS, ausbeer)["Test set","RMSE"],
  Combinaison =
    accuracy(Combination, ausbeer)["Test set","RMSE"])










