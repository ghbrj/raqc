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
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")

diab1 <- window(h02, start=2000)
fit <- ets(diab1)

fit %>% forecast(h=24) %>%
  autoplot() +
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques et pr�visions sur deux ans")

# Faire des s�ries temporelles
(mydata<-c(1,2,3,2,1))  # donn�es
(mydata<-as.ts(mydata))  # on en fait une s�rie temporelle

# Faire une s�rie temporelle annuelle qui d�bute en 2010 
(mydata<-ts(mydata,start=2010))

# Faire une s�rie temporelle trimestielle d�butant en 2010-3
(mydata<-ts(mydata,start=c(2010,3),frequency=4))

# Faire une s�rie temporelle mensuelle d�butant en 2010-9 
(mydata<-ts(mydata,start=c(2010,9),frequency=12))

# S�rie exemple du cours
(x<-ts(c(123,45,49,107,153),start=2014))

# Exemple - ne pas oublier de changer de r�pertoire
(airline<- read.table("./donnees/airline49.dat"))
(airline<-ts(airline,start=c(1949,1),frequency=12))
autoplot(airline)+
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")

# S�rie airline et trac�
plot.ts(airline,main="Nombre mensuel de passagers sur les vols
a�riens internationaux",xlab="Temps",ylab="Nombre de passagers
(en milliers)",col="blue")

# Exemple de s�rie avec cycle
autoplot(lynx)+
  ggtitle("Nombre annuel de prises de lynx au Canada")+
  xlab("Ann�es")+
  ylab("Nombre de lynx")

# D�composition de la s�rie "airline"
airline %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Temps") +
  ggtitle("D�composition multiplicative de la s�rie airline")

# Graphe de saison 
ggseasonplot(airline, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Nombre de passagers par milliers") + xlab("Mois")+
  ggtitle("Graphe de saison : s�rie airline")

# Graphe polaire de saison
ggseasonplot(airline, polar=TRUE) +
  ylab("Nombre de passagers en milliers") + xlab("Mois")+
  ggtitle("Graphe polaire de saison: s�rie airline")

# Sous-s�rie saisonni�re 
ggsubseriesplot(airline) +
  ylab("Nombre de passagers en milliers") + xlab("Mois")+
  ggtitle("sous s�ries saisonni�res: airline")


# Bruit blanc g�n�r� + Autocorr�lation
set.seed(3)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("Bruit blanc")+xlab("Temps")
ggAcf(y)

#Test de Ljung et Box
Box.test(y,lag=24,fitdf=0,type="Lj")

# Porcs abattus + Autocorr�lation
cochon<-window(pigs,start=1990)
autoplot(cochon/1000)+
  ggtitle("S�rie mensuelle du nombre de porcs abattus dans une province")+
  xlab("Temps (Ann�es)")+
  ylab("Nombre de porcs (par milliers)")
ggAcf(cochon)


#Test de Ljung et Box
Box.test(cochon,lag=24,fitdf=0,type="Lj")

# S�rie Bi�re + Autocorr�lation
biere<-window(ausbeer,start=1992)
autoplot(biere)+
  ggtitle("Production trimestrielle de bi�re")+
  xlab("Temps (Ann�es)")+
  ylab("Nombre de m�galitres de bi�re")
ggAcf(biere)

# Autocorr�lation de la s�rie Airline
ggAcf(airline,lag=48)


####################
#                  #
#    Chapitre 2    #
#                  #
####################



# Exemple de la s�rie trimestrielle bi�re
biere2 <- window(biere,start=1992,end=c(2007,4))
# Graphe pour la s�rie biere
autoplot(biere2) +
  autolayer(meanf(biere2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(biere2, h=11),
            series="Na�ve", PI=FALSE) +
  autolayer(snaive(biere2, h=11),
            series="Seasonal na�ve", PI=FALSE) +
  ggtitle("Pr�visions pour la s�rie trimestrielle de bi�re") +
  xlab("Ann�es") + ylab("M�galitres") +
  guides(colour=guide_legend(title="Pr�visions"))


goog150 <- window(goog,start=850,end=1000)
autoplot(goog150) +
  autolayer(meanf(goog150, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog150, h=40),
            series="Na�ve", PI=FALSE) +
  autolayer(rwf(goog150, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Cotation journali�re de Google (fin :  6 d�c. 2013)") +
  xlab("Jour") + ylab("Prix � la cl�ture (US$)") +
  guides(colour=guide_legend(title="Pr�visions"))

# Transformations de Box et Cox sur la s�rie airline
(lambda <- BoxCox.lambda(airline))
autoplot(BoxCox(airline,lambda))+ ylab("Nombre transform� de passagers en milliers") + xlab("Mois")+
  ggtitle("S�rie transform�e par Box et Cox : airline")

fit<-snaive(airline,lambda=-0.3)
autoplot(fit)+ ylab("Nombre de passagers en milliers") +
  xlab("Mois")+
  ggtitle("Pr�visions de la s�rie airline par la m�thode saisonni�re na�ve")

# S�rie goog150 et r�sidus
autoplot(goog150) +
  xlab("Jour") + ylab("Prix de l'action � la cl�ture (US$)") +
  ggtitle("Action Google (fin le  6 d�c. 2013)")

# S�rie goog150 et valeurs ajust�es
fits <- fitted(naive(goog150))
autoplot(goog150, series="Donn�es") +
  autolayer(fits, series="Ajust�es") +
  xlab("Jour") + ylab("Prix de l'action � la cl�ture (en dollars US)") +
  ggtitle("Action Google (fin le 6 d�c. 2013)")

# S�rie goog150 et r�sidus
res <- residuals(naive(goog150))
autoplot(res) + xlab("Jour") + ylab("") +
  ggtitle("R�sidus obtenus par la m�thode na�ve")

checkresiduals(naive(goog150)) 
 
# S�rie bi�re et mesures de pr�cision
biere2 <- window(biere,start=1992,end=c(2007,4))
bierefit1 <- meanf(biere2,h=10)
bierefit2 <- rwf(biere2,h=10)
bierefit3 <- snaive(biere2,h=10)
autoplot(window(biere, start=1992)) +
  autolayer(bierefit1, series="Moyenne", PI=FALSE) +
  autolayer(bierefit2, series="Na�ve", PI=FALSE) +
  autolayer(bierefit3, series="Na�ve saison.", PI=FALSE) +
  xlab("Ann�es") + ylab("M�galitres") +
  ggtitle("Pr�visions pour la production trimestrielle de bi�re") +
  guides(colour=guide_legend(title="Pr�visions"))

biere3 <- window(biere, start=2008)
accuracy(bierefit1, biere3)
accuracy(bierefit2, biere3)
accuracy(bierefit3, biere3)

# Intervalles de pr�vision
res_sd <- sqrt(mean(res^2, na.rm=TRUE))
# c(tail(goog150,1)) + 1.96 * res_sd * c(-1,1)
naive(goog150, level=95)
autoplot(naive(goog150)) + xlab("Jour") +
  ylab("Prix de l'action � la cl�ture (US$)") +
  ggtitle("Action Google avec pr�visions et intervalles de pr�vision  par la m�thode na�ve")



####################
#                  #
#    Chapitre 3    #
#                  #
####################


# S�rie �carts de temp�rature
temp<-ts(globtemp,start=1884)
head(temp)
autoplot(temp)+
  ggtitle("Ecarts globaux annuels de temp�rature")+
  xlab("Ann�es")+
  ylab("Ecarts de temp�rature")


# S�rie �carts de temp�rature et lissage exponentiel simple  
temp1<-ses(temp,h=10)  
autoplot(temp)+
  autolayer(temp1,PI=FALSE)+
  autolayer(fitted(temp1))+
  ggtitle("Pr�visions des �carts de temp�rature (lissage exp.simple)")+
  ylab("Ecarts de temp�rature") + xlab("Ann�es")


# S�rie des �carts de temp�rature et lissage de Holt
temp2 <- holt(temp, h=15)
temp3<-holt(temp,damped=TRUE,phi=0.97,h=15)
autoplot(temp)+
  autolayer(fitted(temp2))+
  autolayer(temp2, series="M�thode de Holt", PI=FALSE) +
  autolayer(temp3, series="M�thode de Holt amortie", PI=FALSE) +
  ggtitle("Pr�visions de par la m�thode de Holt") + xlab("Ann�es") +
  ylab("Ecarts de temp�rature") +
  guides(colour=guide_legend(title="Pr�visions"))


# S�rie champagne et lissage exponentiel de Holt-Winters saisonnier additif
champ<-scan("champ.dat")
champ<-ts(champ,start=c(1962,1),frequency=12)
champ1<- hw(champ,seasonal="additive",h=12)
autoplot(champ) +
  autolayer(champ1, series="Holt-Winters additif", PI=FALSE) +
  xlab("Ann�es") +
  ylab("Ventes de champagne") +
  ggtitle("Pr�visions des ventes de champagne par Holt-Winters") +
  guides(colour=guide_legend(title="Pr�visions"))


# S�rie Airline et lissage exponentiel de Holt-Winters saisonnier multiplicatif
airlinemult<-hw(airline,seasonal="multiplicative",h=12)
summary(airlinemult)  
autoplot(airline)+
  autolayer(airlinemult, series="Holt-Winters multiplicatif", PI=FALSE) +
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux et pr�visions")+
  xlab("Ann�es")+
  ylab("Nombre de passagers (en milliers)")


#  Commande "ETS" sur la s�rie "diab�te"
diab <- window(h02, start=2000)
autoplot(diab)+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")

ets(diab,model="AAA")%>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")
fit1diab<-ets(diab,model="AAA")
summary(fit1diab)


#  Commande "ETS" automatis�e sur la s�rie "diab�te"
ets(diab,model="ZZZ")%>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")
fit2diab<-ets(diab,model="ZZZ")
summary(fit2diab)


# Mesures de comparaison des deux approches pr�c�dentes
diab%>%ets(model="AAA")%>%accuracy()
diab%>%ets()%>%accuracy()

# Etude des r�sidus du mod�le pr�c�dent
diab2<-ets(diab,model="ZZZ")
cbind('Residus' = residuals(diab2),
      'Erreurs de pr�vision' = residuals(diab2,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Ann�es") + ylab("")

checkresiduals(fit2diab)
shapiro.test(residuals(fit2diab))
Box.test(residuals(fit2diab),lag=24,fitdf=0,type="Lj")



####################
#                  #
#    Chapitre 4    #
#                  #
####################


# S�rie "google" non stationnaire
goog150 <- window(goog,start=850,end=1000)
autoplot(goog150)+
  xlab("Jour") + ylab("Prix de l'action � la cl�ture (en dollars US)") +
  ggtitle("Action Google (fin le 6 d�c. 2013)")


# S�rie m�dicaments antidiab�tiques
autoplot(h02)+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")


# Bruit blanc g�n�r�
set.seed(3)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("Bruit blanc")+xlab("Temps")


# S�rie "google" diff�renci�e
dgoog<-diff(goog150)
autoplot(dgoog)+
  xlab("Jour") + ylab("Ecarts des prix de l'action � la cl�ture (en dollars US)") +
  ggtitle("Action Google diff�renci�e (fin le 6 d�c. 2013)")

ggAcf(goog150,lag=20)
ggAcf(dgoog,lag=20)


# S�rie "m�dicaments antidiab�tiques"
cbind("Nb. mens. d'antidiab�tiques" = h02,
      "log du Nb. mens. d'antidiab�tiques" = log(h02),
      "Chang. ann. du log du Nb.d'antidiab." = diff(log(h02),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Ann�es") + ylab("") +
  ggtitle("Ventes mensuelles de m�dicaments antidiab�tiques")


# Tests ADF et KPSS 
adf.test(goog150)
summary(ur.kpss(goog150))



# S�rie prix journalier de l'or - Valeurs manquantes
gold1 <- na.interp(gold)
autoplot(gold1, series="Interpolated") +
  autolayer(gold, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="blue"))+
  xlab("Jour") + ylab("Prix journalier de l'or (en dollars US)") +
  ggtitle("Prix journalier de l'or (1 janv. 1985 � 1 mars 1989")



# S�rie prix journalier de l'or - Valeurs aberrantes
tsoutliers(gold)  # pour rep�rer les valeurs aberrantes
gold[768:772] # pour savoir quelle �tait la valeur aberrante


# S�rie prix journalier de l'or - nettoyage des valeurs aberrantes et manquantes
gold %>% tsclean()%>% autoplot()


# S�rie commerce de d�tail trimestriel
autoplot(euretail) + ylab("Index commercial") + xlab("Ann�es")+
  ggtitle("Commerce de d�tail trimestriel dans la zone euro")

euretail %>% auto.arima() %>% forecast(h=12) %>% autoplot() +
  ylab("Index commercial") + xlab("Ann�es")+
  ggtitle("Commerce de d�tail trimestriel dans la zone euro et pr�visions")



# S�rie m�dicaments antidiab�tiques
autoplot(h02)+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")

h02 %>% auto.arima() 
h02 %>% auto.arima() %>% forecast(h=24) %>% autoplot()+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques et pr�visions")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")

fit.arima<-auto.arima(h02)
checkresiduals(fit.arima) # analyse des r�sidus

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
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")


airline %>% auto.arima(lambda="auto") %>% forecast(h=24)%>%
  autoplot()+
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux et pr�visions")+
  xlab("Temps")+ylab("Nombre de passagers (en milliers)")


fit1.arima<-auto.arima(airline,lambda="auto")
summary(fit1.arima)
checkresiduals(fit1.arima)


####################
#                  #
#    Chapitre 5    #
#                  #
####################

# S�rie m�dicaments antidiab�tiques et TBATS
h02.fit<-tbats(h02)
autoplot(forecast(h02.fit))+
  ggtitle("Nombre mensuel total de m�dicaments antidiab�tiques et pr�visions par TBATS")+
  xlab("Temps")+
  ylab("Nombre de m�dicaments en millions")



# S�rie Airline
(airline<- read.table("./donnees/airline49.dat"))
(airline<-ts(airline,start=c(1949,1),frequency=12))
autoplot(airline)
airline.tbats<-tbats(airline)
autoplot(forecast(airline.tbats))+
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux et pr�visions TBATS")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")


autoplot(airline)+
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")

airline %>% auto.arima() %>% forecast(h=24)%>% autoplot()+
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux et pr�visions")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")
  
fit1.arima<-auto.arima(airline,lambda=lambda)
checkresiduals(fit1.arima) # analyse des r�sidus


# S�rie airline et STL
fit.airline<-stlf(airline,lambda=0)
autoplot(fit.airline) +
  ggtitle("Nombre mensuel de passagers sur les vols a�riens internationaux et pr�visions STL")+
  xlab("Temps")+
  ylab("Nombre de passagers (en milliers)")



#S�rie taches solaires et r�seaux de neurones
fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))+
  xlab("Ann�es")+
  ylab("Taches solaires")+
  ggtitle("Pr�visions de la s�rie taches solaires avec un NNAR(10,6)")

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)+
  xlab("Ann�es")+
  ylab("Taches solaires")+
  ggtitle("Pr�visions de la s�rie taches solaires avec un NNAR(10,6) et intervalles de pr�vision")


# S�rie production de bi�re
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
  xlab("Ann�es") + ylab("M�galitres") +
  ggtitle("Pr�visions pour la production trimestrielle de bi�re")

auscut<-window(ausbeer,start=c(1995,4))
autoplot(auscut)
autoplot(auscut) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combinaison") +
  xlab("Ann�es") + ylab("M�galitres") +
  ggtitle("Pr�visions pour la production trimestrielle de bi�re")



c(ETS = accuracy(ETS, ausbeer)["Test set","RMSE"],
  STL = accuracy(STL, ausbeer)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, ausbeer)["Test set","RMSE"],
  NNAR = accuracy(NNAR, ausbeer)["Test set","RMSE"],
  TBATS = accuracy(TBATS, ausbeer)["Test set","RMSE"],
  Combinaison =
    accuracy(Combination, ausbeer)["Test set","RMSE"])










