setwd("/Users/salah-eddinealaoui/Documents/Etudes/Polytech/Valorisation_donnees/Projet_1/Valorisation_donnees")
data <- read.csv("./data/CryptocoinsHistoricalPrices.csv")
dim(data)
class(data)
summary(data)
head(data)
hist(data$Market.Cap)

names_coins=unique(data$coin)
nbr_coins=length(names)
#1370 type de cryptomonnaies le pie chart est immontrable, y a un moyen de filtrer pour garder que les grosses
# cryptomonnaies stype bitcoin ether ?