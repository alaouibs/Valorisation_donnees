setwd("/Users/salah-eddinealaoui/Documents/Etudes/Polytech/Valorisation_donnees/Projet_1/Valorisation_donnees")
data <- read.csv("./data/CryptocoinsHistoricalPrices.csv")

#structure of the data
str(data) 
sapply(data , class)
head(data)
#summary of the data
summary(data)

#dimensionality of the data
dim(data)

#names of the variables
names(data)


# Number of variables that contain missing values.
count <- 0
for (i in 1:ncol(data)){
  if( match(TRUE,is.na(data[i]),nomatch = FALSE)){
    count <- count+1
  }
}
print(count)

# Name of the variables that contain missing values.
name <- c()
for (i in 1:length(data)){
  if (match(TRUE,is.na(data[i]),nomatch = FALSE)){
    name<-append(name,names(data[i]))
  }
}
print(name)


###
mean(data$High, na.rm = TRUE)



data[, 7]  <- as.numeric(data[, 7])
data[, 8]  <- as.numeric(data[, 8])
newdata <- na.omit(data)

names=unique(newdata$coin)
number_coins=length(names)

hist(newdata$High[complete.cases(newdata)][newdata$coin == "BTC"], breaks = 80, xlim = c(0, max(newdata$High[newdata$coin == "BTC"])))
hist(newdata$High[complete.cases(newdata)][newdata$coin == "PPT"], breaks = 100, xlim = c(0, max(newdata$High[newdata$coin == "PPT"])))
hist(newdata$High[complete.cases(newdata)][newdata$coin == "STRAT"], breaks = 100, xlim = c(0, max(newdata$High[newdata$coin == "STRAT"])))


coord=unique(newdata[,c("coin","Open","Close")]) #coordinates for each pop
colPalette=rep(c("black","red","cyan","orange","brown","blue","pink","purple","darkgreen"),3)
pch=rep(c(16,15,25),each=9)
plot(coord[,c("Open","Close")],pch=pch,col=colPalette,asp=1)
legend("bottomleft",legend=names,col=colPalette,lty=-1,pch=pch,cex=.75,ncol=2,lwd=2)


t <- table(newdata$coin)
pie(table(newdata$coin[length(newdata$coin) > 1000]))



fn=ecdf(newdata$High[newdata$coin == "STRAT"])
fn2=ecdf(newdata$Close[newdata$coin == "STRAT"])
t=seq(0,8,0.08)
t2=seq(0,5,0.05)
fnt=fn(t)
fnt2=fn2(t2)
par(mfrow=c(1,2))
plot(t,fnt,type = "l")
plot(t2,fnt2,type = "l")

par(mfrow=c(1,3))
boxplot(newdata$High[newdata$coin == "STRAT"])
boxplot(newdata$Low[newdata$coin == "STRAT"])
boxplot(newdata$Open[newdata$coin == "STRAT"])

data(iris)
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
pairs(newdata[3:6][newdata$coin == "STRAT"],pch = 21, bg = c("red", "green3", "blue")[unclass(data$coin)],upper.panel=panel.pearson)
