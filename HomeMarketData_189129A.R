homeMarketValue = read.csv("Home Market Value.csv", header = TRUE)
homeMarketValue
summary(homeMarketValue)

#clean the data set
homeMarketValue$Market.Value = as.numeric(gsub('[$,]', '', homeMarketValue$Market.Value))
homeMarketValue$Square.Feet = as.numeric(gsub('[,]', '', homeMarketValue$Square.Feet))

homeMarketValue
summary(homeMarketValue)
plot(homeMarketValue,col="red", cex.main=0.75)

#Min
min(homeMarketValue$House.Age)
min(homeMarketValue$Square.Feet)
min(homeMarketValue$Market.Value)

#Max
max(homeMarketValue$House.Age)
max(homeMarketValue$Square.Feet)
max(homeMarketValue$Market.Value)

#Mean
mean(homeMarketValue$House.Age)
mean(homeMarketValue$Square.Feet)
mean(homeMarketValue$Market.Value)

#standard deviation 
sd(homeMarketValue$House.Age)
sd(homeMarketValue$Square.Feet)
sd(homeMarketValue$Market.Value)

#Mode
mode(homeMarketValue$House.Age);
mymode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}
mymode(homeMarketValue$House.Age)
mymode(homeMarketValue$Square.Feet)
mymode(homeMarketValue$Market.Value)

marketValueRange = max(homeMarketValue$Market.Value) - min(homeMarketValue$Market.Value)
marketValueRange
IQR(homeMarketValue$Market.Value)

par(mfrow=c(1,3))
hist(homeMarketValue$Market.Value, col="green",cex.main = 0.75,main="Histogram of Market Value")
hist(homeMarketValue$Square.Feet,col="red", cex.main = 0.75, main="Histogram of Square Feet")
hist(homeMarketValue$House.Age, col="pink", cex.main = 0.75, , main="Histogram of Age of House")

#CDF - Commutative Distribution Function
mv = ecdf(homeMarketValue$Market.Value)
sq = ecdf(homeMarketValue$Square.Feet)
age = ecdf(homeMarketValue$House.Age)
plot(mv, cex.main=0.75, col="dark green", main="CDF of Market Value")
plot(sq, cex.main=0.75,col="red", main="CDF of Square Feet")
plot(age, cex.main=0.75, col="blue", main="CDF of Age of House")

ageu = mean(homeMarketValue$House.Age)
ages = sd(homeMarketValue$House.Age)
ages/ageu

marketValueU = mean(homeMarketValue$Market.Value)
marketValueS = sd(homeMarketValue$Market.Value)
marketValueS/marketValueU

quantile(homeMarketValue$Market.Value, probs = c(0,0.25,0.5,0.75,1))

par(mfrow=c(1,2))
mv_d = density(homeMarketValue$Market.Value)
mv_d
plot(homeMarketValue$Market.Value, col="red", cex=0.75, main = "Market Value")
plot(mv_d, col="blue", cex.main=0.75, main = "Density of Market Value")

sq_d = density(homeMarketValue$Square.Feet)
d
plot(homeMarketValue$Square.Feet, col="red", cex=0.75, main = "Square Feet")
plot(sq_d,col="purple", cex.main=0.75, main = "Density of Square Feet")

boxplot(homeMarketValue$Market.Value)
boxplot(homeMarketValue$House.Age)
boxplot(homeMarketValue$Square.Feet)

boxplot(Market.Value~House.Age, col="orange", data = homeMarketValue, main="Market Value VS Age ", xlab="Age", ylab="Market Value")
boxplot(Market.Value~Square.Feet, col="red", data = homeMarketValue, main="Market Value VS Square feet ", xlab="Square Feet", ylab="Market Value")

aggregate(Market.Value ~ House.Age,homeMarketValue, mean )
aggregate(Market.Value ~ House.Age,homeMarketValue, sd )

install.packages("dplyr", dep=TRUE)
library(plyr)
library(dplyr)
ddply(homeMarketValue,~House.Age,summarise,mean=mean(Market.Value), sd=sd(Market.Value))

install.packages("e1071")
library(e1071)
skewness(homeMarketValue$Market.Value)
kurtosis(homeMarketValue$Market.Value)
skewness(homeMarketValue$House.Age)
kurtosis(homeMarketValue$House.Age)


#Measures of Association
cov(homeMarketValue$House.Age, homeMarketValue$Market.Value)
cov(homeMarketValue$Square.Feet, homeMarketValue$Market.Value)

par(mfrow=c(1,2))
plot(homeMarketValue$House.Age, homeMarketValue$Market.Value , col="Red", main="Age VS Market Value", xlab="Age", ylab="Market Value")
plot(homeMarketValue$Square.Feet, homeMarketValue$Market.Value , col="MAROON", main="Square Feet VS Market Value", xlab="Square Feet", ylab="Market Value")

cor(homeMarketValue$House.Age, homeMarketValue$Market.Value)
cor(homeMarketValue$Square.Feet, homeMarketValue$Market.Value)

cor(homeMarketValue[,1:3])


#Dimension Reduction
homeMarketDataPCA = prcomp(homeMarketValue[1:3], center = TRUE, scale=TRUE)
summary(homeMarketDataPCA)
screeplot(homeMarketDataPCA, type="lines", cex.main =0.75, col="MAROON")

#Regression
install.packages("FNN")
library(FNN)

head(homeMarketValue)
cor(homeMarketValue)

lm.homeMarketValue = lm(homeMarketValue$Market.Value ~ homeMarketValue$House.Age + homeMarketValue$Square.Feet)
summary(lm.homeMarketValue)

newAge = c(26, 28, 29, 30, 31)
newSquareFeet = c(1650, 1500, 1800, 2200, 2400)
newData = data.frame(House.Age = newAge, Square.Feet = newSquareFeet)
marketvalue.predicted = predict(lm.homeMarketValue, newData, level=0.95, interval = "confidence")

z = homeMarketValue$Market.Value
y = homeMarketValue$House.Age
x = homeMarketValue$Square.Feet
lm.homeMarketValue = lm(z~y+x)
newData = data.frame(x=newSquareFeet, y=newAge)
predictMarketValue = predict(lm.homeMarketValue, newData, level=0.95, interval = "confidence")
predictMarketValue

