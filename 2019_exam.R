library(openxlsx)
data = read.xlsx(xlsxFile = './STWI_Exam_HS19.xlsx')#add file here or rename your exam file to Data.xlsx and copy it into this folder
attach(data) 
# => this sign is for copy pase : ~
# use percent => mean(stockData$`%.Fall`)
#a) height = continous
#b) mother = continous
#c) maths = continous
#d) french = discret
#e) siblings = discret
#f) present = continous
# plz = discret nominal =< price metrisch continous, points => ordinal, continous 
library(MASS) #for fractions(amountOfSeven/(6*6)) => zeigt brüchte an
library(Hmisc) 
describe(c(0,1),type=2)
describe(c(0,1))
fractions(1/(6*6))
x = 1/33
fractions(x)
x=0
curve(dnorm(x),xlim=c(-5,5))
curve(dnorm(x,15,6),xlim=c(-9,40), from = -60,to=120)



#Here we go
# Which province in America produces the most wine titles as given in Wines and how many titles does it produce?
#1b)
typeof(Price)
typeof(Points)
typeof(Year)

us =Province[Country=="US"]
max(table(Province[Country=="US"]))
table(Province[Country=="US"])

# We consider the points awarded for each of the wines in Wines. Which has
# the least spread of points? Include in your answer and appropriate diagram that shows how you make the comparisation.
#1 c)
boxplot(Points~Country)

#2
# A particular restaurant sells either red or white wine. According to their sales 70% are red. 4% of the 
# bottles are corked. Over the years, the restaurant estimate 80% of the time a wine was corked, then it was red.

# One evening the restaurant sells 10 bottles of wine. What is the probability 
# that at least one of the ten bottles of wine is in fact corked?
#a)
1-(1-0.04)^10

# At one table a customer orders a bottle of white wine.
# What is the possibility that this one is corked?
#b)
#TODO =>  fragen
red = 0.7 * (0.04 * 0.8)

# The restaurant has made the following order fro wine from the province texas.
# 
# Title                    Bottles Bought        Price
# Bending Banch 2012       300                   50
# Becker 2012              150                   25
# Los Pinos Ranch 2015     225                   23

# What is the average cost of a bottle wine from this order?
#c)
totalCosts = 300*50 + 150* 25 + 225 *23 
menge = 300 + 150 + 225 
totalCosts/menge



#3
# Explain whether or not therer is an association between the
# points awarded to a wine and the proce of the wine. Include in your answer 
# the diagram and numerical meassures.
#a)
plot(Points~Price)
cor(Price,Points)

# If consider the points the wines are awarded in Wines, find the proportion of the wines
# that fall within one standard deviation of the mane.
#b)
mean = mean(Points)
sd = sd(Points)
diff = mean - sd
sum = mean + sd
a = length(Points[diff <Points & Points < sum])
b = length(Points)

desiredProperties = a/b

# The points awarded to the wines appear to follow a normal distribution which for this questiton will
# assumed has a mean of 90 points. and a standard deviation of 3. If a simple random sample of 20 wines
# were selected, then what is the probability that the mean points awarded for the wines in the sample is greater
# than 91 points.
# c)
1-pnorm(91,m=90, s=3/sqrt(20))

# 4
# Calculate and interpret the skewness of price based on the wines in Wines.
#a)
Price.z = (Price-mean(Price))/sd(Price)
lengthPrice = length(Price)
skewnessHeight = lengthPrice/((lengthPrice-1)*(lengthPrice-2))*sum(Price.z^3)

#This is a large and poitive right skewness. Therer are some very expensive wines grater than the majorit of the data set.

# It is claimed that French wines are more expensive than
# American wines. Create an appropriate hypothesis test and
# use Wines to test whether or not this is the case.
#b)
#H0 = french <= usa
#H1 french > usa
frenchWines = Price[Country=="France"]
usWines = Price[Country=="US"]
t.test(x = frenchWines,usWines,alternative = "greater")
# 0.007 < 0.05 => reject the claim

# Use Wines to give an ppropriate confidence interval for 
# the price of a bottle of wine coming from the Chamapgne province of France.
#c
list = Price[Country=="France" & Province=="Champagne"]
s = sd(list)
mean = mean(list)
abs(qt(0.025,20))
alpha = 1-0.95
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(length(list)) #this is wrong =>  fragen
mean-ME
mean+ME

# paymentTwint = payment[payment=="Twint"]
# s = sd(price[payment=="Twint"])
# n = length(paymentTwint)
# xbar = mean(as.numeric(price[payment=="Twint"]))
# alpha = 1-0.95
# z = qnorm(alpha/2)
# ME = abs(z)*s/sqrt(n)
# xbar-ME
# xbar+ME
# 
# 
# priceChampagne = Price[Province=="Champagne"]
# s = sd(priceChampagne)
# n = length(priceChampagne)
# xbar = mean(priceChampagne)
# alpha = 1-0.95
# z = qnorm(alpha/2)
# ME = abs(z)*s/sqrt(n)
# xbar-ME
# xbar+ME


# 5 
# The time needed to squeeze the juice from a vat of grapes follows an uniform distribution.
# The min. time needed is 17 min. The mean is 25 min. What is the probablity that the time needed
# for a patricular vat is between 20 and 30 min?
# A)
min = 17
mean = 25

1-pnorm(q =30,mean=25,sd=sd(fertility),lower.tail = FALSE) - pnorm(q =20,mean=25,sd=sd(fertility),lower.tail = TRUE) 

# The process needed to fill and cork a standard 750ml bottle of wine follows a normal distribution with
# the mean 30s and standard devitation of 2s. How many seconds (to two decimal places) are needed
# so that at least 70% of all bottles are filled?
#b

mean = 30
sd = 2
qnorm(1-0.7,mean,sd)

# It is claimed that over 27% of the wine produced in the World 
# has a variety Pinot Noir. Create an appropriate hypothesis test 
# and use Wines to thes whether or not this is the case.
#c
x = length(Variety[Variety=="Pinot Noir"])
l = length(Variety)

prop.test(x,l,p=0.27,alternative = "greater")

pHat = length(Variety[Variety=="Pinot Noir"])/length(Variety)
prop.test(x=27,n=100, p = pHat,alternative = "less")
