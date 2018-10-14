##################################################################################################
##                                                                                              ##
##                                   LIU Jixiong                                                ##
##                                Session 1 Stastistique                                        ##
##                                                                                              ##
##################################################################################################

setwd("X:/statistique/session1")
getwd()


####################################################################################################
##                                                                                                ##
##                                       Exercice 1                                               ##☻
##                                                                                                ##
####################################################################################################



sport <- c(9,6,5,7)

label <- c("Rugby","Football","Basketball","Other")


pie(sport, labels = label, main = "Pie chart of Favorite Sports")

barplot (sport, names.arg = label, main = "Bar Chart of  Favorite Sports", col = c("red","blue", "orange","pink"))

####################################################################################################
##                                                                                                ##
##                                       Exercice 2                                               ##☻
##                                                                                                ##
####################################################################################################


data <- read.csv2("Unit1_Piechart.csv", header = T) 
data

#data <- read.csv2(file.choose(), header = "T") 

Favorite_sport <- data$Favorite_Sport
Favorite_sport


frequency_vector <- data$Frequency
frequency_vector

data <- read.csv("Unit1_Heights.csv", header = T) 
data
Heights <- data[,1]
Heights

#
mean(Heights)
#
median(Heights)
#
quantile(Heights)

hist(Heights, breaks = c(1.6,1.7,1.8,1.9), col = "blue", main = "My histogram in R", xlab = "Height", ylab = "Frenquency")

hist(Heights, breaks =3, col = "blue", main = "My histogram in R", xlab = "Height", ylab = "Frenquency")

hist(Heights, col = "blue", main = "My histogram in R", xlab = "Height", ylab = "Frenquency")

quantile(Heights, 0.25)



####################################################################################################
##                                                                                                ##
##                                       Exercice 3                                               ##
##                                                                                                ##
####################################################################################################


#max -min
range_Height <- max(Heights) - min(Heights)
range_Height

#max and min
range(Heights)

# 75% - 25%
IQR(Heights)
quantile(Heights)



####################################################################################################
##                                                                                                ##
##                                       Exercice 4                                               ##
##                                                                                                ##
####################################################################################################

#standart deviation
sd(Heights)

var(Heights)

CV <- function(mean,sd){
  (sd/mean)*100
}

#coefficien variance
CV(mean = mean(Heights), sd = sd (Heights))
CV(3,2)

CV(mean(Heights), sd (Heights))

#Asymmetre coefficien
#AC = 0 >>> Asymetry
#AC > 0, ++++DROIT 

#Install.packages("moments")
library(moments)

#Symetriy
skewness(Heights)

#Well shaped or not
kurtosis(Heights)

boxplot(Heights, horizontal = TRUE, main = "box wisker plot", xlab = "Hyight", ylab = "valure")
abline(v = mean(Heights), col = "red")
