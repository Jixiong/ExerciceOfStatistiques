###############################################################
#                     exercice Page 27                        #
###############################################################


1 - pbinom(2, size = 10, prob = 0.02) 
1-dbinom(0, size=5, prob=0.1)
dbinom(0, size=200, prob=0.01)
pbinom(1, size = 77, prob = 0.05)

getwd()
setwd("C:/workspace/Statistique")

data_height <- read.csv("Unit1_Heights.csv", header = TRUE)
data_height

heights <- data_height$Heights
heights

qqnorm(heights)

### if p-value is larger than 0.05,WHICH MEANS IT IS NORMAL
shapiro.test(heights)

pnorm(4.8, mean = 5, sd = 0.1, lower.tail= FALSE) 


############################################################################
#                                                                          #
#                                                                          #
#                             CLASSE 4                                     #
#                                                                          #
#                                                                          #
############################################################################


income <- c(125,135,145,130,120,145,125,130,150,145)
means <- mean(income)
stand_income <- sd(income)

t.test(income, conf.level = 0.95)

############################################################################
#install.packages('TeachingDemos')
prop.test(120, 200, conf.level = 0.95)

##################################################

cost <- c(2.1, -1.0, 1.7, 1.6, 1.4, 2.5, 1.3, 2.6)

t.test(cost, conf.level = 0,95)

################################################
speed <- c(69,60,80,85,68,74,60,86,92)

mean(speed)

t.test(speed, mu =70,  alternative="greater")

prop.test(2,20,p=0.05,alternative="two.sided") 

# in this case it is a strong conclusion, So we can reject the H0, and we accept
#  that the proposion is not 0.05

prop.test(20,200,p=0.05,alternative="two.sided") 


##################################################
#________________________________________________#
##################################################

prop.test(6,200,p=0.02,alternative="less") 
prop.test(60,200,p=0.02,alternative="greater") 
##################################################

countryA <- c(200, 230, 205, 185, 190, 300, 250, 245, 208)
countryB <- c(190, 220, 200, 180, 190, 260, 240, 241, 200)

sd(countryA)
sd(countryB)

library("stats")

var.test( countryA, countryB)
t.test(countryA, countryB, var.equal = TRUE)
###################################################



Before <- c(200, 230, 205, 185, 190, 300, 250, 245, 208)
After <- c(190, 220, 200, 180, 190, 260, 240, 241, 200)
t.test(Before, After, paired = TRUE) 
t.test(Before - After) 

#Since p-value < 0.05, you reject H0 and admit
#that the new operating system makes a difference in the
#number o tasks done by the computers.


############################################################

Before <- c(24, 24, 27, 19, 31, 29, 33, 20, 26)
After <- c(17, 22, 9, 12, 16, 21, 15, 15, 19)
t.test(Before - After)

#############################################################
  
##### this is the personal quizz for this fonction



setwd("C:/workspace/Statistique")

data_expense <- read.csv2("Data_Expenses.csv", header = TRUE)
data_expense

expense_may <- data_expense$ExpensesMay
expense_may

t.test(expense_may)

t.test(expense_may, mu=300, alternative="greater")

expense_june <- data_expense$ExpensesJune

t.test(expense_may - expense_june)
t.test(expense_may, expense_june, paired = TRUE)


