#############################################################################
#
#
#
#
#############################################################################



setwd("C:/workspace/Statistique")
data_resources <- read.csv2("Unit5_Resources.csv", header = TRUE)
Resource <- data_resources$Resource
Price <- data_resources$Price


Resource_Factor <- as.factor(Resource)
lm(Price~Resource_Factor)
anova_resources <- aov(lm(Price~Resource_Factor))
summary(anova_resources)

# pvalue is less than a, so we reject H° (u1=u2=u3=u4)
# Factor "Resource is significant", it makes a difference in the file

install.packages("graphics")
require(graphics)
plot(TukeyHSD(anova_resources,"Resource_Factor",ordered = TRUE))

##因为账号用户名的原因所以不能运行

#________________________________________________________________________________
data_computers <- read.csv2("Unit5_Computers.csv", header = TRUE)
CARD <- data_computers$CARD
SPEED <- data_computers$SPEED
Performance <- data_computers$Performance
CARD_FACRTOR <- as.factor(CARD)
SPEED_FACRTOR <- as.factor(SPEED)
anova_two_factors <- aov(lm(Performance~CARD_FACRTOR*SPEED_FACRTOR))
summary(anova_two_factors)

aggregate(Performance~CARD_FACRTOR+SPEED_FACRTOR,FUN="mean")

interaction.plot(SPEED_FACRTOR, CARD_FACRTOR, Performance, xlab = "Speed_Factor", ylab = "Performance", trace.label = "CARD_FACTOR", main = "Interaction Plot",col = c("blue", "red", "green"), bg = c("blue", "red", "green"), pch = c(18, 24, 22), type = "b")

#________________________________________________________________________________

data_resistance <- read.csv2("Unit5_Resistance.csv", header = TRUE)
Resistance <- data_resistance$Resistance
A <- data_resistance$A
B <- data_resistance$B

A_FACRTOR <- as.factor(A)
B_FACRTOR <- as.factor(B)
anova_two_factors <- aov(lm(Resistance~A_FACRTOR*B_FACRTOR))
summary(anova_two_factors)

aggregate(Performance~CARD_FACRTOR+SPEED_FACRTOR,FUN="mean")

interaction.plot(SPEED_FACRTOR, CARD_FACRTOR, Performance, xlab = "Speed_Factor", ylab = "Performance", trace.label = "CARD_FACTOR", main = "Interaction Plot",col = c("blue", "red", "green"), bg = c("blue", "red", "green"), pch = c(18, 24, 22), type = "b")
