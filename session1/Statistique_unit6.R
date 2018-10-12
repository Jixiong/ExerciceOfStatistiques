setwd("C:/workspace/Statistique")
data_lifespans <- read.csv2("Unit6_Lifespan.csv", header = TRUE)

lifespan <- data_lifespans$Lifespan
price <- data_lifespans$Price
use <- data_lifespans$Use

model1 <- lm(lifespan ~ price + use)
summary(model1)
# intercepet 是零值的时候的B0值
# Price est pour la b1
# Et use est pour la b2

model1 <- lm(lifespan ~ price)
summary(model1)

predict(model1,data.frame(price = 10, use = 0.5))
predict.lm(model1,data.frame(price = 10, use = 0.5), interval = "confidence")
predict.lm(model1,data.frame(price = 10, use = 0.5), interval = "prediction")

data_voltage <- read.csv2("Unit6_Voltage.csv", header = TRUE)
Current <- data_voltage$Current
Voltage <- data_voltage$Voltage

plot(Voltage ~ Current, main = "Dispersion graph",xlab = "Voltage", ylab = "Current", col = "bleu" )

model_log <- lm(Voltage ~ log (Current + 0.01))
summary(model_log)

model_log <- lm(Voltage ~ Current )
summary(model_log)

model_log <- lm(log (Voltage + 0.01) ~ log (Current + 0.01))
summary(model_log)

