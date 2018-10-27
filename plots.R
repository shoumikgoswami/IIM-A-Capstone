setwd("D:/MY FILES/IIMA EPABA/Project")
data <- read.csv("waterqualitydata.csv")
data$WQI <- as.numeric(as.integer(data$WQI))
data.2017 <- data[data$YEAR %in% "2017",]
View(data.2017)
boxplot(data.2017$WQI~data.2017$STATE, data=data.2017, main = "WQI of each state in 2017", xlab = "States", Ylab= "WQI")
library(ggplot2)
boxplot <- ggplot(data.2017, aes(x=data.2017$STATE, y=data.2017$WQI)) +
  geom_boxplot(outlier.colour=NA) + 
  coord_cartesian(ylim = c(0, 500))
print(boxplot)
scatter <- ggplot(data.2017, aes(x=data.2017$STATE, y=data.2017$WQI)) + 
  geom_point(size=5) +
  coord_cartesian(ylim = c(0, 400))+
  geom_text(label= data.2017$WQI, vjust = 2)
print(scatter)



data.andhra <- data[data$STATE %in% "ANDHRA PRADESH",]
data.assam <- data[data$STATE %in% "ASSAM",]
data.bihar <- data[data$STATE %in% "BIHAR",]
data.chhattisgarh <- data[data$STATE %in% "CHHATTISGARH",]
data.dadra <- data[data$STATE %in% "DADRA NAGAR HAVELI",]
data.daman <- data[data$STATE %in% "DAMAN & DIU",]
data.goa <- data[data$STATE %in% "GOA",]
data.himachal <- data[data$STATE %in% "HIMACHAL PRADESH",]
data.kerala <- data[data$STATE %in% "KERALA",]
data.lakshadweep <- data[data$STATE %in% "LAKSHADWEEP",]
data.madhyapradesh <- data[data$STATE %in% "MADHYA PRADESH",]
data.maharashtra <- data[data$STATE %in% "MAHARASHTRA",]
data.mizoram <- data[data$STATE %in% "MIZORAM",]
data.odisha <- data[data$STATE %in% "ODISHA",]
data.pondi <- data[data$STATE %in% "PONDICHERRY",]
data.punjab <- data[data$STATE %in% "PUNJAB",]
data.rajasthan <- data[data$STATE %in% "RAJASTHAN",]
data.tripura <- data[data$STATE %in% "TRIPURA",]
data.uttarpradesh <- data[data$STATE %in% "UTTAR PRADESH",]
data.westbengal <- data[data$STATE %in% "WEST BENGAL",]

boxplot.andhra <- boxplot(data.andhra$WQI~data.andhra$YEAR, data = data.andhra, main = "WQI of Andhra Pradesh", xlab = "Year", ylab = "WQI")
print(boxplot.andhra)
boxplot.assam <- boxplot(data.assam$WQI~data.assam$YEAR, data = data.assam, main = "WQI of Assam", xlab = "Year", ylab = "WQI", ylim = c(0,1200))
print(boxplot.assam)
boxplot.bihar <- boxplot(data.bihar$WQI~data.bihar$YEAR, data = data.bihar, main = "WQI of Bihar", xlab = "Year", ylab = "WQI")
print(boxplot.bihar)
boxplot.chhattisgarh <- boxplot(data.chhattisgarh$WQI~data.chhattisgarh$YEAR, data = data.chhattisgarh, main = "WQI of Chhattisgarh", xlab = "Year", ylab = "WQI")
print(boxplot.chhattisgarh)
boxplot.dadra <- boxplot(data.dadra$WQI~data.dadra$YEAR, data = data.dadra, main = "WQI of Dadra Nagar Haveli", xlab = "Year", ylab = "WQI")
print(boxplot.dadra)
boxplot.daman <- boxplot(data.daman$WQI~data.daman$YEAR, data = data.daman, main = "WQI of Daman & Diu", xlab = "Year", ylab = "WQI")
print(boxplot.daman)
boxplot.goa <- boxplot(data.goa$WQI~data.goa$YEAR, data = data.goa, main = "WQI of Goa", xlab = "Year", ylab = "WQI")
print(boxplot.goa)
boxplot.himachal <- boxplot(data.himachal$WQI~data.himachal$YEAR, data = data.himachal, main = "WQI of Himachal Pradesh", xlab = "Year", ylab = "WQI", ylim = c(0,200))
print(boxplot.himachal)
boxplot.kerala <- boxplot(data.kerala$WQI~data.kerala$YEAR, data = data.kerala, main = "WQI of Kerala", xlab = "Year", ylab = "WQI")
print(boxplot.kerala)
boxplot.lakshadweep <- boxplot(data.lakshadweep$WQI~data.lakshadweep$YEAR, data = data.lakshadweep, main = "WQI of Lakshadweep", xlab = "Year", ylab = "WQI", ylim = c(0,20000))
print(boxplot.lakshadweep)
boxplot.madhyapradesh <- boxplot(data.madhyapradesh$WQI~data.madhyapradesh$YEAR, data = data.madhyapradesh, main = "WQI of Madhya Pradesh", xlab = "Year", ylab = "WQI")
print(boxplot.madhyapradesh)
boxplot.maharashtra <- boxplot(data.maharashtra$WQI~data.maharashtra$YEAR, data = data.maharashtra, main = "WQI of Maharashtra", xlab = "Year", ylab = "WQI")
print(boxplot.maharashtra)
boxplot.mizoram <- boxplot(data.mizoram$WQI~data.mizoram$YEAR, data = data.mizoram, main = "WQI of Mizoram", xlab = "Year", ylab = "WQI")
print(boxplot.mizoram)
boxplot.odisha <- boxplot(data.odisha$WQI~data.odisha$YEAR, data = data.odisha, main = "WQI of Odisha", xlab = "Year", ylab = "WQI", ylim = c(0,200))
print(boxplot.odisha)
boxplot.pondi <- boxplot(data.pondi$WQI~data.pondi$YEAR, data = data.pondi, main = "WQI of Pondicherry", xlab = "Year", ylab = "WQI")
print(boxplot.pondi)
boxplot.punjab <- boxplot(data.punjab$WQI~data.punjab$YEAR, data = data.punjab, main = "WQI of Punjab", xlab = "Year", ylab = "WQI")
print(boxplot.punjab)
boxplot.rajasthan <- boxplot(data.rajasthan$WQI~data.rajasthan$YEAR, data = data.rajasthan, main = "WQI of Rajasthan", xlab = "Year", ylab = "WQI")
print(boxplot.rajasthan)
boxplot.tripura <- boxplot(data.tripura$WQI~data.tripura$YEAR, data = data.tripura, main = "WQI of Tripura", xlab = "Year", ylab = "WQI")
print(boxplot.tripura)
boxplot.uttarpradesh <- boxplot(data.uttarpradesh$WQI~data.uttarpradesh$YEAR, data = data.uttarpradesh, main = "WQI of Uttar Pradesh", xlab = "Year", ylab = "WQI", ylim = c(0,200))
print(boxplot.uttarpradesh)
boxplot.westbengal <- boxplot(data.westbengal$WQI~data.westbengal$YEAR, data = data.westbengal, main = "WQI of West Bengal", xlab = "Year", ylab = "WQI", ylim = c(0,500))
print(boxplot.westbengal)
