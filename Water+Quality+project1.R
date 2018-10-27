setwd("D:/MY FILES/IIMA EPABA/Project")
data <- read.csv("water_quality_of_groundwater_final2_cleaned.csv")

head(data)
View(data)
#colnames(data)<- c('stationcode','locations','state','min_temp','max_temp','mean_temp','min_ph','max_ph','mean_ph','min_conductivity','max_conductivity','mean_conductivity','min_bod','max_bod','mean_bod','min_nitrate','max_nitrate','mean_nitrate','min_fecalcoliform','max_fecalcoliform','mean_fecalcoliform','min_totalcoliform','max_totalcoliform','mean_totalcoliform')

summary(data)

row.has.na <- apply(data, 1, function(x){any(is.na(x))})

sum(row.has.na)

final_data <- data[!row.has.na,]

View(final_data)

tail(final_data)

#select <- c('stationcode','locations','state','mean_temp','mean_ph','mean_conductivity','mean_bod','mean_nitrate','mean_fecalcoliform','mean_totalcoliform')

#selected_data <- final_data[select]

cor_data <- final_data[,c(4,7,10,13,16,19)]

head(cor_data)

cormat <- cor(cor_data)

head(cormat)

library(ggplot2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data=melted_cormat, aes(x=Var1, y=Var2, fill=value))+ geom_tile()

typeof(final_data$mean_temp)

final_data$mean_temp <- as.numeric(as.double(final_data$mean_temp))
final_data$mean_ph <- as.numeric(as.double(final_data$mean_ph))
final_data$mean_conductivity <- as.numeric(as.double(final_data$mean_conductivity))
final_data$mean_bod <- as.numeric(as.double(final_data$mean_bod))
final_data$mean_nitrate <- as.numeric(as.double(final_data$mean_nitrate))
final_data$mean_fecalcoliform <- as.numeric(as.double(final_data$mean_fecalcoliform))
final_data$WQI <- as.numeric(as.integer(final_data$WQI))
final_data$Result <- as.numeric(as.integer(final_data$WQI))

## 1 - excellent, 2 - good, 3 - poor, 4 - very poor, 5 - unsuitable for drinking
final_data <- within(final_data, Result[WQI < 50] <- 1)
final_data <- within(final_data, Result[WQI > 50 & WQI < 100 ]  <- 2)
final_data <- within(final_data, Result[WQI > 100 & WQI < 200] <- 3)
final_data <- within(final_data, Result[WQI > 200 & WQI < 300] <- 4)
final_data <- within(final_data, Result[WQI > 300] <- 5)

head(final_data)

##plots

df <- data.frame(final_data$mean_bod, final_data$mean_conductivity, final_data$mean_ph, final_data$mean_fecalcoliform, final_data$mean_nitrate, final_data$mean_temp)
plot(df)

plot(final_data$mean_temp)
plot(final_data$mean_ph)
plot(final_data$mean_bod)
plot(final_data$mean_nitrate)
plot(final_data$mean_conductivity)
plot(final_data$mean_fecalcoliform)

summary(lm(final_data$WQI ~ final_data$mean_ph))
summary(lm(final_data$WQI ~ final_data$mean_temp))
summary(lm(final_data$WQI ~ final_data$mean_bod))
summary(lm(final_data$WQI ~ final_data$mean_nitrate))
summary(lm(final_data$WQI ~ final_data$mean_conductivity))
summary(lm(final_data$WQI ~ final_data$mean_fecalcoliform))

plot(lm(final_data$WQI ~ final_data$mean_ph))
plot(lm(final_data$WQI ~ final_data$mean_temp))
plot(lm(final_data$WQI ~ final_data$mean_bod))
plot(lm(final_data$WQI ~ final_data$mean_nitrate))
plot(lm(final_data$WQI ~ final_data$mean_conductivity))
plot(lm(final_data$WQI ~ final_data$mean_fecalcoliform))

lm.wqi <- lm(final_data$WQI ~ final_data$mean_temp + final_data$mean_ph + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)

summary(lm.wqi)

lm.wqi <- lm(final_data$WQI ~ final_data$mean_temp + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)
summary(lm.wqi)

lm.wqi <- lm(final_data$WQI ~ final_data$mean_temp + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)
summary(lm.wqi)

## test


lm.wqi <- lm(final_data$Result ~ final_data$mean_temp + final_data$mean_ph + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)

summary(lm.wqi)

lm.wqi <- lm(final_data$WQI ~ final_data$mean_temp + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)
summary(lm.wqi)

lm.wqi <- lm(final_data$WQI ~ final_data$mean_temp + final_data$mean_conductivity + final_data$mean_bod + final_data$mean_nitrate + final_data$mean_fecalcoliform)
summary(lm.wqi)

