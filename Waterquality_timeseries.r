
library(forecast)
library(fpp)
library(TTR)
library(ggplot2)

data <- read.csv("waterqualitydata.csv")

row.has.na <- apply(data, 1, function(x){any(is.na(x))})

sum(row.has.na)

final_data <- data[!row.has.na,]

head(final_data)

final_data$mean_temp <- as.numeric(as.double(final_data$mean_temp))
final_data$mean_ph <- as.numeric(as.double(final_data$mean_ph))
final_data$mean_conductivity <- as.numeric(as.double(final_data$mean_conductivity))
final_data$mean_bod <- as.numeric(as.double(final_data$mean_bod))
final_data$mean_nitrate <- as.numeric(as.double(final_data$mean_nitrate))
final_data$mean_fecalcoliform <- as.numeric(as.double(final_data$mean_fecalcoliform))
final_data$WQI <- as.numeric(as.integer(final_data$WQI))

data.andhra <- final_data[final_data$STATE %in% "ANDHRA PRADESH",]
data.assam <- final_data[final_data$STATE %in% "ASSAM",]
data.bihar <- final_data[final_data$STATE %in% "BIHAR",]
data.chhattisgarh <- final_data[final_data$STATE %in% "CHHATTISGARH",]
data.dadra <- final_data[final_data$STATE %in% "DADRA NAGAR HAVELI",]
data.daman <- final_data[final_data$STATE %in% "DAMAN & DIU",]
data.goa <- final_data[final_data$STATE %in% "GOA",]
data.himachal <- final_data[final_data$STATE %in% "HIMACHAL PRADESH",]
data.kerala <- final_data[final_data$STATE %in% "KERALA",]
data.lakshadweep <- final_data[final_data$STATE %in% "LAKSHADWEEP",]
data.madhyapradesh <- final_data[final_data$STATE %in% "MADHYA PRADESH",]
data.maharashtra <- final_data[final_data$STATE %in% "MAHARASHTRA",]
data.mizoram <- final_data[final_data$STATE %in% "MIZORAM",]
data.odisha <- final_data[final_data$STATE %in% "ODISHA",]
data.pondi <- final_data[final_data$STATE %in% "PONDICHERRY",]
data.punjab <- final_data[final_data$STATE %in% "PUNJAB",]
data.rajasthan <- final_data[final_data$STATE %in% "RAJASTHAN",]
data.tripura <- final_data[final_data$STATE %in% "TRIPURA",]
data.uttarpradesh <- final_data[final_data$STATE %in% "UTTAR PRADESH",]
data.westbengal <- final_data[final_data$STATE %in% "WEST BENGAL",]


ap_temp <- data.andhra$mean_temp
ap_ph <- data.andhra$mean_ph
ap_conductivity <- data.andhra$mean_conductivity
ap_bod <- data.andhra$mean_bod
ap_nitrate <- data.andhra$mean_nitratenitrite
ap_fecal <- data.andhra$mean_fecalcoliform

ap_temp_ts <-ts(ap_temp, start=c(2006, 1), end=c(2014, 1), frequency=1)
ap_ph_ts <-ts(ap_ph, start=c(2006, 1), end=c(2014, 1), frequency=1)
ap_conductivity_ts <-ts(ap_conductivity, start=c(2006, 1), end=c(2014, 1), frequency=1)
ap_bod_ts <- ts(ap_bod, start=c(2006, 1), end=c(2014, 1), frequency=1)
ap_nitrate_ts <- ts(ap_nitrate, start=c(2006, 1), end=c(2014, 1), frequency=1)
ap_fecal_ts <- ts(ap_fecal, start=c(2006, 1), end=c(2014, 1), frequency=1)

plot(ap_temp_ts)
plot(ap_ph_ts)
plot(ap_conductivity_ts)
plot(ap_bod_ts)
plot(ap_nitrate_ts)
plot(ap_fecal_ts)

ggseasonplot(ap_temp_ts, col = rainbow(7), year.labels = TRUE)
ggseasonplot(ap_ph_ts, col = rainbow(7), year.labels = TRUE)
ggseasonplot(ap_conductivity_ts, col = rainbow(7), year.labels = TRUE)
ggseasonplot(ap_bod_ts, col = rainbow(7), year.labels = TRUE)
ggseasonplot(ap_nitrate_ts, col = rainbow(7), year.labels = TRUE)
ggseasonplot(ap_fecal_ts, col = rainbow(7), year.labels = TRUE)

library(tseries)
adf.test(diff(diff(ap_temp_ts)))
adf.test(diff(diff(ap_ph_ts)))
adf.test(diff(ap_conductivity_ts))
adf.test(diff(diff(ap_bod_ts)))
adf.test(diff(diff(ap_nitrate_ts)))
adf.test(diff(diff(ap_fecal_ts)))

fit_ap_temp_ts <- HoltWinters(ap_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_ap_temp_ts, 3)
plot(forecast(fit_ap_temp_ts,3))

ARIMAfit_ap_temp_ts <- auto.arima(ap_temp_ts, d=2, approximation=FALSE,trace=FALSE)
fr_ap_temp_ts <- forecast(ARIMAfit_ap_temp_ts,3)
fr_ap_temp_ts
plot(fr_ap_temp_ts)

fit_ap_ph_ts <- HoltWinters(ap_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_ap_ph_ts, 3)
plot(forecast(fit_ap_ph_ts,3))

ARIMAfit_ap_conductivity_ts <- auto.arima(ap_conductivity_ts,d=2, approximation=FALSE,trace=FALSE)
fr_ap_conductivity_ts <- forecast(ARIMAfit_ap_conductivity_ts,3)
fr_ap_conductivity_ts
plot(fr_ap_conductivity_ts)

ARIMAfit_ap_bod_ts <- auto.arima(ap_bod_ts, d=2, approximation=FALSE,trace=FALSE)
fr_ap_bod_ts <- forecast(ARIMAfit_ap_bod_ts,3)
fr_ap_bod_ts
plot(fr_ap_bod_ts)

ARIMAfit_ap_nitrate_ts <- auto.arima(ap_nitrate_ts, d=2, approximation=FALSE,trace=FALSE)
fr_ap_nitrate_ts <- forecast(ARIMAfit_ap_nitrate_ts,3)
fr_ap_nitrate_ts
plot(fr_ap_nitrate_ts)

ARIMAfit_ap_fecal_ts <- auto.arima(ap_fecal_ts, d=2, approximation=FALSE,trace=FALSE)
fr_ap_fecal_ts <- forecast(ARIMAfit_ap_fecal_ts,3)
fr_ap_fecal_ts
plot(fr_ap_fecal_ts)

as_temp_ts <-ts(data.assam$mean_temp, start=c(2006, 1), end=c(2014, 1), frequency=1)
as_ph_ts <-ts(data.assam$mean_ph, start=c(2006, 1), end=c(2014, 1), frequency=1)
as_conductivity_ts <-ts(data.assam$mean_conductivity, start=c(2006, 1), end=c(2014, 1), frequency=1)
as_bod_ts <- ts(data.assam$mean_bod, start=c(2006, 1), end=c(2014, 1), frequency=1)
as_nitrate_ts <- ts(data.assam$mean_nitratenitrite, start=c(2006, 1), end=c(2014, 1), frequency=1)
as_fecal_ts <- ts(data.assam$mean_fecalcoliform, start=c(2006, 1), end=c(2014, 1), frequency=1)

## Plots
plot(as_temp_ts)
plot(as_ph_ts)
plot(as_conductivity_ts)
plot(as_bod_ts)
plot(as_nitrate_ts)
plot(as_fecal_ts)

adf.test(diff(diff(as_temp_ts)))
adf.test(diff(as_ph_ts))
adf.test(diff(as_conductivity_ts))
adf.test(diff(diff(as_bod_ts)))
adf.test(log(as_nitrate_ts))
adf.test(as_fecal_ts)

ARIMAfit_as_temp_ts <- auto.arima(as_temp_ts, d=2, approximation=FALSE,trace=FALSE)
fr_as_temp_ts <- forecast(ARIMAfit_as_temp_ts,3)
fr_as_temp_ts
plot(fr_as_temp_ts)

ARIMAfit_as_ph_ts <- auto.arima(as_ph_ts, d=1, approximation=FALSE,trace=FALSE)
fr_as_ph_ts <- forecast(ARIMAfit_as_ph_ts,3)
fr_as_ph_ts
plot(fr_as_ph_ts)

ARIMAfit_as_conductivity_ts <- auto.arima(as_conductivity_ts, approximation=FALSE,trace=FALSE)
fr_as_conductivity_ts <- forecast(ARIMAfit_as_conductivity_ts,3)
fr_as_conductivity_ts
plot(fr_as_conductivity_ts)

ARIMAfit_as_bod_ts <- auto.arima(as_bod_ts, d=2, approximation=FALSE,trace=FALSE)
fr_as_bod_ts <- forecast(ARIMAfit_as_bod_ts,3)
fr_as_bod_ts
plot(fr_as_bod_ts)

ARIMAfit_as_nitrate_ts <- auto.arima(log(as_nitrate_ts), d=2, approximation=FALSE,trace=FALSE)
fr_as_nitrate_ts <- forecast(ARIMAfit_as_nitrate_ts,3)
fr_as_nitrate_ts
plot(fr_as_nitrate_ts)

ARIMAfit_as_fecal_ts <- auto.arima(as_fecal_ts,d=2, approximation=FALSE,trace=FALSE)
fr_as_fecal_ts <- forecast(ARIMAfit_as_fecal_ts,3)
fr_as_fecal_ts
plot(fr_as_fecal_ts)

goa_temp_ts <-ts(data.goa$mean_temp, start=c(2006, 1), end=c(2013, 1), frequency=1)
goa_ph_ts <-ts(data.goa$mean_ph, start=c(2006, 1), end=c(2013, 1), frequency=1)
goa_conductivity_ts <-ts(data.goa$mean_conductivity, start=c(2006, 1), end=c(2013, 1), frequency=1)
goa_bod_ts <- ts(data.goa$mean_bod, start=c(2006, 1), end=c(2013, 1), frequency=1)
goa_nitrate_ts <- ts(data.goa$mean_nitratenitrite, start=c(2006, 1), end=c(2013, 1), frequency=1)
goa_fecal_ts <- ts(data.goa$mean_fecalcoliform, start=c(2006, 1), end=c(2013, 1), frequency=1)

#Plots
plot(goa_temp_ts)
plot(goa_ph_ts)
plot(goa_conductivity_ts)
plot(goa_bod_ts)
plot(goa_nitrate_ts)
plot(goa_fecal_ts)

adf.test(diff(goa_temp_ts))
adf.test(goa_ph_ts)
adf.test(goa_conductivity_ts)
adf.test(goa_bod_ts)
adf.test(diff(goa_nitrate_ts))
adf.test(goa_fecal_ts)

ARIMAfit_goa_temp_ts <- auto.arima(goa_temp_ts, d=2, approximation=FALSE,trace=FALSE)
fr_goa_temp_ts <- forecast(ARIMAfit_goa_temp_ts,3)
fr_goa_temp_ts
plot(fr_goa_temp_ts)

ARIMAfit_goa_ph_ts <- auto.arima(goa_ph_ts, d=2, approximation=FALSE,trace=FALSE)
fr_goa_ph_ts <- forecast(ARIMAfit_goa_ph_ts,3)
fr_goa_ph_ts
plot(fr_goa_ph_ts)

fit_goa_conductivity_ts <- HoltWinters(goa_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_goa_conductivity_ts, 3)
plot(forecast(fit_goa_conductivity_ts,3))

fit_goa_bod_ts <- HoltWinters(goa_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_goa_bod_ts, 3)
plot(forecast(fit_goa_bod_ts,3))

ARIMAfit_goa_nirate_ts <- auto.arima(goa_nitrate_ts, d=1,approximation=FALSE,trace=FALSE)
fr_goa_nirate_ts <- forecast(ARIMAfit_goa_nirate_ts,3)
fr_goa_nirate_ts
plot(fr_goa_nirate_ts)

fit_goa_fecal_ts <- HoltWinters(goa_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_goa_fecal_ts, 3)
plot(forecast(fit_goa_fecal_ts,3))

hp_temp_ts <-ts(data.himachal$mean_temp, start=c(2006, 1), end=c(2014, 1), frequency=1)
hp_ph_ts <-ts(data.himachal$mean_ph, start=c(2006, 1), end=c(2014, 1), frequency=1)
hp_conductivity_ts <-ts(data.himachal$mean_conductivity, start=c(2006, 1), end=c(2014, 1), frequency=1)
hp_bod_ts <- ts(data.himachal$mean_bod, start=c(2006, 1), end=c(2014, 1), frequency=1)
hp_nitrate_ts <- ts(data.himachal$mean_nitratenitrite, start=c(2006, 1), end=c(2014, 1), frequency=1)
hp_fecal_ts <- ts(data.himachal$mean_fecalcoliform, start=c(2006, 1), end=c(2014, 1), frequency=1)

#Plots
plot(hp_temp_ts)
plot(hp_ph_ts)
plot(hp_conductivity_ts)
plot(hp_bod_ts)
plot(hp_nitrate_ts)
plot(hp_fecal_ts)

adf.test(hp_temp_ts)
adf.test(diff(hp_ph_ts))
adf.test(hp_conductivity_ts)
adf.test(diff(hp_bod_ts))
adf.test(diff(hp_nitrate_ts))
adf.test(hp_fecal_ts)

fit_hp_temp_ts <- HoltWinters(hp_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_hp_temp_ts, 3)
plot(forecast(fit_hp_temp_ts,3))

ARIMAfit_hp_ph_ts <- auto.arima(hp_ph_ts, d=2,approximation=FALSE,trace=FALSE)
fr_hp_ph_ts <- forecast(ARIMAfit_hp_ph_ts,3)
fr_hp_ph_ts
plot(fr_hp_ph_ts)

fit_hp_conductivity_ts <- HoltWinters(hp_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_hp_conductivity_ts, 3)
plot(forecast(fit_hp_conductivity_ts,3))

ARIMAfit_hp_bod_ts <- auto.arima(hp_bod_ts, d=2,approximation=FALSE,trace=FALSE)
fr_hp_bod_ts <- forecast(ARIMAfit_hp_bod_ts,3)
fr_hp_bod_ts
plot(fr_hp_bod_ts)

ARIMAfit_hp_nitrate_ts <- auto.arima(hp_nitrate_ts, d=1,approximation=FALSE,trace=FALSE)
fr_hp_nitrate_ts <- forecast(ARIMAfit_hp_nitrate_ts,3)
fr_hp_nitrate_ts
plot(fr_hp_nitrate_ts)

fit_hp_fecal_ts <- HoltWinters(hp_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_hp_fecal_ts, 3)
plot(forecast(fit_hp_fecal_ts,3))

kr_temp_ts <-ts(data.kerala$mean_temp, start=c(2006, 1), end=c(2014, 1), frequency=1)
kr_ph_ts <-ts(data.kerala$mean_ph, start=c(2006, 1), end=c(2014, 1), frequency=1)
kr_conductivity_ts <-ts(data.kerala$mean_conductivity, start=c(2006, 1), end=c(2014, 1), frequency=1)
kr_bod_ts <- ts(data.kerala$mean_bod, start=c(2006, 1), end=c(2014, 1), frequency=1)
kr_nitrate_ts <- ts(data.kerala$mean_nitratenitrite, start=c(2006, 1), end=c(2014, 1), frequency=1)
kr_fecal_ts <- ts(data.kerala$mean_fecalcoliform, start=c(2006, 1), end=c(2014, 1), frequency=1)

#Plots
plot(kr_temp_ts)
plot(kr_ph_ts)
plot(kr_conductivity_ts)
plot(kr_bod_ts)
plot(kr_nitrate_ts)
plot(kr_fecal_ts)

adf.test(diff(kr_temp_ts))
adf.test(diff(kr_ph_ts))
adf.test(kr_conductivity_ts)
adf.test(kr_bod_ts)
adf.test(diff(kr_nitrate_ts))
adf.test(kr_fecal_ts)

ARIMAfit_kr_temp_ts <- auto.arima(kr_temp_ts,d=2,approximation=FALSE,trace=FALSE)
fr_kr_temp_ts <- forecast(ARIMAfit_kr_temp_ts,3)
fr_kr_temp_ts
plot(fr_kr_temp_ts)

ARIMAfit_kr_ph_ts <- auto.arima(kr_ph_ts,d=1,approximation=FALSE,trace=FALSE)
fr_kr_ph_ts <- forecast(ARIMAfit_kr_ph_ts,3)
fr_kr_ph_ts
plot(fr_kr_ph_ts)

fit_kr_conductivity_ts <- HoltWinters(kr_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_kr_conductivity_ts, 3)
plot(forecast(fit_kr_conductivity_ts,3))

fit_kr_bod_ts <- HoltWinters(kr_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_kr_bod_ts, 3)
plot(forecast(fit_kr_bod_ts,3))

ARIMAfit_kr_nitrate_ts <- auto.arima(kr_nitrate_ts,d=2,approximation=FALSE,trace=FALSE)
fr_kr_nitrate_ts <- forecast(ARIMAfit_kr_nitrate_ts,3)
fr_kr_nitrate_ts
plot(fr_kr_nitrate_ts)

fit_kr_fecal_ts <- HoltWinters(kr_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_kr_fecal_ts, 3)
plot(forecast(fit_kr_fecal_ts,3))

mp_temp_ts <-ts(data.madhyapradesh$mean_temp, start=c(2006, 1), end=c(2014, 1), frequency=1)
mp_ph_ts <-ts(data.madhyapradesh$mean_ph, start=c(2006, 1), end=c(2014, 1), frequency=1)
mp_conductivity_ts <-ts(data.madhyapradesh$mean_conductivity, start=c(2006, 1), end=c(2014, 1), frequency=1)
mp_bod_ts <- ts(data.madhyapradesh$mean_bod, start=c(2006, 1), end=c(2014, 1), frequency=1)
mp_nitrate_ts <- ts(data.madhyapradesh$mean_nitratenitrite, start=c(2006, 1), end=c(2014, 1), frequency=1)
mp_fecal_ts <- ts(data.madhyapradesh$mean_fecalcoliform, start=c(2006, 1), end=c(2014, 1), frequency=1)

#Plots
plot(mp_temp_ts)
plot(mp_ph_ts)
plot(mp_conductivity_ts)
plot(mp_bod_ts)
plot(mp_nitrate_ts)
plot(mp_fecal_ts)

adf.test(mp_temp_ts)
adf.test(mp_ph_ts)
adf.test(mp_conductivity_ts)
adf.test(mp_bod_ts)
adf.test(diff(mp_nitrate_ts))
adf.test(mp_fecal_ts)

fit_mp_temp_ts <- HoltWinters(mp_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mp_temp_ts, 3)
plot(forecast(fit_mp_temp_ts,3))

ARIMAfit_mp_ph_ts <- auto.arima(mp_ph_ts,d=2,approximation=FALSE,trace=FALSE)
fr_mp_ph_ts <- forecast(ARIMAfit_mp_ph_ts,3)
fr_mp_ph_ts
plot(fr_mp_ph_ts)

ARIMAfit_mp_conductivity_ts <- auto.arima(mp_conductivity_ts,d=2,approximation=FALSE,trace=FALSE)
fr_mp_conductivity_ts <- forecast(ARIMAfit_mp_conductivity_ts,3)
fr_mp_conductivity_ts
plot(fr_mp_conductivity_ts)

fit_mp_bod_ts <- HoltWinters(mp_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mp_bod_ts, 3)
plot(forecast(fit_mp_bod_ts,3))

fit_mp_nitrate_ts <- HoltWinters(mp_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mp_nitrate_ts, 3)
plot(forecast(fit_mp_nitrate_ts,3))

ARIMAfit_mp_fecal_ts <- auto.arima(mp_fecal_ts, d=1,approximation=FALSE,trace=FALSE)
fr_mp_fecal_ts <- forecast(ARIMAfit_mp_fecal_ts,3)
fr_mp_fecal_ts
plot(fr_mp_fecal_ts)

data.maharashtra1 <- data.maharashtra[data.maharashtra$YEAR==2011|data.maharashtra$YEAR==2012|data.maharashtra$YEAR == 2013|data.maharashtra$YEAR == 2014 ,]

data.maharashtra1

mh_temp_ts <-ts(data.maharashtra1$mean_temp, start=c(2011, 1), end=c(2014, 1), frequency=1)
mh_ph_ts <-ts(data.maharashtra1$mean_ph, start=c(2011, 1), end=c(2014, 1), frequency=1)
mh_conductivity_ts <-ts(data.maharashtra1$mean_conductivity, start=c(2011, 1), end=c(2014, 1), frequency=1)
mh_bod_ts <- ts(data.maharashtra1$mean_bod, start=c(2011, 1), end=c(2014, 1), frequency=1)
mh_nitrate_ts <- ts(data.maharashtra1$mean_nitratenitrite, start=c(2011, 1), end=c(2014, 1), frequency=1)
mh_fecal_ts <- ts(data.maharashtra1$mean_fecalcoliform, start=c(2011, 1), end=c(2014, 1), frequency=1)

#Plots
plot(mh_temp_ts)
plot(mh_ph_ts)
plot(mh_conductivity_ts)
plot(mh_bod_ts)
plot(mh_nitrate_ts)
plot(mh_fecal_ts)

adf.test(mh_temp_ts)
adf.test(mh_ph_ts)
adf.test(mh_conductivity_ts)
adf.test(mh_bod_ts)
adf.test(diff(mh_nitrate_ts))
adf.test(mh_fecal_ts)

fit_mh_temp_ts <- HoltWinters(mh_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_temp_ts, 3)
plot(forecast(fit_mh_temp_ts,3))

fit_mh_ph_ts <- HoltWinters(mh_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_ph_ts, 3)
plot(forecast(fit_mh_ph_ts,3))

fit_mh_conductivity_ts <- HoltWinters(mh_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_conductivity_ts, 3)
plot(forecast(fit_mh_conductivity_ts,3))

fit_mh_bod_ts <- HoltWinters(mh_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_bod_ts, 3)
plot(forecast(fit_mh_bod_ts,3))

fit_mh_nitrate_ts <- HoltWinters(mh_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_nitrate_ts, 3)
plot(forecast(fit_mh_nitrate_ts,3))

fit_mh_fecal_ts <- HoltWinters(mh_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_mh_fecal_ts, 3)
plot(forecast(fit_mh_fecal_ts,3))

data.odisha1 <- data.odisha[data.odisha$YEAR==2011|data.odisha$YEAR==2012|data.odisha$YEAR == 2013|data.odisha$YEAR == 2014 ,]

data.odisha1

od_temp_ts <-ts(data.odisha1$mean_temp, start=c(2011, 1), end=c(2014, 1), frequency=1)
od_ph_ts <-ts(data.odisha1$mean_ph, start=c(2011, 1), end=c(2014, 1), frequency=1)
od_conductivity_ts <-ts(data.odisha1$mean_conductivity, start=c(2011, 1), end=c(2014, 1), frequency=1)
od_bod_ts <- ts(data.odisha1$mean_bod, start=c(2011, 1), end=c(2014, 1), frequency=1)
od_nitrate_ts <- ts(data.odisha1$mean_nitratenitrite, start=c(2011, 1), end=c(2014, 1), frequency=1)
od_fecal_ts <- ts(data.odisha1$mean_fecalcoliform, start=c(2011, 1), end=c(2014, 1), frequency=1)

#Plots
plot(od_temp_ts)
plot(od_ph_ts)
plot(od_conductivity_ts)
plot(od_bod_ts)
plot(od_nitrate_ts)
plot(od_fecal_ts)

adf.test(od_temp_ts)
adf.test(od_ph_ts)
adf.test(od_conductivity_ts)
adf.test(od_bod_ts)
adf.test(od_nitrate_ts)
adf.test(od_fecal_ts)

fit_od_temp_ts <- HoltWinters(od_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_temp_ts, 3)
plot(forecast(fit_od_temp_ts,3))

fit_od_ph_ts <- HoltWinters(od_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_ph_ts, 3)
plot(forecast(fit_od_ph_ts,3))

fit_od_conductivity_ts <- HoltWinters(od_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_conductivity_ts, 3)
plot(forecast(fit_od_conductivity_ts,3))

fit_od_bod_ts <- HoltWinters(od_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_bod_ts, 3)
plot(forecast(fit_od_bod_ts,3))

fit_od_nitrate_ts <- HoltWinters(od_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_nitrate_ts, 3)
plot(forecast(fit_od_nitrate_ts,3))

fit_od_fecal_ts <- HoltWinters(od_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_od_fecal_ts, 3)
plot(forecast(fit_od_fecal_ts,3))

unique(data.rajasthan$YEAR)

data.raj1 <- data.rajasthan[data.rajasthan$YEAR==2011|data.rajasthan$YEAR==2012|data.rajasthan$YEAR == 2013|data.rajasthan$YEAR == 2014 ,]

raj_temp_ts <-ts(data.raj1$mean_temp, start=c(2011, 1), end=c(2014, 1), frequency=1)
raj_ph_ts <-ts(data.raj1$mean_ph, start=c(2011, 1), end=c(2014, 1), frequency=1)
raj_conductivity_ts <-ts(data.raj1$mean_conductivity, start=c(2011, 1), end=c(2014, 1), frequency=1)
raj_bod_ts <- ts(data.raj1$mean_bod, start=c(2011, 1), end=c(2014, 1), frequency=1)
raj_nitrate_ts <- ts(data.raj1$mean_nitratenitrite, start=c(2011, 1), end=c(2014, 1), frequency=1)
raj_fecal_ts <- ts(data.raj1$mean_fecalcoliform, start=c(2011, 1), end=c(2014, 1), frequency=1)

#Plots
plot(raj_temp_ts)
plot(raj_ph_ts)
plot(raj_conductivity_ts)
plot(raj_bod_ts)
plot(raj_nitrate_ts)
plot(raj_fecal_ts)

adf.test(raj_temp_ts)
adf.test(raj_ph_ts)
adf.test(raj_conductivity_ts)
adf.test(raj_bod_ts)
adf.test(raj_nitrate_ts)
adf.test(raj_fecal_ts)

fit_raj_temp_ts <- HoltWinters(raj_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_temp_ts, 3)
plot(forecast(fit_raj_temp_ts,3))

fit_raj_ph_ts <- HoltWinters(raj_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_ph_ts, 3)
plot(forecast(fit_raj_ph_ts,3))

fit_raj_conductivity_ts <- HoltWinters(raj_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_conductivity_ts, 3)
plot(forecast(fit_raj_conductivity_ts,3))

fit_raj_bod_ts <- HoltWinters(raj_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_bod_ts, 3)
plot(forecast(fit_raj_bod_ts,3))

fit_raj_nitrate_ts <- HoltWinters(raj_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_nitrate_ts, 3)
plot(forecast(fit_raj_nitrate_ts,3))

fit_raj_fecal_ts <- HoltWinters(raj_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_raj_fecal_ts, 3)
plot(forecast(fit_raj_fecal_ts,3))

unique(data.uttarpradesh$YEAR)

data.up1 <- data.uttarpradesh[data.uttarpradesh$YEAR==2011|data.uttarpradesh$YEAR==2012|data.uttarpradesh$YEAR == 2013|data.uttarpradesh$YEAR == 2014 ,]

up1_temp_ts <-ts(data.up1$mean_temp, start=c(2011, 1), end=c(2014, 1), frequency=1)
up1_ph_ts <-ts(data.up1$mean_ph, start=c(2011, 1), end=c(2014, 1), frequency=1)
up1_conductivity_ts <-ts(data.up1$mean_conductivity, start=c(2011, 1), end=c(2014, 1), frequency=1)
up1_bod_ts <- ts(data.up1$mean_bod, start=c(2011, 1), end=c(2014, 1), frequency=1)
up1_nitrate_ts <- ts(data.up1$mean_nitratenitrite, start=c(2011, 1), end=c(2014, 1), frequency=1)
up1_fecal_ts <- ts(data.up1$mean_fecalcoliform, start=c(2011, 1), end=c(2014, 1), frequency=1)

#Plots
plot(up1_temp_ts)
plot(up1_ph_ts)
plot(up1_conductivity_ts)
plot(up1_bod_ts)
plot(up1_nitrate_ts)
plot(up1_fecal_ts)

fit_up1_temp_ts <- HoltWinters(up1_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_temp_ts, 3)
plot(forecast(fit_up1_temp_ts,3))

fit_up1_ph_ts <- HoltWinters(up1_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_ph_ts, 3)
plot(forecast(fit_up1_ph_ts,3))

fit_up1_conductivity_ts <- HoltWinters(up1_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_conductivity_ts, 3)
plot(forecast(fit_up1_conductivity_ts,3))

fit_up1_bod_ts <- HoltWinters(up1_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_bod_ts, 3)
plot(forecast(fit_up1_bod_ts,3))

fit_up1_nitrate_ts <- HoltWinters(up1_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_nitrate_ts, 3)
plot(forecast(fit_up1_nitrate_ts,3))

fit_up1_fecal_ts <- HoltWinters(up1_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_up1_fecal_ts, 3)
plot(forecast(fit_up1_fecal_ts,3))

unique(data.westbengal$YEAR)

data.wb <- data.westbengal[data.westbengal$YEAR==2011|data.westbengal$YEAR==2012|data.westbengal$YEAR == 2013|data.westbengal$YEAR == 2014 ,]

wb_temp_ts <-ts(data.wb$mean_temp, start=c(2011, 1), end=c(2014, 1), frequency=1)
wb_ph_ts <-ts(data.wb$mean_ph, start=c(2011, 1), end=c(2014, 1), frequency=1)
wb_conductivity_ts <-ts(data.wb$mean_conductivity, start=c(2011, 1), end=c(2014, 1), frequency=1)
wb_bod_ts <- ts(data.wb$mean_bod, start=c(2011, 1), end=c(2014, 1), frequency=1)
wb_nitrate_ts <- ts(data.wb$mean_nitratenitrite, start=c(2011, 1), end=c(2014, 1), frequency=1)
wb_fecal_ts <- ts(data.wb$mean_fecalcoliform, start=c(2011, 1), end=c(2014, 1), frequency=1)

#Plots
plot(wb_temp_ts)
plot(wb_ph_ts)
plot(wb_conductivity_ts)
plot(wb_bod_ts)
plot(wb_nitrate_ts)
plot(wb_fecal_ts)

fit_wb_temp_ts <- HoltWinters(wb_temp_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_temp_ts, 3)
plot(forecast(fit_wb_temp_ts,3))

fit_wb_ph_ts <- HoltWinters(wb_ph_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_ph_ts, 3)
plot(forecast(fit_wb_ph_ts,3))

fit_wb_conductivity_ts <- HoltWinters(wb_conductivity_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_conductivity_ts, 3)
plot(forecast(fit_wb_conductivity_ts,3))

fit_wb_bod_ts <- HoltWinters(wb_bod_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_bod_ts, 3)
plot(forecast(fit_wb_bod_ts,3))

fit_wb_nitrate_ts <- HoltWinters(wb_nitrate_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_nitrate_ts, 3)
plot(forecast(fit_wb_nitrate_ts,3))

fit_wb_fecal_ts <- HoltWinters(wb_fecal_ts, beta=FALSE, gamma=FALSE)
forecast(fit_wb_fecal_ts, 3)
plot(forecast(fit_wb_fecal_ts,3))
