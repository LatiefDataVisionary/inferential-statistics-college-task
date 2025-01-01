# install.packages("forecast","tseries","zoo","lmtest")
library(forecast)
library(tseries)
library(zoo)
library(lmtest)

#CONTOH AR(1)
#ACF & PACF Teoritis Untuk AR(1)
ACF.AR1 = ARMAacf(ar=0.8, ma=0, 20)
PACF.AR1 = ARMAacf(ar=0.8, ma=0, 20, pacf = TRUE)
ACF.AR1 = ACF.AR1[2:21]
c1 = ACF.AR1
c2 = PACF.AR1
AR1 = cbind(c1,c2)
AR1 


par(mfrow = c(1,2))
plot(ACF.AR1, type = "h", xlab = "lag", ylim = c(-1, 1))
abline(h = 0)
plot(PACF.AR1, type = "h", xlab = "lag", ylim = c(-1,1))
abline(h = 0)


#Model AR(1) dengan Data Sunspots
#Memasukkan data
year <- c(1755:1831)
sunspots <- c(9.6, 10.2, 32.4, 47.6, 54, 62.9, 85.9, 61.2, 45.1, 36.4, 
              20.9, 11.4, 37.8, 69.8, 106.1, 100.8, 81.6, 66.5, 34.8, 
              30.6, 7, 19.8, 92.5, 154.4, 125.9, 84.8, 68.1, 38.5, 22.8, 
              10.2, 24.1, 82.9, 132, 130.9, 118.1, 89.9, 66.6, 6, 46.9, 
              41, 21.3, 16, 6.4, 4.1, 6.8, 14.5, 34, 45, 43.1, 47.5, 
              42.2, 28.1, 10.1, 8.1, 2.5, NA, 1.4, 5, 12.2, 13.9, 35.4, 
              45.8, 41.1, 30.1, 23.9, 15.6, 6.6, 4, 1.8, 8.5, 16.6, 
              36.3, 49.6, 64.2, 67, 70.9, 47.8)

sunspots_filled <- zoo::na.approx(sunspots)

sunspots_ts <- ts(sunspots_filled, start = 1755, end = 1831, frequency = 1)
print(sunspots_ts)

#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
ar_model <- arima(sunspots_ts, order = c(1, 0, 0))  # AR(p=1)
print("Model AR:")
summary(ar_model)

# Diagnostik Residual
# Untuk AR
cat("Diagnostik Residual untuk AR Model\n")
checkresiduals(ar_model)
accuracy(ar_model)

#Prediksi AR
forecast_ar <- forecast(ar_model, h = 10)  # Prediksi AR
plot(forecast_ar, main = "Forecast from AR(1,0,0)", col = "blue")





#CONTOH MA(2)
ACF.MA2 = ARMAacf(ar = 0, ma = c(1.5, -0.7), 20)
PACF.MA2 = ARMAacf(ar = 0, ma =c(1.5, -0.7), 20, pacf = TRUE)
ACF.MA2 = ACF.MA2[2:21]
c1 = ACF.MA2
c2 = PACF.MA2
MA2 = cbind(c1,c2)
MA2

par(mfrow = c(1,2))
plot(ACF.MA2, type = "h", xlab = "lag", ylim = c(-1,1))
abline(h = 0)
plot(PACF.MA2, type = "h", xlab = "lag", ylim = c(-1,1))
abline(h = 0)




#Model MA(1) dengan Data Sunspots
#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
ma_model <- arima(sunspots_ts, order = c(0, 0, 1))  # MA(q=1)
print("Model MA:")
summary(ma_model)

#Cek Residual
# Untuk MA(1)
cat("Diagnostik Residual untuk MA Model\n")
checkresiduals(ma_model)
accuracy(ar_model)

#Prediksi MA(1)
forecast_ma <- forecast(ma_model, h = 10)  # Prediksi MA
plot(forecast_ma, main = "Forecast MA Model", col = "red")






#CONTOH ARMA(1,1)
ACF.ARMA1 = ARMAacf(ar = 0.7, ma = 0.4, 10)
PACF.ARMA1 = ARMAacf(ar = 0.7, ma = 0.7, 10, pacf = TRUE)
ACF.ARMA1 = ACF.ARMA1[2:21]
c1 = ACF.ARMA1
c2 = PACF.ARMA1
ARMA1 = cbind(c1,c2)
ARMA1

par(mfrow = c(1,2))
plot(ACF.ARMA1, type = "h", xlab = "lag", ylim = c(-1,1))
abline(h = 0)
plot(PACF.ARMA1, type = "h", xlab = "lag", ylim = c(-1,1))
abline(h = 0)


#Model ARMA (1,1) dengan Data Sunspots
#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
arma_model <- arima(sunspots_ts, order = c(1, 0, 1))  # ARMA(p=1, q=1)
print("Model ARMA:")
summary(arma_model)
#Y
#Cek Residual
#Cek Residual dan Akurasi digunakan untuk menentukan apakah model yang di bentuk sudah cukup baik atau kurang baik.
cat("Diagnostik Residual untuk ARMA Model\n")
checkresiduals(arma_model)
accuracy(ar_model)

#Prediksi ARMA (1,1)
forecast_arma <- forecast(arma_model, h = 10)  # Prediksi ARMA
plot(forecast_arma, main = "Forecast ARMA Model", col = "green")






#CONTOH ARIMA STASIONER
library(readxl)
datacontoh <- read_excel("C:/Kuliah/Sains Data Sem 3/Statistika Inferensial Lanjut Praktik/Tugas/15. AR MA ARMA ARIMA/Data Contoh ARIMA.xlsx")
View(datacontoh)

#Mengubah data menjadi data Time Series
datacontoh <- ts(datacontoh$Zt)

#Menggambar grafik data contoh
par(mfrow = c(2,1))
plot(datacontoh, lwd = 2, main = "Data Contoh")
abline(h = mean(datacontoh), col = 'red')
acf(datacontoh, lag.max = 36)

#Uji Stasioner
library(tseries)
adf.test(datacontoh)

#Spesifikasi model
par(mfrow = c(2,1))
acf(datacontoh, lag.max = 36)
abline(h = 0)
pacf(datacontoh, lag.max = 36, pacf = TRUE)

library(lmtest)
AR1 <- arima(datacontoh, order = c(1,0,0))
MA1 <- arima(datacontoh, order = c(0,0,1))
ARMA1 <- arima(datacontoh, order = c(1,0,1))
#Menguji signifikansi model Koefisien
coeftest(AR1)
coeftest(MA1)
coeftest(ARMA1)

#Menentukan Model terbaik Berdasarkan AR1 dan MA1
library(forecast)
AR1 <- arima(datacontoh, order = c(1,0,0))
AR1
MA1 <- arima(datacontoh, order = c(0,0,1))
MA1
checkresiduals(AR1)
accuracy(AR1)

#Peramalan model
Prediksi = forecast(ts(datacontoh), model = AR1, h = 20)
Prediksi
plot(forecast_ar, main = "Forecast from ARIMA(1,0,0)", col = "blue")


#CONTOH MODEL ARIMA NON STASIONER
BJsales
plot(BJsales) #Plotting data BJsales

#Uji stasioner
library(tseries)
adf.test(BJsales)

#Proses Differencing karena Data Tidak Stasioner
diff1 = diff(BJsales)
diff1
par(mfrow = c(2,1)) #Plotting data hasil differencing
plot(diff1, lwd = 2, main = "Data Hasil Differencing 1")
abline(h = mean(diff1), lwd = 2, lty = 2, col = "blue")
acf(diff1, main = "ÄCF Data Hasil Diferensing 1", lag.max = 36)

#Uji stasioner untuk data diff1
adf.test(diff1)

#Proses diferensing ke 2
diff2 = diff(diff1)
diff2
par(mfrow = c(2,1))
plot(diff2, lwd = 2, main = "Data Hasil Diferensing 2")
abline(h = mean(diff2), lwd = 2, lty = 2, col = "red")
acf(diff2, main = "ACF Data Hasil Diferensing 2", lag.max = 36)

#Uji Stasioner untuk diff2
adf.test(diff2)

#Penentuan Model
par(mfrow = c(2,1))
acf(diff2, main = "ACF Data diff2", lag.max = 36)
pacf(diff2, main = "PACF Data diff2", lag.max = 36, pacf = TRUE)
library(lmtest)
ARIMA121 = arima(diff2, order = c(1,2,1))
ARIMA120 = arima(diff2, order = c(1,2,0))
ARIMA021 = arima(diff2, order = c(0,2,1))

#Uji Signifikansi Model (Koefisien)
coeftest(ARIMA121)
coeftest(ARIMA120)
coeftest(ARIMA021)

#Menentukan Model Terbaik
library(forecast)
ARIMA121 = arima(BJsales, order = c(1,2,1))
ARIMA121
ARIMA120 = arima(BJsales, order = c(1,2,0))
ARIMA120
ARIMA021 = arima(BJsales, order = c(0,2,1))
ARIMA021

#Diagnostik & Akurasi Model
checkresiduals(ARIMA021)
accuracy(ARIMA021)

#Peramalan model ARIMA 021 menggunakan Forecasting
forecast(BJsales, model = ARIMA021, h = 10)
plot(forecast(BJsales, model = ARIMA021, h = 10))
plot(forecast_ar, main = "Forecast from ARIMA(0,2,1)", col = "purple")






#LATIHAN SOALL
#Cara 1
year <- c(1755:1831)
sunspots <- c(9.6, 10.2, 32.4, 47.6, 54, 62.9, 85.9, 61.2, 45.1, 36.4, 
              20.9, 11.4, 37.8, 69.8, 106.1, 100.8, 81.6, 66.5, 34.8, 
              30.6, 7, 19.8, 92.5, 154.4, 125.9, 84.8, 68.1, 38.5, 22.8, 
              10.2, 24.1, 82.9, 132, 130.9, 118.1, 89.9, 66.6, 6, 46.9, 
              41, 21.3, 16, 6.4, 4.1, 6.8, 14.5, 34, 45, 43.1, 47.5, 
              42.2, 28.1, 10.1, 8.1, 2.5, NA, 1.4, 5, 12.2, 13.9, 35.4, 
              45.8, 41.1, 30.1, 23.9, 15.6, 6.6, 4, 1.8, 8.5, 16.6, 
              36.3, 49.6, 64.2, 67, 70.9, 47.8)

sunspots_filled <- zoo::na.approx(sunspots)

sunspots_ts <- ts(sunspots_filled, start = 1755, end = 1831, frequency = 1)
print(sunspots_ts)


#Cara 2
#NO 1
library(readxl)
Datasunspot <- read_excel("sunspots_1755_1831.xlsx")
View(Datasunspot)

# Periksa struktur data dan pastikan nama kolom benar
str(Datasunspot)
names(Datasunspot)

# Mengonversi data menjadi time series
Datasunspot <- ts(Datasunspot$Sunspots, start = min(Datasunspot$Year), frequency = 1)

# Pastikan data hanya berupa angka dan tidak ada nilai NA
Datasunspot <- na.omit(as.numeric(Datasunspot))

# Menggambarkan Grafik Data Time Series

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))  # Menyesuaikan margin

# Plot data time series
plot(Datasunspot, lwd = 2, main = "Data Sunspot (1755-1831)", xlab = "Yaear", ylab = "Sunspots")
abline(h = mean(Datasunspot), col = 'red', lwd = 2, lty = 2)  # Menambahkan garis rata-rata

# Plot Autocorrelation Function (ACF)
acf(Datasunspot, lag.max = 36, main = "ACF Data Sunspot")


sunspots_ts
  plot(sunspots_ts,
     type = "o", 
     col = "orange",
     xlab = "Year",
     ylab = "Number of Sunspots",
     main = "Sunspot Activity (1755-1831)",
     lwd = 2, 
     pch = 16)


#MENGUJI STASIIONER DATA SUNSPORTS
library(tseries)
adf.test(Datasunspot)




#NO 2 
#SPESIFIKASI MODEL
par(mfrow=c(2,1))
acf(Datasunspot, lag.max=36)
abline(h=0)
pacf(Datasunspot, lag.max=36, pacf=TRUE)

#Menguji Signifikansi Koefisien dari Model
library(lmtest)
AR2 <- arima(Datasunspot, order = c(2,0,0))
MA1 <- arima(Datasunspot, order = c(0,0,1))
ARMA1 <- arima(Datasunspot, order = c(2,0,1))
#Menguji Signifikansi Koefisien dari Model
coeftest(AR2)
coeftest(MA1)
coeftest(ARMA1)


#Menentukan Model Terbaik
library(forecast)
AR2 <- arima(Datasunspot, order = c(2,0,0))
AR2
MA1 <- arima(Datasunspot, order = c(0,0,1))
MA1

#syntack lain
auto.arima(Datasunspot, trace = TRUE)


#Diagnostik & Akurasi Model
checkresiduals(AR2)
accuracy(AR2)




#NO 3
#Peramalan
forecast_ar <- forecast(ar_model, h = 10)  
plot(forecast_ar, main = "Forecast from ARIMA(2,0,0)", col = "blue")


















































#Model AR(1) dengan Data Sunspots
#Memasukkan data
year <- c(1755:1831)
sunspots <- c(9.6, 10.2, 32.4, 47.6, 54, 62.9, 85.9, 61.2, 45.1, 36.4, 
              20.9, 11.4, 37.8, 69.8, 106.1, 100.8, 81.6, 66.5, 34.8, 
              30.6, 7, 19.8, 92.5, 154.4, 125.9, 84.8, 68.1, 38.5, 22.8, 
              10.2, 24.1, 82.9, 132, 130.9, 118.1, 89.9, 66.6, 6, 46.9, 
              41, 21.3, 16, 6.4, 4.1, 6.8, 14.5, 34, 45, 43.1, 47.5, 
              42.2, 28.1, 10.1, 8.1, 2.5, NA, 1.4, 5, 12.2, 13.9, 35.4, 
              45.8, 41.1, 30.1, 23.9, 15.6, 6.6, 4, 1.8, 8.5, 16.6, 
              36.3, 49.6, 64.2, 67, 70.9, 47.8)

sunspots_filled <- zoo::na.approx(sunspots)

sunspots_ts <- ts(sunspots_filled, start = 1755, end = 1831, frequency = 1)
print(sunspots_ts)

#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
ar_model <- arima(sunspots_ts, order = c(1, 0, 0))  # AR(p=1)
print("Model AR:")
summary(ar_model)

# Diagnostik Residual
# Untuk AR
cat("Diagnostik Residual untuk AR Model\n")
checkresiduals(ar_model)
accuracy(ar_model)

#Prediksi AR
forecast_ar <- forecast(ar_model, h = 10)  # Prediksi AR
plot(forecast_ar, main = "Forecast from AR(1,0,0)", col = "blue")


#Model MA(1) dengan Data Sunspots
#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
ma_model <- arima(sunspots_ts, order = c(0, 0, 1))  # MA(q=1)
print("Model MA:")
summary(ma_model)

#Cek Residual
# Untuk MA(1)
cat("Diagnostik Residual untuk MA Model\n")
checkresiduals(ma_model)
accuracy(ar_model)

#Prediksi MA(1)
forecast_ma <- forecast(ma_model, h = 10)  # Prediksi MA
plot(forecast_ma, main = "Forecast MA Model", col = "red")

#Model ARMA (1,1) dengan Data Sunspots
#Uji Stasioneritas
adf_test <- adf.test(sunspots_ts)
print(adf_test)

#AIC
arma_model <- arima(sunspots_ts, order = c(1, 0, 1))  # ARMA(p=1, q=1)
print("Model ARMA:")
summary(arma_model)

#Cek Residual
cat("Diagnostik Residual untuk ARMA Model\n")
checkresiduals(arma_model)
accuracy(ar_model)

#Prediksi ARMA (1,1)
forecast_arma <- forecast(arma_model, h = 10)  # Prediksi ARMA
plot(forecast_arma, main = "Forecast ARMA Model", col = "green")

