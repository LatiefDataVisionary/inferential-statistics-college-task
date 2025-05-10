#VARIANSI TREN 
#cnth data dari drive
install.packages(readx1)
library(readxl)

View(Penjualan_Rumah_di_US)
plot.ts(Penjualan_Rumah_di_US$Shipments, xlab="Year", ylab="Shipments", main="Tren Pembangunan Rumah di USA")
points(Penjualan_Rumah_di_US)



#klpk2
#data variansi tren hal.18
year <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
          2002,2003,2004,2005,2006,2007,2008,2009)
shipments <- c(188.172, 170.713, 210.787, 254.276, 303.932, 339.601, 363.411, 353.377, 
               372.843, 348.671, 250.55, 193.229,	168.491, 130.815, 130.748, 146.8, 117.3, 95.7, 81.9, 49.8)

#membuat data frame
data <- data.frame(Year = year, Shipments = shipments)
print(data)
variance_shipments <-var(data$Shipments)
print(paste("variansi Shipments:", variance_shipments))

#menentukan tren
trend_model <- lm(shipments ~ year, data = data)
summary(trend_model)

#Plot data
plot(data$Year, data$Shipments, type="b", col="blue", pch=16,xlab="Year", ylab="Shipments", main="Trend of Shipments")

#garis tren
abline(trend_model, col="red", lwd=2)

# Smoothing menggunakan LOESS
loess_model <- loess(Shipments ~ Year, data = data)
lines(data$Year, predict(loess_model), col="green", lwd=2)
library(forecast)

# Konversi ke time series
ts_shipments <- ts(data$Shipments, start = c(1990), frequency = 2)
decomposed <- decompose(ts_shipments, type = "additive")

#Plot dekomposisi
plot(decomposed)

#variansi siklis 
View(mock_kaggle)
plot.ts(mock_kaggle$preco, xlab="preco", ylab="data", main="Grafik Time Series Variansi Siklis")
points(mock_kaggle)


#variasi musim
View(mock_kaggle)
plot.ts(mock_kaggle$estoque, xlab="Time", ylab="Estoque", main="Data Penjualan Estoque")
points(mock_kaggle)

#data(contoh 2)
tahun <- c(2001, 2002, 2003, 2004, 2005, 2006)
pemakaian_minyak <- c(54632, 54797, 56282, 59484, 59330, 57139)

#membuat data frame
data <- data.frame(tahun = tahun, pemakaian_minyak = pemakaian_minyak)
print(data)
variance_minyak <- var(data$pemakaian_minyak)
print(paste("Variansi Pemakaian Minyak Bumi:", variance_minyak))
trend_model <- lm(pemakaian_minyak ~ tahun, data = data)
summary(trend_model)

#Plot data
plot(data$tahun, data$pemakaian_minyak, type="b", col="blue", pch=16, xlab="Tahun", ylab="Pemakaian Minyak Bumi (Ton)", main="Tren Pemakaian Minyak Bumi")

#garis tren
abline(trend_model, col="red", lwd=2)

#Prediksi 3 tahun mendatang
tahun_baru <- data.frame(tahun = c(2007, 2008, 2009))
prediksi <- predict(trend_model, newdata = tahun_baru)
print(data.frame(tahun_baru, predik_pemakaian_minyak = prediksi))








#variansi tak beraturan
# Membuat data frame
data <- data.frame(Tahun = 2013:2022, Produksi = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190))

# Visualisasi
library(ggplot2)
ggplot(data, aes(x = Tahun, y = Produksi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Tahun", y = "Produksi Padi")











#SMA CONTOH 1 (DENGAN LIBRARY ZOO)
#memuat library yang diperlukan
install.packages("zoo")
library(zoo) 

#data harga aset
harga <- c (20, 24, 22, 21, 20, 18, 17, 22, 26, 30, 31, 34, 33, 30, 28)
periode <- c(1, 2, 3, 4, 5)

#Menghitung SMA
SMA_harian <- rollapply(harga, width = 5, FUN = mean, align = 'right', fill = NA)
SMA_harian                 

#membuat data frame untuk menampilkan hasil
hasil_SMA <- data.frame(hari = 1: length(harga), harga, SMA = SMA_harian)
print(hasil_SMA)


#plot hasil harga aset dan sma
plot(1:length(harga), harga, type = "l", col = "blue", xlab = "Hari", ylab = "Harga Aset", main = "Harga Aset dan SMA 5 Hari")
lines(1:length(SMA_harian), SMA_harian, col = "red", lwd = 2) #untuk menambahkan sma ke plot     
legend("topleft", legend = c("Harga Aset", "SMA 5 Hari"), col = c("blue", "red"), lty = 1, lwd = 2)





#SMA CONTOH 2(CARA SEDERHANA)
#Menulis Dataset yang akan digunakan
permintaan_aktual<- c(79,82,95,82,85,88,93,84,90,83,76,87)
permintaan_aktual

#melihat grafik time series dari permintaan aktual
plot.ts(permintaan_aktual, main="Grafik Time Series Permintaan Aktual")

#Menentukan nilai SMA
SMA.4 <- SMA(permintaan_aktual, 4)
SMA.4

#Grafik SMA
plot.ts(SMA.4, main="Grafik SMA 4 bulanan")





installed.packages()




#WMA CONTOH 1(CONTOH LIBRARY TTR)
data_asset <- c(20,24,22,21,20,18,17,22,26,30,31,34,33,30,28)
plot.ts(data_asset, main="Grafik time series data aset")
install.packages(TTR)
library(TTR)

WMA.5 <- WMA(data_asset, 5, 1:5)
WMA.5
plot.ts(WMA.5, main="Grafik weighted moving average 5 harian")


#WMA CONTOH 2
#Menentukan Nilai WMA
WMA.4 <- WMA(permintaan_aktual, n=4, 1:4)
WMA.4

#Grafik WMA
plot.ts(WMA.4, main="Grafik WMA 4 bulanan")
















#EMA CONTOH 1
#menghitung EMA
#fungsi menghitung EMA

calculate_ema <- function(harga, n = 5) {
  alpha <- 2 / (n + 1)  # Smoothing factor
  ema <- rep(NA, length(harga))  # Inisialisasi dengan NA untuk semua bulan
  # EMA pertama dihitung dari permintaan pertama
  ema[1] <- harga[1]
  
  # Mulai menghitung EMA dari bulan ke-2 sampai seterusnya
  for (i in 2:length(harga)) {
    ema[i] <- alpha * harga[i] + (1 - alpha) * ema[i - 1]
  }
  
  return(ema)
}

harga <- c (20, 24, 22, 21, 20, 18, 17, 22, 26, 30, 31, 34, 33, 30, 28)

#menghitung EMA dengan periode 5 hari
EMA_hari<- calculate_ema (harga, n = 5 )
EMA_hari[1:4] <- NA
print(EMA_hari)

#membuat data frame untuk menampilkan hasil
hasil_EMA <- data.frame(hari = 1: length(harga), harga, EMA = EMA_hari)
print(hasil_EMA)

plot(1:length(harga), harga, type = "l", col = "blue", xlab = "Hari",
     ylab = "Harga Aset", main = "Harga Aset dan EMA 5 Hari")
lines(1:length(EMA_hari), EMA_hari, col = "red", lwd = 2) #untuk menambahkan ema ke plot     
legend("topleft", legend = c("Harga Aset", "EMA 5 Hari"), col = c("blue", "red"), lty = 1, lwd = 2)





#EMA CONTOH 2

calculate_ema <- function(permintaan, n = 4) {
  alpha <- 2 / (n + 1)  # Smoothing factor
  ema <- rep(NA, length(permintaan))  # Inisialisasi dengan NA untuk semua bulan
  # EMA pertama dihitung dari permintaan pertama
  ema[1] <- permintaan[1]
  
  # Mulai menghitung EMA dari bulan ke-2 sampai seterusnya
  for (i in 2:length(permintaan)) {
    ema[i] <- alpha * permintaan[i] + (1 - alpha) * ema[i - 1]
  }
  
  return(ema)
}

permintaan <- c(79, 82, 95, 82, 85, 88, 93, 84, 90, 83, 76, 87) 


#menghitung EMA dengan periode 5 hari
EMA_bulan<- calculate_ema (permintaan, n = 4 )
EMA_bulan[1:3] <- NA
print(EMA_bulan)

#membuat data frame untuk menampilkan hasil
hasil_EMA <- data.frame(bulan = 1: length(permintaan), permintaan, EMA = EMA_bulan)
print(hasil_EMA)


plot(1:length(permintaan), permintaan, type = "l", col = "blue", xlab = "Bulan", ylab = "Permintaan Aktual", main = "Permintaan Aktual dan EMA 4 Bulan", lwd = 2)
lines(1:length(EMA_bulan), EMA_bulan, col = "red", lwd = 2) #untuk menambahkan sma ke plot     
legend("topleft", legend = c("Permintaan Aktual", "WMA 4 Bulan"), col = c("blue", "red"), lty = 1, lwd = 2)














#LATIHAN hal.70
install.packages("lubridate")
library(lubridate)
data <- data.frame(Tanggal = seq(as.Date("2014-02-15"),
                                 as.Date("2014-02-24"), by = 'day'),
                   Harga = c(116, 87, 66, 44, 33, 13, 53, 72, 50, 15) )
library(TTR)
data$SMA_5 <- SMA (data$Harga, n = 5)
data$WMA_5 <- WMA (data$Harga, n=5, weights= 1:5)
data$EMA_5 <- EMA (data$Harga, n = 5)
print(data)

library(ggplot2)
ggplot(data, aes(x=Tanggal)) + geom_line(aes(y=Harga, colour = "Harga"))+
  geom_line(aes(y= SMA_5, colour = "SMA 5"))+ geom_line(aes(y=WMA_5, colour = "WMA 5"))+
  geom_line(aes(y=EMA_5, colour = "EMA"))


















#ANALISIS TREN
# Masukkan data ke dalam dataframe GENAP. hal 74
data_bawang <- data.frame(
  Tahun = 2001:2012,
  Ton = c(861150, 766572, 762795, 757399, 732609,
          794931, 802810, 853615, 965164, 1048934,
          893124, 964221)
)

# Visualisasi data
install.packages("ggplot2")
library(ggplot2)
print(data_bawang)
ggplot(data_bawang, aes(x = Tahun, y = Ton)) +
  geom_line() +
  labs(x = "Tahun", y = "Produksi Bawang Merah (Ton)",
       (title = "Grafik Produksi Bawang Merah di Indonesia"), x = "tahun", y = "ton")

# Memilih model (contoh menggunakan model linear)
model <- lm(Ton ~ Tahun, data = data_bawang)
summary(model)

# Peramalan untuk tahun 2014
prediksi_2013 <- predict(model, newdata = data.frame(Tahun = 2013))
print(prediksi_2013)

prediksi_2014 <- predict(model, newdata = data.frame(Tahun = 2014))
print(prediksi_2014)


#GANJIL.hal.80
# Membuat data frame
data <- data.frame(
  Tahun = c(2000:2012),
  Penjualan = c(772218, 861150, 766572, 762795, 757399, 732609, 794931, 802810, 853615, 965164, 1048934, 893124, 964221)
)

library(ggplot2)
print(data)
ggplot(data, aes(x = Tahun, y = Penjualan)) +
  geom_point() +
  geom_smooth(method = "lm")+
  ggtitle("penjualan bawang merah")+
  xlab("Tahun")+
  ylab("Penjuaan (Ton")
model<- lm (Penjualan ~ Tahun, data = data)
summary(model)

tahun_baru <- 2013:2014
prediksi<- predict(model, newdata = data.frame(Tahun=tahun_baru))
prediksi














#metode kuadrat terkecil hal.90-91
Penjualanbawang = c(772218, 861150, 766572, 762795, 757399, 732609, 794931, 802810, 853615, 965164, 1048934, 893124, 964221)
Penjualanbawang
x=-6:6
y=Penjualanbawang
fit=lm(y~x)
summary(fit)


#CONTOH GANJIL hal. 92
UnitMobil <- c(483.170, 533.922, 318.904, 434.473, 607.805, 486.061, 764.709, 894.180, 816.322)
UnitMobil
X <- -4:4
Y <- UnitMobil
fit <- lm(Y ~ X)
summary(fit)




