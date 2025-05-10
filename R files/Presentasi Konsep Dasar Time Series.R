# Install library yang akan digunakan
# install.packages(c("TSA", "tseries", "lmtest", "forecast", "tidyverse", "rmarkdown"))
library(TSA)
library(tseries)
library(lmtest)
library(forecast)
library(tidyverse)
library(rmarkdown)
library(readxl)

# 1. Mengecek stasionaritas dan cara mengatasi data tidak stasioner
# Load dan proses data
data_finansial <- read_csv("C:/Kuliah/Sains Data Sem 3/Statistika Inferensial Lanjut Praktik/Tugas/14. Laporan Praktikum Konsep Dasar Pemodelan Time Series/financial_portfolio_data.csv")
data <- data_finansial
# Melihat data
view(data)
print(data, n=100)

# Menfilter hanya stock A
data$Price <- as.numeric(data$Price)
data <- data %>%
  filter(Asset == 'Stock A')

# Mengurutkan data sesuai tanggal
data <- data[order(data$Date), ]
print(data, n = 'All')

# Membuat time series object
data_ts <- ts(data$Price, start = c(2023, 2) , frequency=365)
data_frame <- as.data.frame(data_ts)


# Visualisasi dari data kita
plot(data_frame, data_frame$Price, xlab ="Date", ylab = "Price", main = "Plot Time Series")

# Pemeriksaaan Kestasioneran
# Adf dari data
adf.test(data_ts)

# Fungsi untuk melakukan differencing dan uji ADF Apabila tidak stasioner 
fix_stationarity <- function(asset_name, data) {
  # Filter data berdasarkan aset
  filtered_data <- data %>%
    filter(Asset == asset_name)
  
  # Pastikan kolom Date berupa tanggal dan urutkan berdasarkan tanggal
  filtered_data$Date <- as.Date(filtered_data$Date)
  filtered_data <- filtered_data[order(filtered_data$Date), ]
  
  # Konversi data ke time series dengan frekuensi bulanan (12)
  ts_data <- ts(filtered_data$Price, frequency = 12)
  
  # Plot time series untuk aset tersebut
  ggplot(filtered_data, aes(x = Date, y = Price)) +
    geom_line(color = "blue") + 
    labs(title = paste("Time Series for", asset_name), x = "Date", y = "Price") + 
    theme_minimal() 
  
  # Lakukan differencing pertama
  ts_data_diff <- diff(ts_data, differences = 1)
  
  # Plot data setelah differencing
  plot(ts_data_diff, main = paste("Differenced Time Series for", asset_name))
  
  # Lakukan uji ADF pada data setelah differencing
  adf_test <- adf.test(ts_data_diff)
  
  # Cetak hasil uji ADF
  print(paste("ADF Test for", asset_name, "after differencing:"))
  print(adf_test)
  # return(ts_data_diff)
}

# Menfilter hanya Bond A
data_tk_stat <- data_finansial %>%
  filter(Asset == 'Bond A')
data_tk_stat$Price <- as.numeric(data_tk_stat$Price)

# Mengurutkan data sesuai tanggal
data_tk_stat <- data_tk_stat[order(data_tk_stat$Date), ]
data_tk_stat

# Membuat time series object
data_ts_tk_stat <- ts(data_tk_stat$Price, start = c(2023, 3) , frequency=365)
data_frame_tk_stat <- as.data.frame(data_ts_tk_stat)

# Pemeriksaaan Kestasioneran
# Adf dari data Bond A
adf.test(data_ts_tk_stat)

# Analisis untuk Bond A (asumsi data Bond A tidak stasioner)
fix_stationarity("Bond A", data_finansial)

## diferensi data yg tdak stasioner
ts_diff_tk_stat <- diff(data_ts_tk_stat)

# uji ADF pd dta diferensi
adf_test_diff <- adf.test(ts_diff_tk_stat)
print(adf_test_diff)

# # ALTERNATIVE ADF
# # Menggunakan kpss test
# kpss.test(ts_diff_tk_stat)
# kpss.test(diff_ts)
# 
# # Misalnya, data Anda bernama data
# kpss_result_stocka <- kpss.test(diff_ts, null = "Level")  # null bisa 'Level' atau 'Trend'
# kpss_result_bonda <- kpss.test(diff_ts, null = "Level")  # null bisa 'Level' atau 'Trend'
# 
# print(kpss_result_stocka)
# print(kpss_result_stockb)


# 2. ACF
# Acf dari data
acf(data_ts, lag=100)
# Pacf dari data (ini akan digunakan di model ar dimana kita akan menggunakan yang garisnya melebihi garis white noise)
pacf(data_ts, lag=100)

# 3. Whitenoise
# Membuat model menggunakan pacf
m1=arima(data_ts, order=c(6,0,0), method="ML")

# Ngecek lag mana yang signifikan
lmtest::coeftest(m1)

# Check residual bila residual menyerupai white noise, maka model sudah bagus
checkresiduals(m1)

# Memprediksi menggunakan model yang sudah kita buat
ramalan<-forecast::forecast(data_ts, model=m1, h=10)
ramalan

# Ngeplot ramalan kita
plot(ramalan)

