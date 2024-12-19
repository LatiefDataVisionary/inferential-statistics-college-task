# Memuat library untuk visualisasi
library(ggplot2)

# 1. Memasukkan dan memuat data
# Data penjualan mobil
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
units <- c(354331, 483170, 533922, 318904, 434473, 607805, 486061, 764709, 894180, 816322)

# Membuat data frame
car_sales_data <- data.frame(
  Year = year,
  Units = units
)

# Menampilkan data
print(car_sales_data)



# 2. Visualisasi data penjualan awal
# Memuat library untuk visualisasi
library(ggplot2)
ggplot(car_sales_data, aes(x = Year, y = Units)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Tren Penjualan Mobil di Indonesia (Data Awal)",
    x = "Tahun",
    y = "Unit Terjual"
  ) +
  scale_x_continuous(breaks = seq(2003, 2012, by = 1)) +  # Menampilkan semua tahun
  scale_y_continuous(labels = scales::comma) +            # Format angka dengan koma
  theme_minimal()



# 3. Metode Semi Average
# Membagi data menjadi dua periode
first_period <- car_sales_data[1:5, ]
first_period

second_period <- car_sales_data[6:10, ]
second_period

# Menghitung rata-rata penjualan tiap periode
avg_first <- mean(first_period$Units)
avg_first
avg_second <- mean(second_period$Units)
avg_second

# Tahun tengah untuk masing-masing periode
mid_first <- mean(first_period$Year)
mid_first
mid_second <- mean(second_period$Year)
mid_second

# Menentukan slope dan intercept regresi
slope <- (avg_second - avg_first) / (mid_second - mid_first)
slope 

intercept <- avg_first - slope * mid_first
intercept

# Fungsi prediksi berdasarkan metode semi average
predict_semi_average <- function(year) {
  intercept + slope * year
}
predict_semi_average

# Menambahkan prediksi ke data asli
car_sales_data$Predicted_Units <- predict_semi_average(car_sales_data$Year)

# Prediksi 5 tahun ke depan
future_years <- data.frame(Year = 2013:2017)
future_years$Predicted_Units <- predict_semi_average(future_years$Year)

# Menampilkan hasil prediksi
print(future_years)




# 4. Visualisasi Data Awal dan Prediksi
# Menggabungkan data awal dan data prediksi
combined_data <- rbind(
  car_sales_data[, c("Year", "Units", "Predicted_Units")],
  data.frame(Year = future_years$Year, Units = NA, Predicted_Units = future_years$Predicted_Units)
)

# Visualisasi gabungan data awal dan hasil prediksi
ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Units, color = "Data Awal"), size = 1) +
  geom_line(aes(y = Predicted_Units, color = "Hasil Prediksi"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Units, color = "Data Awal"), size = 2) +
  labs(
    title = "Tren Penjualan Mobil di Indonesia (Data Awal dan Prediksi)",
    x = "Tahun",
    y = "Unit Terjual",
    color = "Legenda"
  ) +
  scale_x_continuous(breaks = seq(2003, 2017, by = 1)) +  # Menampilkan semua tahun
  scale_y_continuous(labels = scales::comma) +            # Format angka dengan koma
  scale_color_manual(values = c("Data Awal" = "blue", "Hasil Prediksi" = "red")) +
  theme_minimal()


# 5. Menghitung Measure of Accuracy

# 1. Filter data yang memiliki nilai aktual (tidak NA)
actual_data <- car_sales_data[!is.na(car_sales_data$Units), ]

# 2. Hitung Error
actual_data$Error <- actual_data$Units - actual_data$Predicted_Units

# 3. Mean Absolute Error (MAE)
mae <- mean(abs(actual_data$Error))

# 4. Mean Squared Error (MSE)
mse <- mean(actual_data$Error^2)

# 5. Mean Absolute Percentage Error (MAPE)
mape <- mean(abs(actual_data$Error / actual_data$Units)) * 100

# Menampilkan hasil measure of accuracy
cat("Measure of Accuracy:\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")

