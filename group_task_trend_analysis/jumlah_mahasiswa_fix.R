# Memuat library untuk visualisasi
library(ggplot2)

# 1. Memasukkan Data
year <- c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988)
students <- c(590, 600, 685, 720, 631, 660, 503, 681, 948)

# Membuat data frame
students_data <- data.frame(
  Year = year,
  Students = students
)

print(students_data)

# 2. Visualisasi Tren Jumlah Mahasiswa
ggplot(students_data, aes(x = Year, y = Students, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Tren Jumlah Mahasiswa",
    x = "Tahun",
    y = "Jumlah Mahasiswa"
  ) +
  theme_minimal()

# 3. Analisis Tren dengan Regresi Linier
# Model regresi linier
model_students <- lm(Students ~ Year, data = students_data)
summary(model_students)

# 4. Prediksi Tren ke Depan
# Membuat data prediksi untuk 5 tahun ke depan
future_years <- data.frame(Year = 1989:1993)

# Prediksi jumlah mahasiswa
future_students <- predict(model_students, newdata = future_years)

# Menampilkan hasil prediksi
predictions <- data.frame(
  Year = 1989:1993,
  Predicted_Students = future_students
)

print(predictions)








# === 4. Analisis Tren dengan Metode Kuadratis ===
# Menambahkan kolom kuadrat dari Year
students_data$Year_Square <- students_data$Year^2

# Model regresi kuadratis
model_quadratic <- lm(Students ~ Year + Year_Square, data = students_data)
summary(model_quadratic)

# Prediksi jumlah mahasiswa (kuadratis)
future_years$Year_Square <- future_years$Year^2
future_students_quadratic <- predict(model_quadratic, newdata = future_years)

# Hasil prediksi kuadratis
predictions_quadratic <- data.frame(
  Year = 1989:1993,
  Predicted_Students = future_students_quadratic
)

print(predictions_quadratic)

# === 5. Analisis Tren dengan Metode Eksponensial ===
# Transformasi logaritma jumlah mahasiswa
students_data$Log_Students <- log(students_data$Students)

# Model regresi eksponensial
model_exponential <- lm(Log_Students ~ Year, data = students_data)
summary(model_exponential)

# Prediksi jumlah mahasiswa (eksponensial)
future_students_log <- predict(model_exponential, newdata = future_years)
future_students_exponential <- exp(future_students_log)

# Hasil prediksi eksponensial
predictions_exponential <- data.frame(
  Year = 1989:1993,
  Predicted_Students = future_students_exponential
)

print(predictions_exponential)

# === 6. Visualisasi Data dan Prediksi (Metode Eksponensial) ===
# Menyatukan data asli dan prediksi (eksponensial) dengan tipe yang berbeda
combined_data_exponential <- rbind(
  data.frame(Year = students_data$Year, Students = students_data$Students, Type = "Actual"),
  data.frame(Year = 1989:1993, Students = future_students_exponential, Type = "Prediction")
)

# Plot data aktual dan prediksi eksponensial
ggplot(combined_data_exponential, aes(x = Year, y = Students, color = Type, linetype = Type)) +
  geom_line(size = 1) +  # Garis data
  geom_point(size = 2) +  # Titik data
  scale_color_manual(values = c("Actual" = "blue", "Prediction" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Prediction" = "dashed")) +
  labs(
    title = "Tren Jumlah Mahasiswa (Metode Eksponensial)",
    x = "Tahun",
    y = "Jumlah Mahasiswa",
    color = "Tipe Data",
    linetype = "Tipe Data"
  ) +
  theme_minimal()



# === 6. Visualisasi Gabungan untuk Semua Metode ===

# Menyatukan data aktual dan prediksi dari ketiga metode dengan tipe berbeda
combined_data_all <- rbind(
  data.frame(Year = students_data$Year, Students = students_data$Students, Type = "Actual"),
  data.frame(Year = 1989:1993, Students = future_students_linear, Type = "Linear Prediction"),
  data.frame(Year = 1989:1993, Students = future_students_quadratic, Type = "Quadratic Prediction"),
  data.frame(Year = 1989:1993, Students = future_students_exponential, Type = "Exponential Prediction")
)

# Plot gabungan
ggplot(combined_data_all, aes(x = Year, y = Students, color = Type, linetype = Type)) +
  geom_line(size = 1) +  # Garis data
  geom_point(data = subset(combined_data_all, Type == "Actual"), size = 2) +  # Titik untuk data aktual
  scale_color_manual(values = c(
    "Actual" = "black",
    "Linear Prediction" = "blue",
    "Quadratic Prediction" = "green",
    "Exponential Prediction" = "red"
  )) +
  scale_linetype_manual(values = c(
    "Actual" = "solid",
    "Linear Prediction" = "dashed",
    "Quadratic Prediction" = "dotted",
    "Exponential Prediction" = "dotdash"
  )) +
  labs(
    title = "Tren Jumlah Mahasiswa (Gabungan Metode Prediksi)",
    x = "Tahun",
    y = "Jumlah Mahasiswa",
    color = "Tipe Data",
    linetype = "Tipe Data"
  ) +
  theme_minimal()




