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
model_students <- lm(Students ~ Year, data = students_data)
summary(model_students)

# Menambahkan prediksi dari model regresi linier ke data
students_data$Linear <- predict(model_students, newdata = students_data)

# Membuat data prediksi untuk 5 tahun ke depan
future_years <- data.frame(Year = 1989:1993)

# Prediksi regresi linier
future_years$Linear <- predict(model_students, newdata = future_years)

# 4. Metode Semiaverage
semiaverage <- function(data) {
  n <- nrow(data)
  mid <- floor(n / 2)
  
  group1 <- mean(data$Students[1:mid])
  group2 <- mean(data$Students[(mid + 1):n])
  
  years <- c(mean(data$Year[1:mid]), mean(data$Year[(mid + 1):n]))
  averages <- c(group1, group2)
  
  model <- lm(averages ~ years)
  pred <- predict(model, newdata = data.frame(years = data$Year))
  return(list(pred = pred, model = model))
}

semi_model <- semiaverage(students_data)
students_data$Semiaverage <- semi_model$pred
future_years$Semiaverage <- predict(semi_model$model, newdata = data.frame(years = future_years$Year))

# 5. Metode Least Squares (Linear)
least_squares <- function(data) {
  model <- lm(Students ~ Year, data = data)
  pred <- predict(model, newdata = data)
  return(list(pred = pred, model = model))
}

least_model <- least_squares(students_data)
students_data$LeastSquares <- least_model$pred
future_years$LeastSquares <- predict(least_model$model, newdata = future_years)

# 6. Metode Quadratic
quadratic <- function(data) {
  model <- lm(Students ~ poly(Year, 2, raw = TRUE), data = data)
  pred <- predict(model, newdata = data)
  return(list(pred = pred, model = model))
}

quad_model <- quadratic(students_data)
students_data$Quadratic <- quad_model$pred
future_years$Quadratic <- predict(quad_model$model, newdata = future_years)

# 7. Metode Exponential
exponential <- function(data) {
  log_model <- lm(log(Students) ~ Year, data = data)
  pred <- exp(predict(log_model, newdata = data))
  return(list(pred = pred, model = log_model))
}

exp_model <- exponential(students_data)
students_data$Exponential <- exp_model$pred
future_years$Exponential <- exp(predict(exp_model$model, newdata = future_years))

# 8. Visualisasi Perbandingan Metode
# Menambahkan data asli dan prediksi ke dalam satu data frame untuk visualisasi
visualization_data <- rbind(
  data.frame(Year = students_data$Year, Value = students_data$Students, Method = "Data Asli"),
  data.frame(Year = future_years$Year, Value = future_years$Linear, Method = "Regresi Linear"),
  data.frame(Year = future_years$Year, Value = future_years$Semiaverage, Method = "Semiaverage"),
  data.frame(Year = future_years$Year, Value = future_years$LeastSquares, Method = "Least Squares(Kuadrat Terkecil)"),
  data.frame(Year = future_years$Year, Value = future_years$Quadratic, Method = "Quadratic(Kuadratis)"),
  data.frame(Year = future_years$Year, Value = future_years$Exponential, Method = "Exponential")
)

# Membuat plot
ggplot() +
  geom_line(data = students_data, aes(x = Year, y = Students), color = "black", size = 1.2, linetype = "solid") +
  geom_line(data = visualization_data, aes(x = Year, y = Value, color = Method), size = 1) +
  geom_point(data = students_data, aes(x = Year, y = Students), color = "black", size = 2) +
  labs(
    title = "Perbandingan Tren Metode dan Data Asli",
    x = "Tahun",
    y = "Jumlah Mahasiswa",
    color = "Metode"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Regresi Linear" = "red",
    "Semiaverage" = "blue",
    "Least Squares(Kuadrat Terkecil)" = "green",
    "Quadratic(Kuadratis)" = "purple",
    "Exponential" = "orange"
  ))

# 9. Tabel Perbandingan Hasil Prediksi untuk 5 Tahun ke Depan
print(future_years)






