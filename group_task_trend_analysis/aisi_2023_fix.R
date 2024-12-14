# === 1. Memasukkan dan Memuat Data ===
# Memuat library untuk visualisasi
# install.packages(ggplot2)   # Untuk menginstal library ggplot2
library(ggplot2)

## Data penjualan AISI 2023
# Fungsi c() di R adalah singkatan dari combine. 
# Fungsi ini digunakan untuk membuat sebuah vektor dengan menggabungkan elemen-elemen.
bulan <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
penjualan_domestik <- c(615416, 575502, 633155, 354323, 529771, 493763, 475428, 534379, 509946, 516293, 571983, 427033)
penjualan_ekspor <- c(39269, 45627, 36962, 41201, 55743, 49920, 53443, 55247, 51318, 52517, 46020, 42737)

## Membuat data frame
# Tujuan: Membuat sebuah data frame yang bernama aisi_data, 
# yang berisi data penjualan domestik dan ekspor sepeda motor berdasarkan bulan.
aisi_data <- data.frame(
  Bulan = bulan,
  Penjualan_Domestik = penjualan_domestik,
  Penjualan_Ekspor = penjualan_ekspor
)
print(aisi_data)

# Konversi kolom 'Bulan' menjadi faktor dengan urutan sesuai kalender
aisi_data$Bulan <- factor(aisi_data$Bulan, 
                          levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

#=======================================================

# === 2. Visualisasi Data Awal (2 Kolom, 2 Grafik) ===
# Penjualan Domestik
plot_domestik <- ggplot(aisi_data, aes(x = Bulan, y = Penjualan_Domestik, group = 1)) +
  geom_line(color = "blue", size = 1) +  # utk membuat garis, warna biru ukurannya 1
  geom_point(color = "blue", size = 2) +  # Utk membuat titik, warna biru ukurannya 2 agar lebih tebal
  labs(                                   # Utk menambahkan label pada grafik, judul, x, y
    title = "Penjualan Domestik AISI 2023",
    x = "Bulan",
    y = "Jumlah Penjualan"
  ) +
  theme_minimal()

# theme_minimal(): Fungsi ini menerapkan tema minimalis pada grafik, 
# yang akan menghilangkan elemen desain yang tidak perlu 
# dan membuat grafik terlihat lebih bersih dan sederhana. 
# Ini membantu fokus pada data dan mengurangi gangguan visual.


# Penjualan Ekspor
plot_ekspor <- ggplot(aisi_data, aes(x = Bulan, y = Penjualan_Ekspor, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 2) +
  labs(
    title = "Penjualan Ekspor AISI 2023",
    x = "Bulan",
    y = "Jumlah Penjualan"
  ) +
  theme_minimal()

# Menampilkan grafik
print(plot_domestik)
print(plot_ekspor)

#=======================================================

# === 3. Tren Linear ===

## a. Metode Semi Rata-Rata

# Untuk Tren Negatif pada Kolom Domestik
semi_average_forecast_dom <- function(data, col_name) {
  n <- nrow(data)  # Menentukan jumlah total baris (data) dalam data frame data.
  print(paste("Total baris data (n):", n))
  
  half <- floor(n / 2)  # Membagi data menjadi dua bagian yang hampir sama, 
                        # dengan half sebagai jumlah baris pada bagian pertama.
  print(paste("Jumlah data setengah bagian pertama (half):", half))
  
  # Menghitung rata-rata nilai pada setengah pertama dari data (dari bulan 1 hingga tengah).
  mean_first <- mean(data[[col_name]][1:half]) 
  print(paste("Rata-rata setengah bagian pertama (mean_first):", mean_first))
  
  # Menghitung rata-rata nilai pada setengah kedua dari data (dari tengah hingga akhir).
  mean_second <- mean(data[[col_name]][(half + 1):n])
  print(paste("Rata-rata setengah bagian kedua (mean_second):", mean_second))
  
  # Menghitung kemiringan (slope) dari garis tren dengan mengukur perubahan rata-rata 
  #  antara dua setengah data dibagi dengan jumlah data pada masing-masing setengah.
  slope <- (mean_second - mean_first) / half
  print(paste("Kemiringan (slope):", slope))
  
  # Menghitung titik potong (intercept) dari garis tren menggunakan rumus regresi linier.
  intercept <- mean_first - slope * (half / 2)
  print(paste("Titik potong (intercept):", intercept))
  
  # Menentukan indeks bulan yang akan datang (JAN, FEB, MAR)
  future_indices <- 13:15
  print("Indeks bulan yang akan datang (future_indices):")
  
  # Menghitung nilai ramalan untuk bulan-bulan yang akan datang 
  # menggunakan persamaan garis tren (slope dan intercept).
  future_values <- intercept - slope * future_indices
  print("Nilai ramalan (future_values):")
  print(future_indices)
  
  # Print hasil perhitungan
  cat("\n\n")
  print(paste("Intercept (a):", intercept))
  print(paste("Slope (b):", slope))
  print(paste("Indeks bulan yang akan datang (X):", paste(future_indices, collapse = ", ")))
  print(paste("Nilai ramalan (Y):", paste(round(future_values, 0), collapse = ", ")))
  
  # Menampilkan persamaan lengkap
  print(paste("===Persamaan Semi Rata-Rata Kolom Domestik(Tren Negatif): Y = ", round(intercept, 2), " - ", round(slope, 2), " * X"))
  
  # Mengembalikan data frame yang berisi bulan (JAN, FEB, MAR) 
  # beserta nilai ramalan yang sudah dibulatkan.
  return(data.frame(Bulan = c("JAN", "FEB", "MAR"), Ramalan = round(future_values, 0)))
}

# Untuk Tren Positif pada Kolom Ekspor
semi_average_forecast_eks <- function(data, col_name) {
  n <- nrow(data)  # Menentukan jumlah total baris (data) dalam data frame data.
  print(paste("Total baris data (n):", n))
  
  half <- floor(n / 2)  # Membagi data menjadi dua bagian yang hampir sama, 
  # dengan half sebagai jumlah baris pada bagian pertama.
  print(paste("Jumlah data setengah bagian pertama (half):", half))
  
  # Menghitung rata-rata nilai pada setengah pertama dari data (dari bulan 1 hingga tengah).
  mean_first <- mean(data[[col_name]][1:half]) 
  print(paste("Rata-rata setengah bagian pertama (mean_first):", mean_first))
  
  # Menghitung rata-rata nilai pada setengah kedua dari data (dari tengah hingga akhir).
  mean_second <- mean(data[[col_name]][(half + 1):n])
  print(paste("Rata-rata setengah bagian kedua (mean_second):", mean_second))
  
  # Menghitung kemiringan (slope) dari garis tren dengan mengukur perubahan rata-rata 
  #  antara dua setengah data dibagi dengan jumlah data pada masing-masing setengah.
  slope <- (mean_second - mean_first) / half
  print(paste("Kemiringan (slope):", slope))
  
  # Menghitung titik potong (intercept) dari garis tren menggunakan rumus regresi linier.
  intercept <- mean_first + slope * (half / 2)
  print(paste("Titik potong (intercept):", intercept))
  
  # Menentukan indeks bulan yang akan datang (JAN, FEB, MAR)
  future_indices <- 13:15
  print("Indeks bulan yang akan datang (future_indices):")
  
  # Menghitung nilai ramalan untuk bulan-bulan yang akan datang 
  # menggunakan persamaan garis tren (slope dan intercept).
  future_values <- intercept - slope * future_indices
  print("Nilai ramalan (future_values):")
  print(future_indices)
  
  # Print hasil perhitungan
  cat("\n\n")
  print(paste("Intercept (a):", intercept))
  print(paste("Slope (b):", slope))
  print(paste("Indeks bulan yang akan datang (X):", paste(future_indices, collapse = ", ")))
  print(paste("Nilai ramalan (Y):", paste(round(future_values, 0), collapse = ", ")))
  
  # Menampilkan persamaan lengkap
  print(paste("===Persamaan Semi Rata-Rata Kolom Ekspor(Tren Positif): Y = ", round(intercept, 2), " - ", round(slope, 2), " * X"))
  
  # Mengembalikan data frame yang berisi bulan (JAN, FEB, MAR) 
  # beserta nilai ramalan yang sudah dibulatkan.
  return(data.frame(Bulan = c("JAN", "FEB", "MAR"), Ramalan = round(future_values, 0)))
}

cat("\n===Penjualan Domestik")
ramalan_semi_dom <- semi_average_forecast_dom(aisi_data, "Penjualan_Domestik")
cat("Hasil Ramalan Semi Rata-Rata (Domestik):\n")
print(ramalan_semi_dom)

cat("\n===Penjualan Ekspor")
ramalan_semi_eks <- semi_average_forecast_eks(aisi_data, "Penjualan_Ekspor")
cat("\nHasil Ramalan Semi Rata-Rata (Ekspor):\n")
print(ramalan_semi_eks)


# ----------------------------------------------------------------------

## b. Metode Kuadrat Terkecil (Linear Regression)

# Menambahkan kolom Index ke dataset
aisi_data$Index <- seq_len(nrow(aisi_data))  # Menambahkan urutan 1, 2, 3, ..., sesuai jumlah baris data

ramalan_linear <- function(data, col_name) {
  # Membuat model linear menggunakan fungsi lm()
  model <- lm(data[[col_name]] ~ Index, data = data)
  
  # Menampilkan ringkasan model
  print("Ringkasan Model Linear:")
  print(summary(model))
  
  # Menentukan indeks bulan yang akan datang (JAN, FEB, MAR)
  future_indices <- data.frame(Index = 13:15)
  print("Indeks bulan yang akan datang (future_indices):")
  print(future_indices)
  
  # Menghitung nilai ramalan untuk bulan-bulan yang akan datang
  future_values <- predict(model, newdata = future_indices)
  print("Nilai ramalan (future_values):")
  print(future_values)
  
  # Menampilkan hasil perhitungan
  cat("\n\n")
  print(paste("Model Linear untuk kolom", col_name, ":"))
  print(paste("Nilai ramalan (Y) untuk bulan JAN, FEB, MAR:", paste(round(future_values, 0), collapse = ", ")))
  
  # Menampilkan persamaan model linear (Y = a + bX)
  coef_intercept <- coef(model)[1]  # Intercept (a)
  coef_slope <- coef(model)[2]     # Slope (b)
  print(paste("Persamaan Linear Regression: Y = ", round(coef_intercept, 2), " + ", round(coef_slope, 2), " * X"))
  
  # Mengembalikan data frame yang berisi bulan (JAN, FEB, MAR) 
  # beserta nilai ramalan yang sudah dibulatkan.
  return(data.frame(Bulan = c("JAN", "FEB", "MAR"), Ramalan = round(future_values, 0)))
}

cat("\n===Penjualan Domestik")
ramalan_linear_dom <- linear_model_forecast(aisi_data, "Penjualan_Domestik")
cat("Hasil Ramalan Kuadrat Terkecil (Domestik):\n")
print(ramalan_linear_dom)

cat("\n===Penjualan Ekspor")
ramalan_linear_eks <- linear_model_forecast(aisi_data, "Penjualan_Ekspor")
cat("\nHasil Ramalan Kuadrat Terkecil (Ekspor):\n")
print(ramalan_linear_eks)


#=======================================================

# === 4. Tren Non-Linear ===

# Menambahkan kolom Index jika belum ada
if (!"Index" %in% colnames(aisi_data)) {
  aisi_data$Index <- 1:nrow(aisi_data)  # Mengisi kolom Index dari 1 hingga jumlah baris data
}

# Menambahkan variabel kuadrat ke dataset
aisi_data$Index_Squared <- aisi_data$Index^2

# Fungsi untuk model kuadratis dan prediksi nilai di masa depan
quadratic_model_forecast <- function(data, col_name) {
  # Membuat model regresi kuadratis
  model <- lm(data[[col_name]] ~ Index + Index_Squared, data = data)
  
  # Menampilkan persamaan model
  coef <- coef(model)
  persamaan <- paste("Y = ", round(coef[1], 2), 
                     "+", round(coef[2], 2), "* X", 
                     "+", round(coef[3], 2), "* X^2")
  cat("\nPersamaan Model Kuadratis:\n", persamaan, "\n")
  
  # Menentukan indeks bulan masa depan (JAN, FEB, MAR)
  future_indices <- data.frame(Index = 13:15, Index_Squared = (13:15)^2)
  
  # Membuat prediksi
  future_values <- predict(model, newdata = future_indices)
  
  # Menampilkan ringkasan hasil
  hasil <- data.frame(Bulan = c("JAN", "FEB", "MAR"), Ramalan = round(future_values, 0))
  return(hasil)
}

# Memprediksi penjualan domestik
cat("\n=== Penjualan Domestik (Metode Kuadratis) ===")
ramalan_quad_dom <- quadratic_model_forecast(aisi_data, "Penjualan_Domestik")
cat("\nHasil Ramalan Kuadratis (Domestik):\n")
print(ramalan_quad_dom)

# Memprediksi penjualan ekspor
cat("\n=== Penjualan Ekspor (Metode Kuadratis) ===")
ramalan_quad_eks <- quadratic_model_forecast(aisi_data, "Penjualan_Ekspor")
cat("\nHasil Ramalan Kuadratis (Ekspor):\n")
print(ramalan_quad_eks)






#----------------------------------------


## b. Metode Eksponensial
# Fungsi untuk metode eksponensial
exponential_model_forecast <- function(data, col_name) {
  # Menambahkan kolom log dari data penjualan
  data$log_penjualan <- log(data[[col_name]])
  
  # Membuat model regresi linier pada skala logaritmik
  model <- lm(log_penjualan ~ Index, data = data)
  
  # Menampilkan persamaan model
  coef <- coef(model)
  persamaan <- paste("ln(Y) = ", round(coef[1], 2), "+", round(coef[2], 2), "* X")
  cat("\nPersamaan Model Eksponensial:\n", persamaan, "\n")
  
  # Menentukan indeks bulan masa depan (JAN, FEB, MAR)
  future_indices <- data.frame(Index = 13:15)
  
  # Membuat prediksi log nilai masa depan
  future_log_values <- predict(model, newdata = future_indices)
  
  # Mengubah prediksi log menjadi nilai eksponensial
  future_values <- exp(future_log_values)
  
  # Menampilkan hasil perhitungan
  hasil <- data.frame(Bulan = c("JAN", "FEB", "MAR"), Ramalan = round(future_values, 0))
  return(hasil)
}

# Memprediksi penjualan domestik
cat("\n=== Penjualan Domestik (Metode Eksponensial) ===")
ramalan_exp_dom <- exponential_model_forecast(aisi_data, "Penjualan_Domestik")
cat("\nHasil Ramalan Eksponensial (Domestik):\n")
print(ramalan_exp_dom)

# Memprediksi penjualan ekspor
cat("\n=== Penjualan Ekspor (Metode Eksponensial) ===")
ramalan_exp_eks <- exponential_model_forecast(aisi_data, "Penjualan_Ekspor")
cat("\nHasil Ramalan Eksponensial (Ekspor):\n")
print(ramalan_exp_eks)





#=======================================================

# === 5. Tabel Hasil Keseluruhan ===
hasil_domestik <- cbind(
  Semi_Rata_Rata = ramalan_semi_dom$Ramalan,
  Kuadrat_Terkecil = ramalan_linear_dom$Ramalan,
  Kuadratis = ramalan_quad_dom$Ramalan,
  Eksponensial = ramalan_exp_dom$Ramalan
)
rownames(hasil_domestik) <- ramalan_semi_dom$Bulan

hasil_ekspor <- cbind(
  Semi_Rata_Rata = ramalan_semi_eks$Ramalan,
  Kuadrat_Terkecil = ramalan_linear_eks$Ramalan,
  Kuadratis = ramalan_quad_eks$Ramalan,
  Eksponensial = ramalan_exp_eks$Ramalan
)
rownames(hasil_ekspor) <- ramalan_semi_eks$Bulan

cat("\nTabel Hasil Penjualan Domestik (Keempat Metode):\n")
print(hasil_domestik)

cat("\nTabel Hasil Penjualan Ekspor (Keempat Metode):\n")
print(hasil_ekspor)



#=======================================================

# === 5. Tabel Hasil Keseluruhan ===
# Pastikan dataset ramalan sudah memiliki nama bulan
ramalan_semi_dom$Bulan <- c("JAN", "FEB", "MAR")
ramalan_semi_eks$Bulan <- c("JAN", "FEB", "MAR")
ramalan_linear_dom$Bulan <- c("JAN", "FEB", "MAR")
ramalan_linear_eks$Bulan <- c("JAN", "FEB", "MAR")
ramalan_quad_dom$Bulan <- c("JAN", "FEB", "MAR")
ramalan_quad_eks$Bulan <- c("JAN", "FEB", "MAR")
ramalan_exp_dom$Bulan <- c("JAN", "FEB", "MAR")
ramalan_exp_eks$Bulan <- c("JAN", "FEB", "MAR")


# Buat tabel hasil domestik dan ekspor
hasil_domestik <- cbind(
  Semi_Rata_Rata = ramalan_semi_dom$Ramalan,
  Kuadrat_Terkecil = ramalan_linear_dom$Ramalan,
  Kuadratis = ramalan_quad_dom$Ramalan,
  Eksponensial = ramalan_exp_dom$Ramalan
)
rownames(hasil_domestik) <- ramalan_semi_dom$Bulan

hasil_ekspor <- cbind(
  Semi_Rata_Rata = ramalan_semi_eks$Ramalan,
  Kuadrat_Terkecil = ramalan_linear_eks$Ramalan,
  Kuadratis = ramalan_quad_eks$Ramalan,
  Eksponensial = ramalan_exp_eks$Ramalan
)
rownames(hasil_ekspor) <- ramalan_semi_eks$Bulan


cat("\nTabel Hasil Penjualan Domestik (Keempat Metode):\n")
print(hasil_domestik)

cat("\nTabel Hasil Penjualan Ekspor (Keempat Metode):\n")
print(hasil_ekspor)



#=======================================================


# === 6. Menghitung Measure of Accuracy ===
# Fungsi untuk menghitung MAPE, MAD, dan MSD
measure_of_accuracy <- function(actual, predicted) {
  n <- length(actual)
  errors <- actual - predicted
  mape <- mean(abs(errors / actual)) * 100
  mad <- mean(abs(errors))
  msd <- mean(errors^2)
  return(list(MAPE = mape, MAD = mad, MSD = msd))
}

# Fungsi untuk menghitung accuracy untuk semua metode
calculate_accuracy <- function(data, ramalan, col_name) {
  actual <- data[[col_name]]
  mape <- mad <- msd <- numeric(length(ramalan))
  
  for (i in seq_along(ramalan)) {
    predicted <- ramalan[[i]]$Ramalan
    acc <- measure_of_accuracy(actual, predicted)
    mape[i] <- acc$MAPE
    mad[i] <- acc$MAD
    msd[i] <- acc$MSD
  }
  
  return(data.frame(
    Metode = c("Semi Rata-Rata", "Kuadrat Terkecil", "Kuadratis", "Eksponensial"),
    MAPE = round(mape, 2),
    MAD = round(mad, 2),
    MSD = round(msd, 2)
  ))
}

# List ramalan untuk masing-masing kolom
ramalan_domestik <- list(ramalan_semi_dom, ramalan_linear_dom, ramalan_quad_dom, ramalan_exp_dom)
ramalan_ekspor <- list(ramalan_semi_eks, ramalan_linear_eks, ramalan_quad_eks, ramalan_exp_eks)

# Menghitung accuracy untuk penjualan domestik
accuracy_domestik <- calculate_accuracy(aisi_data, ramalan_domestik, "Penjualan_Domestik")

# Menghitung accuracy untuk penjualan ekspor
accuracy_ekspor <- calculate_accuracy(aisi_data, ramalan_ekspor, "Penjualan_Ekspor")



# === 7. Membuat Tabel Hasil Keseluruhan ===

cat("\nMeasure of Accuracy untuk Penjualan Domestik:\n")
print(accuracy_domestik)

cat("\nMeasure of Accuracy untuk Penjualan Ekspor:\n")
print(accuracy_ekspor)


# ===Penjelasan Ouput Measure of Accuracy===
# A. Penjualan Domestik

# 1. Semi Rata-Rata

# MAPE (13.56%): Kesalahan rata-rata persentase dari metode ini adalah sekitar 13,56%. Artinya, prediksi metode ini memiliki penyimpangan sekitar 13,56% dari nilai aktual.
# MAD (69.724): Rata-rata kesalahan absolutnya adalah 69.724 unit, menunjukkan seberapa jauh prediksi metode ini dari nilai aktual.
# MSD (7.041.342.391): Nilai ini menunjukkan besar variansi atau penyebaran kesalahan. Makin kecil MSD, makin stabil prediksinya.
# 
# 2. Kuadrat Terkecil
#       
# MAPE (15.28%): Kesalahan rata-rata metode ini sedikit lebih tinggi dibandingkan Semi Rata-Rata, yaitu 15,28%.
# MAD (80.581,83): Rata-rata kesalahan absolut metode ini juga lebih besar, yang berarti prediksinya sedikit lebih jauh dari nilai aktual dibandingkan metode pertama.
# MSD (8.912.563.380): Nilai MSD lebih tinggi dari metode Semi Rata-Rata, menandakan kesalahan yang lebih besar dan lebih bervariasi.
# 
# 3. Kuadratis
#             
# MAPE (29.20%): Metode ini memiliki kesalahan rata-rata yang jauh lebih besar, yaitu 29,20%. Ini menunjukkan metode ini kurang akurat untuk tren penjualan domestik.
# MAD (153.386,63): Nilai kesalahan absolut rata-rata metode ini jauh lebih besar dibanding metode lain.
# MSD (35.281.428.755): Variansi kesalahan juga sangat tinggi, menunjukkan metode ini kurang tepat untuk data domestik.
# 
# 4. Eksponensial
#                   
# MAPE (14.94%): Kesalahan rata-rata metode ini lebih rendah dibanding Kuadrat Terkecil, tetapi sedikit lebih tinggi dibanding Semi Rata-Rata.
# MAD (78.523,83): Rata-rata kesalahan absolut metode ini berada di tengah-tengah antara Kuadrat Terkecil dan Semi Rata-Rata.
# MSD (8.535.678.442): Variansi kesalahan lebih kecil dibanding Kuadrat Terkecil, menunjukkan prediksi yang relatif lebih konsisten.
#                   
# 
# B. Penjualan Ekspor
#                   
# 1. Semi Rata-Rata
#                   
# MAPE (17.77%): Kesalahan rata-rata metode ini cukup signifikan, yaitu 17,77%.
# MAD (7.488,33): Rata-rata kesalahan absolut sebesar 7.488 unit, menunjukkan prediksi metode ini cukup dekat dengan nilai aktual.
# MSD (91.701.397): Variansi kesalahan cukup kecil dibanding metode Kuadratis, menandakan prediksi yang stabil.
# 
# 2. Kuadrat Terkecil
#                         
# MAPE (15.36%): Metode ini memiliki kesalahan rata-rata terendah, yaitu 15,36%, menjadikannya metode yang paling akurat untuk penjualan ekspor.
# MAD (6.504,33): Nilai MAD juga paling kecil, menunjukkan metode ini memiliki prediksi yang paling dekat dengan nilai aktual.
# MSD (67.356.005): Nilai MSD yang rendah memperkuat bahwa metode ini menghasilkan prediksi yang paling stabil.
# 
# 3. Kuadratis
#                               
# MAPE (54.39%): Kesalahan rata-rata metode ini sangat tinggi, menunjukkan prediksi yang sangat jauh dari nilai aktual.
# MAD (24.924,95): Rata-rata kesalahan absolut sangat besar dibanding metode lain.
# MSD (925.575.581): Variansi kesalahan juga sangat tinggi, menandakan metode ini tidak cocok untuk data penjualan ekspor.
# 
# 4. Eksponensial
#                               
# MAPE (15.74%): Kesalahan rata-rata metode ini sedikit lebih tinggi dibanding Kuadrat Terkecil.
# MAD (6.656,67): Rata-rata kesalahan absolut metode ini lebih besar dari Kuadrat Terkecil, tetapi tetap lebih kecil dibanding metode lainnya.
# MSD (71.049.142): Nilai MSD yang cukup rendah menunjukkan prediksi yang cukup konsisten.
# 
# 
# ===Kesimpulan===
# 
# Untuk penjualan domestik, metode Semi Rata-Rata memberikan prediksi yang paling akurat berdasarkan MAPE, MAD, dan MSD.
# Untuk penjualan ekspor, metode Kuadrat Terkecil adalah yang paling akurat karena memiliki MAPE, MAD, dan MSD terendah.
# 
# 
















# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# === Measure of Accuracy ===
# Fungsi untuk menghitung MAPE, MAD, dan MSD
calculate_accuracy <- function(actual, predicted) {
  # Menghitung Error
  error <- actual - predicted
  
  # MAPE
  mape <- mean(abs(error) / actual) * 100
  
  # MAD
  mad <- mean(abs(error))
  
  # MSD
  msd <- mean(error^2)
  
  # Mengembalikan hasil dalam bentuk list
  return(list(MAPE = mape, MAD = mad, MSD = msd))
}

# Contoh penerapan untuk data domestik dan ekspor
# Data aktual (actual) adalah data penjualan asli untuk Januari-Maret
actual_domestik <- aisi_data$Penjualan_Domestik[1:3]
actual_ekspor <- aisi_data$Penjualan_Ekspor[1:3]

# Data prediksi (predicted) adalah hasil dari masing-masing metode ramalan
# Misalnya menggunakan metode Semi Rata-Rata
predicted_semi_domestik <- ramalan_semi_dom$Ramalan
predicted_semi_ekspor <- ramalan_semi_eks$Ramalan

# Menghitung akurasi untuk Penjualan Domestik (Metode Semi Rata-Rata)
accuracy_semi_domestik <- calculate_accuracy(actual_domestik, predicted_semi_domestik)
cat("\nAkurasi Penjualan Domestik (Metode Semi Rata-Rata):\n")
print(accuracy_semi_domestik)

# Menghitung akurasi untuk Penjualan Ekspor (Metode Semi Rata-Rata)
accuracy_semi_ekspor <- calculate_accuracy(actual_ekspor, predicted_semi_ekspor)
cat("\nAkurasi Penjualan Ekspor (Metode Semi Rata-Rata):\n")
print(accuracy_semi_ekspor)

# Anda bisa mengganti predicted_semi_domestik dengan hasil dari metode lainnya
# seperti Kuadrat Terkecil, Kuadratis, atau Eksponensial

# Contoh untuk semua metode dapat dilakukan dengan iterasi sebagai berikut:
methods <- list(Semi_Rata_Rata = list(ramalan_semi_dom, ramalan_semi_eks),
                Kuadrat_Terkecil = list(ramalan_linear_dom, ramalan_linear_eks),
                Kuadratis = list(ramalan_quad_dom, ramalan_quad_eks),
                Eksponensial = list(ramalan_exp_dom, ramalan_exp_eks))

# Iterasi per metode untuk menghitung MAPE, MAD, MSD
for (method_name in names(methods)) {
  predicted_dom <- methods[[method_name]][[1]]$Ramalan
  predicted_eks <- methods[[method_name]][[2]]$Ramalan
  
  cat("\nAkurasi Penjualan Domestik (", method_name, "):\n", sep = "")
  print(calculate_accuracy(actual_domestik, predicted_dom))
  
  cat("\nAkurasi Penjualan Ekspor (", method_name, "):\n", sep = "")
  print(calculate_accuracy(actual_ekspor, predicted_eks))
}

