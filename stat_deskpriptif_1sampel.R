### Uji Binomial

# PPT Hal. 11 Contoh 1
binom.test(10, 24, 0.5)

# PPT Hal. 17 Contoh 2
binom.test(7, 20, 0.5, "two.sided", 0.95)

# PPT Hal. 23 Contoh 3
binom.test(8, 30, 0.5, "two.sided", 0.95)

# PPT Hal. 29 Latihan 1
binom.test(4, 13, 0.5)

# PPT Hal. 30 Latihan 2
binom.test(5, 20, 0.5, alternative = "less")
binom.test(15, 20, 0.5, alternative = "greater")

# PPT Hal. 31 Latihan 3
binom.test(24, 30, 0.5, alternative = "greater")
binom.test(6, 30, 0.5, alternative = "less")

# PPT Hal. 32 Latihan 4
binom.test(3, 10, 0.5, "two.sided")

# PPT Hal. 33 Latihan 5
binom.test(4, 10, 0.5, "two.sided")


### Uji Chi Square

# PPT Hal. 37 Contoh 1
pengunjung<- matrix(c(64, 76, 60), dimnames=list(c("Lab A", "Lab B", "Lab C"), c("Jumlah")))
pengunjung
chisq.test(pengunjung)

# PPT Hal. 41 Contoh 2
hadiah<- matrix(c(183, 142, 175), dimnames=list(c("T-Shirt", "Anting-anting", "mug"), c("Jumlah")))
hadiah
chisq.test(hadiah)

# PPT Hal. 45 Latihan 6
pil_kepala_desa <- matrix(c(200, 100), 
                          dimnames = list(c("Pria", "Wanita"), 
                                          c("Jumlah")))
pil_kepala_desa 
chisq.test(pil_kepala_desa)

# PPT Hal. 46 Latihan 7
pilihan_cat <- matrix(c(1000, 900, 600, 500), 
                      dimnames = list(c("Biru", "Merah", "Putih", "Lainnya"), 
                                      c("Jumlah")))
pilihan_cat
chisq.test(pilihan_cat)

# PPT Hal. 47 Latihan 8
kemasan <- matrix(c(19, 9, 7, 5), 
                  dimnames = list(c("Kemasan 1", "Kemasan 2", "Kemasan 3", "Kemasan 4"), 
                                  c("Frekuensi (hari)")))
kemasan
chisq.test(kemasan)



### Uji Runs

skor<- factor(c("+", "-", "+", "+", "+", "+", "-", "+", "+", "+", "-", "-", "-", "-", "+", "-", "-", "+", "+", "-", "-", "-", "-"))
skor

install.packages("tseries")

library(tseries)
runs.test(skor)



# Latihan modul hal 19 No. 4
list_skor_petani<-list(45, 65, 35, 70, 60, 50, 30, 90, 85, 90, 40, 80, 55, 30, 75)
median_value <- median(unlist(list_skor_petani))
print(median_value)

tanda <- sapply(unlist(list_skor_petani), function(x) {
  if (x <= median_value) {
    return("-")
  } else {
    return("+")
  }
})

tanda

skor_petani <- factor(c("-", "+", "-", "+", "-", "-", "+", "+", "+", "-", "+", "-", "-", "+"))
print(skor_petani)

library(tseries)
runs.test(skor_petani)



## PPT Hal. 64 Latihan 1
peserta_penyuluhan <- factor(c("P", "P", "W", "W", "P", "W", "P", "W", "P", "W", 
                               "P", "P", "P", "P", "W", "P", "W", "W", "W", "P", 
                               "W", "P", "W", "P", "W", "P", "P", "W", "P", "P", 
                               "W", "P", "P", "P", "P", "W", "P", "W", "P", "P"))
library(tseries)
runs_test <- runs.test(peserta_penyuluhan)
print(runs_test)


## PPT hal 65
skor_adop<-list(45, 65, 35, 70, 50, 30, 90, 85, 90, 40, 80, 55, 30, 75)
median_skor_adopsi<- median(unlist(skor_adop))

print(median_skor_adopsi)
tanda2 <- sapply(unlist(skor_adop), function(x) {
  if (x <= median_value) {
    return("-")
  } else {
    return("+")
  }
})

tanda2
plus_minus_skor_adopsi<- factor(c("-", "+", "-", "+", "-", "-", "+", "+", "+", "-", "+", "-", "-", "+"))
library(tseries)
runs.test(plus_minus_skor_adopsi)


## PPT Hal. 65 Latihan 2
total_skor <- c(45, 65, 35, 70, 60, 50, 30, 90, 85, 90, 40, 80, 55, 30, 75)
median_skor <- median(total_skor)

# Mengubah total skor menjadi tanda + dan -
# + untuk skor di atas atau sama dg median, - untuk skor di bawah median
adopsi_teknologi <- ifelse(total_skor >= median_skor, "+", "-")

# Mengubah mnjd faktor
adopsi_teknologi_factor <- factor(adopsi_teknologi)

# penggunaan library tseries untuk Uji Runs
library(tseries)

# Melakukan uji runs
runs_test <- runs.test(adopsi_teknologi_factor)

# Menampilkan hasil uji runs
print(runs_test)



## PPT Hal. 66 Latihan 3
# Data nilai ujian siswa
nilai_ujian <- c(65, 45, 49, 74, 80, 90, 64, 57, 68, 54, 76, 72, 64, 52, 90, 94, 58, 60, 58, 69, 79, 83, 66, 62, 82, 84, 52, 41, 62, 76)

# Menghitung median dari data nilai ujian
median_nilai <- median(nilai_ujian)

# Menampilkan median
print(paste("Median nilai ujian adalah:", median_nilai))

# Mengelompokkan nilai berdasarkan median
skor <- factor(ifelse(nilai_ujian > median_nilai, "+", "-"))
print(skor)

library(tseries)

# Melakukan uji runs
hasil_uji_runs <- runs.test(skor)
print(hasil_uji_runs)



## Modul Praktikum Hal. 19 Latihan 4
# Data total skor responden
total_skor <- c(45, 65, 35, 70, 60, 50, 30, 90, 85, 90, 40, 80, 55, 30, 75)

# Menghitung median menggunakan fungsi median()
median_skor <- median(total_skor)

# Menampilkan median
print(paste("Median total skor adalah:", median_skor))

# Menghapus nilai yang sama dengan median
total_skor_filtered <- total_skor[total_skor != median_skor]

# Mengelompokkan skor berdasarkan median
skor <- factor(ifelse(total_skor_filtered > median_skor, "+", "-"))
print(skor)

library(tseries)

# Melakukan uji runs
hasil_uji_runs <- runs.test(skor)
print(hasil_uji_runs)







