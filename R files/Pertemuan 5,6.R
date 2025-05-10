### Uji Median 
install.packages("agricolae")

## Contoh 1 PPT Pertemuan 5,6 Hal. 
# Menggunakan Uji Median
Penghasilan<-c(50,60,70,70,75,80,90,95,95,100,45,50,55,60,65,65,70,80,100) 
Kelompok<- c(rep("Petani", 10), rep("Nelayan", 9))
data<- data.frame(Penghasilan, Kelompok)
data

library(agricolae)
Median.test(data$Penghasilan, data$Kelompok)

# Menggunakan Uji Fisher
Penghasilan<- matrix(c(6, 4, 2, 7), nrow=2, dimnames=(list(c("Di atas Median", "Dibawah Median"), c("Petani", "Nelayan"))))
Penghasilan

fisher.test(Penghasilan, alternative="two.sided")

## Contoh 2 PPT Pertemuan 5,6 Hal. 12
# Menggunakan Uji Median
Skor<-c(83,94,91,96,91,89,85,92,92,90,91,90,85,80,91,90,84,83,83,84,81,88) 
Kelompok<- c(rep("Leaflet", 10), rep("video", 12))
data<- data.frame(Skor, Kelompok)
data

library(agricolae)
Median.test(data$Skor, data$Kelompok)

# Menggunakan Uji Chi-Square
Skor<- matrix(c(7, 3, 4, 8), nrow=2, dimnames=(list(c("Di atas Median", "Dibawah Median"), c("Leaflet", "Video"))))
Skor

chisq.test(Skor)


## Latihan 1 PPT Pertemuan 5,6 Hal. 18
# Menggunakan Uji Median
Produksi<- c(83, 91, 94, 89, 96, 91, 92, 90, 92, 85, 91, 90, 81, 83, 84, 83, 88, 91, 90, 84, 80, 85)
Kelompok<- c(rep("Metode_1", 10), rep("Metode_2", 12))
data<-data.frame(Produksi, Kelompok)
data

library(agricolae)
Median.test(data$Produksi, data$Kelompok)

# Menggunakan Uji Chi-Square
Skor<- matrix(c(7, 3, 4, 8), nrow=2, dimnames=(list(c("Di atas Median", "Dibawah Median"), c("Metode_1", "Metode_2"))))
Skor

chisq.test(Skor)


## Latihan 2 PPT Pertemuan 5,6 Hal. 19
# Menggunakan Uji Median
Waktu<- c(45.6, 49, 13.7, 37.9, 26.8, 30.6, 4, 35.0, 41.3, 32.5, 8.8, 17.4, 13.8, 26.3, 14.4, 20.1, 14, 42.3, 29.7, 17.8, 22.6, 15, 10.7, 21.5, 7, 11.2, 18, 27.9)
Kelompok<- c(rep("Normal", 15), rep("Penderita Sirosis Hati", 13))
data<- data.frame(Waktu, Kelompok)
data

library(agricolae)
Median.test(data$Waktu, data$Kelompok)

# Membuat tabel skor untuk Uji Chi-Square
Skor <- matrix(c(9, 6, 5, 8), nrow=2, dimnames=list(c("Di atas Median", "Di bawah Median"), c("Normal", "Sirosis")))
Skor

chisq.test(Skor)





### Uji Mann-Whitney (U test)

## Contoh 1 PPT Pertemuan 5,6 Hal. 26
BankFavorit<- c(16,18,10,12,16,14,15,10,12,15,16,11,0,0,0) 
BankTidakFavorit<- c(19,19,21,25,26,27,23,27,19,19,25,27,23,19,29) 
data<- data.frame(BankFavorit, BankTidakFavorit) 
data

wilcox.test(BankFavorit, BankTidakFavorit, paired= FALSE, data=data)



## Contoh 2 PPT Pertemuan 5,6 Hal. 26
metodehebat <- c(50,60,80,30,70,80,90,60,99,78,89,67,87)
metodesuper <- c(52,59,78,29,72,81,88,63,98,79,87,65,86)
datal<- data.frame(metodehebat, metodesuper)
datal

wilcox.test(metodehebat, metodesuper, paired=FALSE, data=data1)


## Latihan Modul No. 2 Hal. 45 

#----------------------
tidak_dapat_bantuan <- c(60,70,70,50,60,60,70,70,50,60)
dapat_bantuan <- c(70,70,80,60,80,90,70,60,50,60,70,80,80,80,90)

Produktivitas <- c(tidak_dapat_bantuan, dapat_bantuan)
Kelompok <- c(rep("Tidak Dapat Bantuan", length(tidak_dapat_bantuan)), 
              rep("Dapat Bantuan", length(dapat_bantuan)))

data <- data.frame(Produktivitas, Kelompok)
print(data)

wilcox.test(Produktivitas ~ Kelompok, data = data, paired = FALSE)
wilcox.test(metodehebat, metodesuper, paired=FALSE, data=data1)



#-------------------------------
## Latihan Modul No. 2 Hal. 45 
Produktivitas<-c(60,70,70,50,60,60,70,70,50,60,70,70,80,60,80,90,70,60,50,60,70,80,80,80,90) 
Kelompok<- c(rep("tidak_dapat_bantuan", 10), rep("dapat_bantuan", 15))

data1 <- data.frame(Produktivitas, Kelompok)
data

wilcox.test(Produktivitas ~ Kelompok, data = data, paired = FALSE)
wilcox.test(Produktivitas, Kelompok, paired=TRUE, data=data1)

