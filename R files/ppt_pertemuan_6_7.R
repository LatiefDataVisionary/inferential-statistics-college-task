## Uji Hipotesis komparatif k sampel Dependen dan Independen

# 1. Uji Cochran (Q-test) (Dependen)
install.packages("nonpar")

Abatisasi<- c(0,0,0,1,0,1,1,1,0,0,1)
Menutup<- c(1,0,0,1,1,0,1,1,0,1,1)
Menguras<- c(0,1,0,1,1,0,1,1,1,0,1)
data<- data.frame(Abatisasi, Menutup, Menguras)
data

library(nonpar)
cochrans.q(data)


# 2. Uji Friedman (Anava Ranking Two Way ) (Dependen)
Coklat<- c(78,82,81,80,82,83,85,79,82,78)
Stroberi<- c(80,76,78,77,74,81,78,73,70,71)
Keju<- c(84,85,80,88,86,89,84,85,87,88)
Kelapa<- c(71,73,70,71,75,70,70,72,73,70)
data<- data.frame (Coklat, Stroberi, Keju, Kelapa)
data

friedman.test(data.matrix(data))


# 3. Uji Chi-Square k Sampel (Independen)
Kontrasepsi<- matrix(c(55, 33, 23, 60,60,36, 65, 81, 45, 12, 35, 80), nrow=3, 
                     dimnames=list(c("20-30", "30-40", "> 40"), 
                    c("Pil KB", "Suntik", "IUD", "Kontrasepsi Mantap")))
Kontrasepsi

chisq.test(Kontrasepsi)

 
# 4. Uji Median Extension (Perluasan Uji Median) (Independen)
Kunjungan <- c(0,0,1,1,2,3,3,4,5,7,0,1,1,1,2,2,2,3,4,5,6,0,0,1,1,2,2,3,4,5,5,6,7,8,2,
               2,2,2,3,4,5,5,6,9)
Pendidikan<- c(rep("SD", 10), rep("SMP", 11), rep("SMA", 13), rep ("PT", 10))
data<- data.frame (Kunjungan, Pendidikan)
data

# install.packages("agricolae")
library("agricolae")
Median.test(data$Kunjungan, data$Pendidikan)

# Atau
Kunjungan<- matrix(c(5,5,4,7,7,6,6,4), nrow=2, 
                   dimnames=list(c("Di Atas Rata-Rata", 
                  "DiBawah Rata-rata"), c("SD", "SMP", "SMA", "PT")))
Kunjungan

chisq.test(Kunjungan)


# 5. Anova One Way Kruskal-Wallis (Independen)
Skor<- c(340,340,356, 386, 386, 402, 402, 417, 433, 495, 557, 294, 325, 325, 340, 356, 371, 385, 402, 263, 309, 340, 356, 371, 371, 402, 417)
Perlakuan <- c(rep("Kontrol", 11), rep("LSD", 8), rep("UML", 8))
Serotonin<- data.frame(Skor, Perlakuan)
Serotonin

kruskal.test(Skor~Perlakuan, data = Serotonin)





