### Uji Mc Nemar

## Contoh 1 PPT Hal. 8

sebelum1<- rep(1,150)
sebelum2<- rep(2,50)
sebelum<- c(sebelum1, sebelum2)

setelah1<- c(rep(1,65), rep(2,85))
setelah2<- c(rep(1,10), rep(2,40))
setelah<- c(setelah1, setelah2)
rep()
data<- data.frame(sebelum, setelah)
table(data)

mcnemar.test(table(data))


## Contoh 2 PPT Hal. 12

sebelum1<- rep(1,20)
sebelum2<- rep(2,8)
sebelum<- c(sebelum1, sebelum2)

setelah1<- c(rep(2,14), rep(1,6))
setelah2<- c(rep(2,5), rep(1,3))
setelah<- c(setelah1, setelah2)

data<- data.frame(sebelum, setelah)
table(data)
mcnemar.test(table(data))


## Latihan 1 PPT Hal. 16
sebelum1 <- rep(1,12)
sebelum2 <- rep(2,9)
sebelum <- c(sebelum1, sebelum2)

setelah1 <- c(rep(1,3), rep(2,9))
setelah2 <- c(rep(1,4), rep(2,5))
setelah <- c(setelah1, setelah2)

data<- data.frame(sebelum, setelah)
table(data)
mcnemar.test(table(data))


## Latihan 2 PPT Hal. 21
sebelum1 <- rep(1, 20) 
sebelum2 <- rep(2, 10)  
sebelum <- c(sebelum1, sebelum2)

setelah1 <- c(rep(1, 9), rep(2, 14))  
setelah2 <- c(rep(1, 1), rep(2, 6))  
setelah <- c(setelah1, setelah2)

data <- data.frame(sebelum, setelah)
table(data)

mcnemar.test(table(data))


## Latihan 2 PPT Hal. 21
sebelum <- c(0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 
             0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1)

sesudah <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 
             0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0)

data <- data.frame(sebelum, sesudah)
table(data)

mcnemar.test(table(data))



### Uji Tanda/Sign Test
install.packages("BSDA")

## Contoh 1 PPT Hal. 27

istri<- c(4,5,4,4,5,4,4,2,1,4,3,2,4,4,3,2,4,1,5,5) 
suami<- c(1,4,5,5,4,3,3,1,3,5,2,3,1,2,2,1,5,2,4,4)
data<- data.frame(istri, suami, diff=suami-istri)
data

library(BSDA) 
SIGN.test(x=data$diff, alternative="less", conf.level=0.95)


## Contoh 2 PPT Hal. 35
sebelum <- c(5,4,3,4,4,3,3,4,4,3,4,3,3,2,4,3,3,5,2,2,3,3,4,4,3,4,4,4,3,2,4,4,5,5,4,3,2,3,2,3)
sesudah <- c(5,5,4,3,3,4,4,5,5,5,3,4,4,3,4,3,4,4,3,3,4,4,5,3,3,3,5,5,4,3,3,4,4,4,4,4,3,4,3,5)

diff <- sesudah - sebelum

data <- data.frame(Sebelum = sebelum, Sesudah = sesudah, Diff = diff)
data

library(BSDA)
SIGN.test(x = diff, alternative = "two.sided", conf.level = 0.95)


## Latihan 1 PPT Hal. 40

# Data beda
beda <- c(-2, 3, -2, -8, 4, 5, 8, -2, 2, -2, 4, 10, 10, 4)
library(BSDA)
SIGN.test(beda, alternative = "two.sided")


## Latihan 1 PPT Hal. 40
sendiri <- c(80, 82, 84, 86, 80, 84, 83, 81, 82, 80, 83, 84, 82, 81, 80, 79, 75)
kelompok <- c(78, 85, 82, 78, 84, 89, 83, 81, 90, 78, 85, 82, 86, 91, 80, 89, 79)

beda <- kelompok - sendiri

data <- data.frame(Sendiri = sendiri, Kelompok = kelompok, Beda = beda)
print(data)

data_analisis <- data[data$Beda != 0, ]

library(BSDA)
SIGN.test(x = data_analisis$Beda, alternative = "two.sided", conf.level = 0.95)



### Uji Wilcoxon

## Contoh 𝑛 < 25 PPT Hal. 46
sebelum<- c(76, 58, 62, 67, 66, 81, 85, 72, 71, 75) 
sesudah<- c(80, 60, 68, 72, 79, 80, 82, 80, 81, 79)
pelatihan<- data.frame(sebelum, sesudah)
pelatihan

wilcox.test(sebelum, sesudah, paired=TRUE, data=pelatihan)


## Contoh 𝒏 > 𝟐𝟓 PPT Hal. 51
pria <- c(8, 7, 8, 7, 7, 6, 9, 9, 5, 4, 9, 8, 7, 8, 6, 6, 5, 10, 10, 6, 5, 7, 7, 4, 5, 8, 10, 6, 8, 8)
wanita <- c(10, 7, 8, 6, 7, 6, 5, 5, 4, 3, 4, 5, 2, 5, 7, 5, 6, 5, 2, 4, 3, 4, 10, 6, 4, 4, 2, 4, 5, 9)

data_peserta <- data.frame(pria, wanita)
data_peserta

wilcox.test(pria, wanita, paired = TRUE, data=data_peserta)


## Latihan 1 PPT Hal. 57
sebelum <- c(20, 19, 23, 20, 18, 21, 20, 20, 19, 18, 23, 21, 20, 20, 23)
sesudah <- c(18, 17, 20, 21, 16, 19, 18, 22, 17, 15, 21, 20, 20, 19, 22)
kemampuan <- data.frame(sebelum, sesudah)
kemampuan

wilcox.test(sebelum, sesudah, paired = TRUE, data = kemampuan)



## Latihan 2 PPT Hal. 59
sebelum <- c(25, 27, 20, 21, 18, 19, 20, 22, 24, 25, 24, 27, 23, 25, 22)
sesudah <- c(26, 26, 22, 24, 22, 21, 24, 21, 26, 26, 25, 28, 25, 27, 25)
suplemen <- data.frame(sebelum, sesudah)
suplemen

wilcox.test(sebelum, sesudah, paired = TRUE, data = suplemen)



## Latihan 3 PPT Hal. 61
sebelum <- c(75, 50, 77, 80, 30, 68, 55, 72, 33, 70, 64, 40, 90, 35, 87, 75, 35, 60, 55, 82, 37, 66, 73, 65, 73, 45, 74, 76, 70, 60)
sesudah <- c(77, 60, 70, 81, 60, 70, 70, 75, 55, 70, 64, 50, 80, 45, 80, 67, 60, 60, 70, 80, 65, 65, 73, 60, 70, 50, 80, 75, 67, 70)
pertanian <- data.frame(sebelum, sesudah)
pertanian 

wilcox.test(sebelum, sesudah, paired = TRUE, data=pertanian)


