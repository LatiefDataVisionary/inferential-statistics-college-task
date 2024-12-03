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



## HEHEHEHEHEHEHHEHE
sebelum1 <- rep(1,7)
sebelum2 <- rep(2,17)
sebelum <- c(sebelum1, sebelum2)

setelah1 <- c(rep(1,9), rep(2,3))
setelah2 <- c(rep(1,5), rep(2,4))
setelah <- c(setelah1, setelah2)

data<- data.frame(sebelum, setelah)
table(data)
mcnemar.test(table(data))




sebelum1 <- rep(1,10)
sebelum2 <- rep(2,20)
sebelum <- c(sebelum1, sebelum2)

setelah1 <- c(rep(1,9), rep(2,1))
setelah2 <- c(rep(1,14), rep(2,6))
setelah <- c(setelah1, setelah2)

data<- data.frame(sebelum, setelah)
table(data)
mcnemar.test(table(data))








## Contoh 2 PPT Hal. 35
sebelum <- c(5,4,3,4,4,3,3,4,4,3,4,3,3,2,4,3,3,5,2,2,3,3,4,4,3,4,4,4,3,2,4,4,5,5,4,3,2,3,2,3)
sesudah <- c(5,5,4,3,3,4,4,5,5,5,3,4,4,3,4,3,4,4,3,3,4,4,5,3,3,3,5,5,4,3,3,4,4,4,4,4,3,4,3,5)
diff <- sesudah - sebelum
data <- data.frame(Sebelum = sebelum, Sesudah = sesudah, Diff = diff)
data

library(BSDA)
SIGN.test(x = diff, alternative = "two.sided", conf.level = 0.95)
SIGN.test(x=data$diff, alternative="two.sided", conf.level=0.95)


## Contoh 1 PPT Hal. 27
istri<- c(4,5,4,4,5,4,4,2,1,4,3,2,4,4,3,2,4,1,5,5) 
suami<- c(1,4,5,5,4,3,3,1,3,5,2,3,1,2,2,1,5,2,4,4)
data<- data.frame(istri, suami, diff=suami-istri)
data

library(BSDA) 
#SIGN.test(x=data$diff, alternative="less", conf.level=0.95)
SIGN.test(x = diff, alternative = "less", conf.level = 0.95)



## Latihan 1 PPT Hal. 40
sendiri <- c(80, 82, 84, 86, 80, 84, 83, 81, 82, 80, 83, 84, 82, 81, 80, 79, 75)
kelompok <- c(78, 85, 82, 78, 84, 89, 83, 81, 90, 78, 85, 82, 86, 91, 80, 89, 79)
beda <- kelompok - sendiri
data <- data.frame(Sendiri = sendiri, Kelompok = kelompok, Beda = beda)
print(data)

data_analisis <- data[data$Beda != 0, ]
library(BSDA)
SIGN.test(x = data_analisis$Beda, alternative = "two.sided", conf.level = 0.95)

##HUHUHUHUHUHUHUHUHUHUHHUHU
sendiri <- c(80, 82, 84, 86, 80, 84, 83, 81, 82, 80, 83, 84, 82, 81, 80, 79, 75)
kelompok <- c(78, 85, 82, 78, 84, 89, 83, 81, 90, 78, 85, 82, 86, 91, 80, 89, 79)
data <- data.frame(Sendiri = sendiri, Kelompok = kelompok, Beda = kelompok-sendiri)
data 

library(BSDA)
SIGN.test(x = diff, alternative="two.sided", conf.level=0.95)




# contoh 1 uji tanda
beda <- c(-3,-1,1,1,-1,-1,-1,-1,2,1,-1,1,-3,-2,-1,-1,1,1,-1,-1)
library(BSDA)
SIGN.test(beda, alternative = "less")


# contoh 2 uji tanda
beda <- c(0,1,1,-1,-1,1,1,1,1,2,-1,1,1,1,0,0,1,-1,1,1,1,1,1,-1,0,-1,1,1,1,1,-1,0,-1,-1,0,1,1,1,1,2)
library(BSDA)
SIGN.test(beda, alternative = "two.sided")




## Contoh 𝑛 < 25 PPT Hal. 46
sebelum<- c(76, 58, 62, 67, 66, 81, 85, 72, 71, 75) 
sesudah<- c(80, 60, 68, 72, 79, 80, 82, 80, 81, 79)
pelatihan<- data.frame(sebelum, sesudah)
pelatihan

wilcox.test(sebelum, sesudah, paired=TRUE, data=pelatihan)

## HEHEHEHEHEHEHHEHEHHEHEHEHEH
wilcox.test(sebelum, sesudah, paired=TRUE)

## HUHUHUHUHUHUHU
wilcox.test(pelatihan$sebelum, pelatihan$sesudah, paired=TRUE)




## Latihan 3 PPT Hal. 61
sebelum <- c(75, 50, 77, 80, 30, 68, 55, 72, 33, 70, 64, 40, 90, 35, 87, 75, 35, 60, 55, 82, 37, 66, 73, 65, 73, 45, 74, 76, 70, 60)
sesudah <- c(77, 60, 70, 81, 60, 70, 70, 75, 55, 70, 64, 50, 80, 45, 80, 67, 60, 60, 70, 80, 65, 65, 73, 60, 70, 50, 80, 75, 67, 70)
pertanian <- data.frame(sebelum, sesudah)
pertanian 

wilcox.test(sebelum, sesudah, paired = TRUE, data=pertanian)
wilcox.test(sebelum, sesudah, paired = TRUE, conf.int = 0.975)




## Contoh 2 PPT Pertemuan 4,5 Hal 17
Kanker<- matrix(c(3, 1, 0, 3), nrow=2, dimnames=list(c("Merokok", "Tidak Merokok"), c("Kanker Paru", "Tidak Kanker Paru")))
Kanker

fisher.test(Kanker, alternative="greater")
fisher.test(Kanker, alternative="two.sided")
fisher.test(Kanker)














