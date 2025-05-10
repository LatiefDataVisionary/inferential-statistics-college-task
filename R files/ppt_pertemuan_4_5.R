### Uji Fisher

## Contoh 1 PPT Pertemuan 4,5 Hal 10
Hama<- matrix(c(2, 5, 4, 3), nrow=2, dimnames=list(c("Desa A", "Desa B"),c("Hama Tikus", "Hama Keong Mas")))
Hama

fisher.test(Hama, alternative="two.sided")


## Contoh 2 PPT Pertemuan 4,5 Hal 17
Kanker<- matrix(c(3, 1, 0, 3), nrow=2, dimnames=list(c("Merokok", "Tidak Merokok"), c("Kanker Paru", "Tidak Kanker Paru")))
Kanker

fisher.test(Kanker, alternative="greater")


## Soal Latihan PPT Pertemuan 4,5 Hal 21
kepala_bumn <-  matrix(c(1,4,6,1), nrow=2, dimnames=list(c("Ka. BUMN", "ka. Per.Swasta"), c("Bukan Sarjana", "Sarjana")))
kepala_bumn

fisher.test(kepala_bumn, alternative="two.sided")
fisher.test(kepala_bumn, alternative="greater")
fisher.test(kepala_bumn, alternative="less")


# Modul Praktikum Latihan 1 Hal 38
birodemisi <- matrix(c(3,5,5,2), nrow=2, dimnames=list(c("Birokrat", "Akademisi"), c("Terang", "Gelap")))
birodemisi

fisher.test(birodemisi)


#-------------------------------------------------------------------------------

### Uji Chi Square Dua Sampel Indenpenden

## PPT Pertemuan 4,5 Hal 26
siswa <- matrix(c(30,14,36,20), nrow=2, dimnames=list(c("Lulus", "Tdk lulus"), c("cowo", "cewe")))
siswa

chisq.test(siswa)


## PPT Pertemuan 4,5 Hal 31

Alumni <- matrix(c(15, 10, 5, 15, 20, 25), nrow=3, dimnames=list(c("Pertanian", "Industri", "Jasa"), c("sosek", "Produksi")))
Alumni

chisq.test(Alumni)



Hama<- matrix(c(2, 5, 4, 3), nrow=2, dimnames=list(c("Desa A", "Desa B"),c("Hama Tikus", "Hama Keong Mas")))
Hama

chisq.test(Hama)


## PPT Pertemuan 4,5 Hal 37
tipe_sekolah<- matrix(c(77,14,91,18), nrow=2, dimnames=list(c("Negeri", "Swasta"),c("Laki-laki", "Perempuan")))
tipe_sekolah

chisq.test(tipe_sekolah)
