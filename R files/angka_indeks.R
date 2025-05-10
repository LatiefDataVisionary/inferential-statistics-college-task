install.packages("IndexNumber")
library(IndexNumber)

## Round 1: Jenis-Jenis Angka Indeks

# Contoh soal perhitungan 1 
harga <- c(5505, 6541)
index.number.chain(harga, "Harga")

# Contoh soal perhitungan 2
Harga_2008 <- c(300, 250)
index.number.chain(Harga_2008, "Nilai_Penjualan")

Harga_2009 <- c(300, 350)
index.number.chain(Harga_2009, "Nilai_Penjualan")

Harga_2010 <- c(300, 400)
index.number.chain(Harga_2010, "NIlai_Penjualan")

Harga_2011 <- c(300, 425)
index.number.chain(Harga_2011, "NIlai_Penjualan")

# Contoh Perhitungan 3
angka_indeks_gabungan <- c(40926, 45908)
index.number.chain(angka_indeks_gabungan, "Harga Rata-Rata")


## Round 2: Jenis-Jenis Angka Indeks
# Berdasarkan Penggunaan
# Indeks Harga (Price Index)
angka_indeks_gabungan <-c(10000+15000+25000, 12000+18000+30000)
index.number.chain(angka_indeks_gabungan, "Harga rata-rata")


# Indeks Kuantitas (Quantity Index)
produk_A <- c(1000, 1200)
index.number.chain(produk_A, "Kuantitas A")

produk_B <- c(800, 900)
index.number.chain(produk_B, "Kuantitas B")


# Indeks Nilai (Value Index)
produk_A <- c(1000*10000, 1200*12000)
index.number.chain(produk_A, "Nilai A")

produk_B <- c(800*15000, 900*18000)
index.number.chain(produk_B, "Nilai B")

# Berdasarkan Penentuan
# Indeks Tidak Tertimbang
Indeks_Tidak_Tertimbang <- c(10000+8000+15000, 12000+10000+18000)
index.number.chain(Indeks_Tidak_Tertimbang, "Harga Makanan")

# Indeks Tertimbang
Indeks_Tertimbang <- c((1000*50)+(8000*30)+(15000*20), (12000*50)+(10000*30)+(18000*20))
index.number.chain(Indeks_Tertimbang, "Harga Makanan")

# Indeks Rantai
indeks_Berantai <- c(10000, 12000, 15000)
index.number.chain(indeks_Berantai, "Harga")


## Round 3: Metode Penghitungan Angka Indeks
# a. Indeks Harga Relatif Sederhana
Harga_Barang_A <- c(100, 150, 200)
Harga_Barang_B <- c(200, 250, 300)
Harga_Barang_C <- c(500, 600, 700)
Harga_Barang_D <- c(400, 500, 600)
index.number.serie(Harga_Barang_A, name="Harga")
index.number.serie(Harga_Barang_B, name="Harga")
index.number.serie(Harga_Barang_C, name="Harga")
index.number.serie(Harga_Barang_D, name="Harga")


# b. Indeks Kuantitas Relatif Sederhana
Padisawah<- c(24732, 27993, 30989)
PadiLadang<- c(1551, 1659, 1785)
Jagung<- c(3606, 3994, 4509)
Ketela<- c(13751, 13774, 13301)
index.number.serie (Padisawah, name="Kuantitas")
index.number.serie(PadiLadang, name="Kuantitas")
index.number.serie(Jagung, name="Kuantitas")
index.number.serie(Ketela, name="Kuantitas")

# c.	Angka Indeks Agregat Sederhana 
Komoditi<- matrix(c(12518, 8355, 5505, 3241, 23825, 12529, 10628, 6541, 3333, 25406), nrow=2, byrow=TRUE)
aggregated.index.number(Komoditi, "serie", "BDutot", "Harga", opt.plot=FALSE, opt.summary=FALSE)

# d.	Angka Indeks Kuantitas Agregat Sederhana 
Quantity<- matrix(c(14684, 4979, 652, 2261, 14787, 5743, 159, 120, 80, 20875, 15931, 1294, 5107, 54415, 13882, 17051, 100, 120), nrow=2, byrow=TRUE)
aggregated.index.number (Quantity, "serie", "BDutot", "Kuantitas", opt.plot=FALSE, opt.summary=FALSE)

# e.	Angka Indeks Nilai Kuantitas Agregat Sederhana  
Harga<- matrix (c(8000, 5000, 2000, 4500, 10000, 8000, 3000, 5000), nrow=2, byrow=TRUE)
Quantity<- matrix(c(4, 2, 1, 6, 6, 3, 2, 6), nrow=2, byrow=TRUE)
Nilai<- Harga * Quantity
aggregated.index.number (Nilai, "serie", "BDutot", "Nilai", opt.plot=FALSE, opt.summary=FALSE)

# f.	Angka Indeks Harga Tertimbang 
# (a) Dengan metode Laspeyres.
Harga<- matrix(c(500, 525, 800, 900, 600, 700, 300, 400), ncol=4) 
Kuantitas<- matrix (c(2,4, 5, 6, 3, 4, 10, 15), ncol=4)
laspeyres.index.number(Harga, Kuantitas, name="biaya", opt.plot=FALSE, opt.summary=FALSE)

# (b) Dengan metode Paasche.
Harga<- matrix(c(500, 525, 800, 900, 600, 700, 300, 400), ncol=4) 
Kuantitas<- matrix (c(2,4, 5, 6, 3, 4, 10, 15), ncol=4)
paasche.index.number(Harga, Kuantitas, name="biaya", opt.plot=FALSE, opt.summary=FALSE)

# (c) Dengan metode Irving Fisher.
Harga<- matrix(c(500, 525, 800, 900, 600, 700, 300, 400), ncol=4) 
Kuantitas<- matrix (c(2,4, 5, 6, 3, 4, 10, 15), ncol=4)
fisher.index.number(Harga, Kuantitas, name="biaya", opt.plot=FALSE, opt.summary=FALSE)

# (d) Dengan metode Drobish.
Harga<- matrix(c(500, 525, 800, 900, 600, 700, 300, 400), ncol=4) 
Kuantitas<- matrix (c(2,4, 5, 6, 3, 4, 10, 15), ncol=4)
drobish.index.number(Harga, Kuantitas, name="biaya", opt.plot=FALSE, opt.summary=FALSE)

# (e) Dengan metode Marshall-Edgeworth.
Harga<- matrix(c(500, 525, 800, 900, 600, 700, 300, 400), ncol=4) 
Kuantitas<- matrix (c(2,4, 5, 6, 3, 4, 10, 15), ncol=4)
edgeworth.index.number(Harga, Kuantitas, name="biaya", opt.plot=FALSE, opt.summary=FALSE)


## Round 4: Jenis-Jenis Angka Indeks
Harga<- c(4900, 5154, 5505, 6541) 
index.number.chain(Harga, "Harga")

## Bagian 5: Pergeseran Tahun Dasar
indeks_awal <- c(100, 120, 130, 142, 150, 200, 250, 300) # Indeks awal dengan tahun dasar 2005
tahun_dasar_baru <- indeks_awal [6] # Indeks tahun 2010
indeks_baru <- (indeks_awal / tahun_dasar_baru)*100 # Hitung indeks baru
indeks_baru # Menampilkan hasil


## Bagian 3: Merangkai Angka Indeks (Splicing) 
#Backward Splicing
#Data Indeks Lama dan Baru
indeks_lama <- c(100, 130, 140, 150) # Tahun dasar 2004
indeks_baru <- c(100, 120, 130, 135, 140) # Tahun dasar 2007 # Rasio splicing (menghubungkan tahun 2010 dari kedua rangkaian)
rasio_splicing <- indeks_lama [4] / indeks_baru [1] # Tahun 2007 adalah titik penghubung
# Sesuaikan Indeks Baru ke Tahun Dasar Lama (2004) 
indeks_baru_spliced <- indeks_baru * rasio_splicing
#Gabungkan kedua rangkaian
indeks_tergabung <- c(indeks_lama, indeks_baru_spliced[-1]) # Hindari pengulangan tahun 2007
#Tampilkan hasil
indeks_tergabung

#Forward splicing
#Rasio splicing (menghubungkan tahun 2007 dari kedua rangkaian)
rasio_splicing <- indeks_baru [1] / indeks_lama [4] # Tahun 2007 adalah titik penghubung
# Sesuaikan Indeks Lama ke Tahun Dasar Baru (2007) 
indeks_lama_spliced <- indeks_lama * rasio_splicing
# Gabungkan kedua rangkaian
indeks_tergabung <- c(indeks_lama_spliced [-length (indeks_lama)], indeks_baru)
#Tampilkan hasil indeks_tergabung


## Bagian 7: Angka Indeks untuk Proses Deflasi
#Angka Indeks untuk proses deflasi
upah_nominal <- c(500000, 600000, 750000, 800000) 
ihk <- c(80, 125, 200, 320)
upah_riil <- (upah_nominal / ihk) * 100
upah_riil


## Bagian 8: Daya Beli Mata Uang
#Daya Beli Mata Uang
Ihk <- c(80, 95, 100, 120, 154, 170, 425)
ihktahundasar<- c(100)
daya_beli <- (Ihk / ihktahundasar)
daya_beli
