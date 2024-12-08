install.packages("IndexNumber")
library(IndexNumber)

# Contoh soal perhitungan 1 
harga <- c(5505, 6541)
index.number.chain(harga, "Harga")

# Contoh soal perhitungan 2
library (IndexNumber)
Harga_2008 <- c(300, 250)
index.number.chain (Harga_2008, "Nilai_Penjualan")

Harga_2009 <- c(300, 350)
index.number.chain (Harga_2009, "Nilai_Penjualan")

Harga_2010 <- c(300, 400)
index.number.chain (Harga_2010, "NIlai_Penjualan")

Harga_2011 <- c(300, 425)
index.number.chain (Harga_2011, "NIlai_Penjualan")

# Contoh Perhitungan 3
angka_indeks_gabungan <- c(40926, 45908)
index.number.chain(angka_indeks_gabungan, "Harga Rata-Rata")


## Round 2 Jenis-Jenis Angka Indeks
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


Harga_Barang_A <- c(100, 150, 200)
Harga_Barang_B <- c(200, 250, 300)
Harga_Barang_C <- c(500, 600, 700)
Harga_Barang_D <- c(400, 500, 600)
index.number.serie(Harga_Barang_A, "Harga")
index.number.serie(Harga_Barang_B, "Harga")
index.number.serie(Harga_Barang_C, "Harga")
index.number.serie(Harga_Barang_D, "Harga")






