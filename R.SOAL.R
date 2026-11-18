# =====================================================
# NOMOR 1. MEMBUAT FUNGSI IMPOR TABEL
# =====================================================
# Membuat fungsi impor tabel 
impor_tabel <- function(tabel, dbname) {
  con <- koneksi_mysql(dbname)   
  on.exit(dbDisconnect(con),  add = TRUE)   
  
  dbReadTable(con, tabel)
} 
# Mengimpor tabel responden
responden <- impor_tabel("responden", "db_survei") 
# Menampilkan enam baris pertama 
head(responden) 
# Melihat struktur data 
str(responden) 

# NOMOR 2. KONVERSI TANGGAL DAN MISSING VALUE
# ===================================================== 
#Mengubah variabel tgl_survei menjadi Date 
responden$tgl_survei <- as.Date(responden$tgl_survei) 
# Melihat struktur data setelah konversi 
str(responden) 
# Menghitung missing value setiap variabel
jumlah_missing <- colSums(is.na(responden)) 
# Menghitung total missing value
sum(is.na(responden)) 
# Menampilkan missing value dalam bentuk tabel 
data.frame(  
  Variabel = names(responden),
  Jumlah_Missing = jumlah_missing
)      

# NOMOR 3. MEMBANDINGKAN RATA-RATA USIA R DAN MYSQL 
# ===================================================== 
# Menghitung rata-rata usia menggunakan R 
rata_rata_R <- mean(responden$usia, na.rm = TRUE)
rata_rata_R 
# Membuat koneksi ke database MySQL 
con <- koneksi_mysql("db_survei") 
# Menghitung rata-rata usia menggunakan MySQL 
rata_rata_SQL <- dbGetQuery( 
  con, 
  "SELECT AVG(usia) AS rata_rata_usia   
  FROM responden"
) 
rata_rata_SQL 
# Menyamakan ketelitian angka desimal 
rata_rata_SQL <- dbGetQuery(   
  con,
  "SELECT CAST(AVG(usia) AS DECIMAL(20,10)) AS rata_rata_usia    
  FROM responden"
) 
hasil_SQL <- as.numeric(rata_rata_SQL$rata_rata_usia)  
# Membandingkan hasil R dan MySQL
hasil_perbandingan <- isTRUE(   all.equal(rata_rata_R, hasil_SQL) )  
hasil_perbandingan
# Menutup koneksi 
dbDisconnect(con) 

# NOMOR 4. JOIN TABEL DAN JUMLAH RESPONDEN PER WILAYAH 
# =====================================================  
# Membuat koneksi ke database 
con <- koneksi_mysql("db_survei")  
# Menggabungkan tabel responden dan kab_kota 
data_gabungan <- dbGetQuery(  
  con,
  "   
  SELECT
   r.id_resp,
  r.kode_wilayah,
   k.nama, 
     k.jenis,
     r.usia, 
     r.pengeluaran_jt,
       r.tgl_survei
     FROM responden r 
       INNER JOIN kab_kota k 
    ON r.kode_wilayah = k.kode  
  "                             
) 
# Menampilkan hasil penggabungan 
head(data_gabungan) 
# Menghitung jumlah responden setiap wilayah 
jumlah_responden <- dbGetQuery( 
  con, 
  " 
  SELECT 
  k.kode,
  k.nama, 
  COUNT(r.id_resp) AS jumlah_responden 
  FROM kab_kota k 
  LEFT JOIN responden r
  ON k.kode = r.kode_wilayah 
  GROUP BY k.kode, k.nama 
  "
) 
jumlah_responden 
# Menutup koneksi 
dbDisconnect(con) 

# NOMOR 5. MENCARI WILAYAH YANG BELUM DISURVEI
# ===================================================== 
# Membuat koneksi ke database 
con <- koneksi_mysql("db_survei") 
# Mencari wilayah yang tidak memiliki responden 
wilayah_belum_disurvei <- dbGetQuery( con,
                                      " SELECT
                                      k.kode, 
                                      k.nama,
                                      k.jenis FROM kab_kota k
                                      LEFT JOIN responden r
                                      ON k.kode = r.kode_wilayah 
                                      WHERE r.id_resp IS NULL
                                      "
)
# Menampilkan wilayah yang belum disurvei 
wilayah_belum_disurvei 
# Menutup koneksi 
dbDisconnect(con) 

# NOMOR 6. EXPORT SUMMARY DAN VALIDASI TRANSFER
# ===================================================== 
# Membuat koneksi baru ke database 
con <- koneksi_mysql("db_survei")  
# Membuat ringkasan jumlah responden per wilayah 
summary_wilayah <- dbGetQuery( 
  con, 
  "  
  SELECT  
  k.kode, 
  k.nama, 
  k.jenis,  
  COUNT(r.id_resp) AS jumlah_responden 
  FROM kab_kota k  
  LEFT JOIN responden r  
  ON k.kode = r.kode_wilayah   
  GROUP BY k.kode, k.nama, k.jenis   
  " 
) 
# Menampilkan hasil ringkasan 
summary_wilayah 
# Menghapus tabel lama jika sudah ada 
dbExecute(con, "DROP TABLE IF EXISTS summary_wilayah") 
# Membuat tabel baru 
dbExecute(   
  con,   
  "  
  CREATE TABLE summary_wilayah ( 
  kode CHAR(4), 
  nama VARCHAR(100), 
  jenis VARCHAR(50),   
  jumlah_responden INT 
  ) 
  " 
) 
#Memasukkan data ke tabel MySQL 
for (i in 1:nrow(summary_wilayah)) {   
  query <- sprintf(    
    "INSERT INTO summary_wilayah   
    (kode, nama, jenis, jumlah_responden)     
    VALUES ('%s', '%s', '%s', %d)",  
    dbEscapeStrings(con, summary_wilayah$kode[i]), 
    dbEscapeStrings(con, summary_wilayah$nama[i]),   
    dbEscapeStrings(con, summary_wilayah$jenis[i]),    
    summary_wilayah$jumlah_responden[i]   )     
  dbExecute(con, query) } 
# Mengambil kembali tabel hasil ekspor
hasil_export <- dbReadTable(con, "summary_wilayah")  
# Menampilkan hasil ekspor 
hasil_export 
# Validasi transfer data 
validasi_transfer <- data.frame(   Pemeriksaan = c(     
  "Jumlah baris",
  "Jumlah kolom",    
  "Nama kolom"  
), 
Hasil =c(   
  nrow(summary_wilayah) == nrow(hasil_export),  
  ncol(summary_wilayah) == ncol(hasil_export),   
  identical(names(summary_wilayah), names(hasil_export)) 
)
)  
# Menampilkan hasil validasi 
validasi_transfer 
# Menutup koneksi 
dbDisconnect(con) 