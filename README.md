# =========================================
# PACKAGE
# =========================================

library(microbenchmark)

# =========================================
# FUNGSI MERGE
# =========================================

merge <- function(kiri, kanan) {
  
  hasil <- c()
  
  # Membandingkan elemen kiri dan kanan
  while(length(kiri) > 0 && length(kanan) > 0) {
    
    if(kiri[1] < kanan[1]) {
      
      hasil <- c(hasil, kiri[1])
      kiri <- kiri[-1]
      
    } else {
      
      hasil <- c(hasil, kanan[1])
      kanan <- kanan[-1]
    }
  }
  
  # Menggabungkan sisa elemen
  hasil <- c(hasil, kiri, kanan)
  
  return(hasil)
}

# =========================================
# FUNGSI MERGE SORT
# =========================================

merge_sort <- function(data) {
  
  # Basis rekursi
  if(length(data) <= 1) {
    return(data)
  }
  
  # Membagi data menjadi dua bagian
  tengah <- floor(length(data)/2)
  
  kiri <- data[1:tengah]
  kanan <- data[(tengah+1):length(data)]
  
  # Rekursif
  kiri_sort <- merge_sort(kiri)
  kanan_sort <- merge_sort(kanan)
  
  # Menggabungkan
  hasil <- merge(kiri_sort, kanan_sort)
  
  return(hasil)
}
# =========================================
# FUNGSI QUICKSORT WORST CASE
# Pivot selalu elemen pertama
# =========================================

quick_sort_worst <- function(data) {
  
  # Basis rekursi
  if(length(data) <= 1) {
    return(data)
  }
  
  # Pivot elemen pertama
  pivot <- data[1]
  
  # Membagi data berdasarkan pivot
  kiri <- data[data < pivot]
  tengah <- data[data == pivot]
  kanan <- data[data > pivot]
  
  # Rekursif
  hasil <- c(
    
    quick_sort_worst(kiri),
    tengah,
    quick_sort_worst(kanan)
    
  )
  
  return(hasil)
}

# =========================================
# UKURAN DATA
# Dibatasi agar tidak stack overflow
# =========================================

ukuran_data <- c(
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
)

runtime <- c()
runtime_merge <-c()
# =========================================
# PENGUJIAN WORST CASE
# Data sudah terurut
# =========================================

for(n in ukuran_data) {
  
  # Data terurut
  data <- 1:n
  
  cat("\n====================================\n")
  cat("JUMLAH DATA :", n, "\n")
  
  # Menampilkan sebagian data
  data_tampil <- head(data,1000)
  
  cat("\nDATA TERURUT (1000 DATA PERTAMA)\n")
  print(data_tampil)
  
  # =====================================
  # MENGHITUNG RUNTIME MERGE SORT
  # =====================================
  
  waktu_merge <- microbenchmark(
    
    merge_sort(data),
    
    times = 30
    
  )
  
  mean_merge <- mean(waktu_merge$time)
  
  runtime_merge <- c(runtime_merge, mean_merge)
  
  # =====================================
  # MENGHITUNG RUNTIME
  # =====================================
  
  waktu <- microbenchmark(
    
    quick_sort_worst(data),
    
    times = 30
    
  )
  
  # Median runtime
  rata_runtime <- mean(waktu$time)
  
  runtime <- c(runtime, rata_runtime)
  
  # =====================================
  # HASIL SORTING
  # =====================================
  
  cat("\nHASIL SORTING DATA\n")
  print(sort(data_tampil))
  
  
  # =====================================
  # Runtime
  # =====================================
  
  cat("\nRuntime Merge Sort :", 
      mean_merge, "nanodetik\n")
  
  
  cat("\nMean Runtime :", rata_runtime, "nanodetik\n")
}

# =========================================
# TABEL HASIL
# =========================================

hasil <- data.frame(
  
  Jumlah_Data = ukuran_data,
  Runtime_MergeSort = runtime_merge,
  Runtime = runtime
  
)

cat("\n====================================\n")
cat("TABEL HASIL PENGUJIAN\n")
print(hasil)


# =========================================
# GRAFIK PERBANDINGAN ALGORITMA
# =========================================

plot(
  
  ukuran_data,
  runtime_merge,
  
  type = "b",
  pch = 19,
  lwd = 2,
  col = "blue",
  
  xlab = "Jumlah Data (n)",
  ylab = "Runtime (nanodetik)",
  
  main = "Perbandingan Runtime Merge Sort dan Quicksort"
)

lines(
  
  ukuran_data,
  runtime,
  
  type = "b",
  pch = 19,
  lwd = 2,
  col = "red",
)

legend(
  
  "topleft",
  
  legend = c(
    "Merge Sort",
    "Quicksort"
  ),
  
  col = c("blue","red"),
  
  pch = c(19,17),
  
  lty = 1,
  
  lwd = 2
)
