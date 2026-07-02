# 1. Menyiapkan data yang sudah Anda impor
data_inflasi <- Data
data_inflasi$Nilai <- as.numeric(gsub(",", ".", data_inflasi$Nilai))

# 2. Mengubah kolom Tanggal dari teks menjadi format Date asli di R
data_inflasi$Tanggal <- as.Date(data_inflasi$Tanggal, format = "%d-%m-%Y")

# 3. Melihat struktur data untuk memastikan tipe data sudah benar
str(data_inflasi)

# 4. Mengubah menjadi objek Time Series (ts) di R (Frekuensi 12 = Bulanan)
inflasi_ts <- ts(data_inflasi$Nilai, start = c(1995, 1), frequency = 12)

# 5. Plot data untuk melihat trennya
plot(inflasi_ts, 
     main = "Data Time Series Inflasi Kota Makassar", 
     ylab = "Inflasi (%)", 
     xlab = "Tahun", 
     col = "magenta", 
     lwd = 2)

# SARIMA
library(tseries)
library(forecast)
library(lmtest)

# ==============================================================================
# TAHAP 1: UJI STASIONERITAS DALAM VARIANS
# ==============================================================================
cat("\n--- 1. Uji Stasioneritas dalam Varians ---\n")
min_val <- min(inflasi_ts)
if (min_val <= 0) {
  inflasi_ts_pos <- inflasi_ts + abs(min_val) + 1
} else {
  inflasi_ts_pos <- inflasi_ts
}

lambda_val <- BoxCox.lambda(inflasi_ts_pos)
cat("Nilai Lambda Box-Cox:", lambda_val, "\n")
if(lambda_val > 0.5) {
  print("Kesimpulan: Data diasumsikan STASIONER dalam varians (tidak perlu transformasi data).")
  data_model <- inflasi_ts
} else {
  print("Kesimpulan: Data TIDAK STASIONER dalam varians. Transformasi Box-Cox diterapkan.")
  data_model <- BoxCox(inflasi_ts_pos, lambda_val)
}

# Menampilkan plot data setelah melalui proses (atau pengecekan) Box-Cox
plot(data_model, 
     main = "Plot Data Inflasi Setelah Transformasi Box-Cox", 
     ylab = "Nilai Transformasi", 
     xlab = "Tahun", 
     col = "darkgreen", 
     lwd = 2)

# Opsi tambahan: Membandingkan langsung dengan plot aslinya
par(mfrow=c(2,1)) # Membagi jendela grafik menjadi 2 baris (atas dan bawah)
plot(inflasi_ts, main = "Data Asli", col="blue", ylab="Inflasi")
plot(data_model, main = "Data Setelah Transformasi Box-Cox", col="darkgreen", ylab="Nilai Transformasi")
par(mfrow=c(1,1)) # Mengembalikan jendela grafik ke normal (1 plot saja)

# ==============================================================================
# TAHAP 2: UJI STASIONERITAS DALAM RATA-RATA
# ==============================================================================
cat("\n--- 2. Uji Stasioneritas dalam Rata-rata ---\n")
# Menggunakan Augmented Dickey-Fuller (ADF) Test
# H0: Data tidak stasioner (memiliki akar unit)
# H1: Data stasioner (p-value < 0.05)
uji_adf <- adf.test(data_model)
print(uji_adf)

# Mengecek apakah butuh differencing biasa (d) atau musiman (D)
d_val <- ndiffs(data_model)
D_val <- nsdiffs(data_model)
cat("Jumlah Differencing Non-Musiman (d) yang disarankan:", d_val, "\n")
cat("Jumlah Differencing Musiman (D) yang disarankan:", D_val, "\n")

# Lakukan differencing jika d > 0 atau D > 0
data_diff <- data_model
if(D_val > 0) data_diff <- diff(data_diff, lag = 12, differences = D_val)
if(d_val > 0) data_diff <- diff(data_diff, differences = d_val)

# Plot data setelah disesuaikan agar stasioner
plot(data_diff, main="Plot Data Setelah Differencing", col="blue", ylab="Nilai Diferensiasi")

# ==============================================================================
# TAHAP 3: IDENTIFIKASI & PENENTUAN BEST MODEL
# ==============================================================================
cat("\n--- 3. Identifikasi dan Penentuan Best Model SARIMA ---\n")
# Kita akan menggunakan fungsi auto.arima() untuk mencari model terbaik
# berdasarkan kriteria AIC (Akaike Information Criterion) terkecil.
# Fungsi ini secara otomatis memeriksa efek AR, MA, Seasonal AR, dan Seasonal MA.

best_model <- auto.arima(data_model, 
                         ic = "aic", 
                         stepwise = FALSE,   # Pencarian ekstensif (semua kombinasi dicoba)
                         approximation = FALSE, 
                         trace = TRUE)       # trace=TRUE akan mencetak daftar model yang diuji

cat("\n--- Model SARIMA Terbaik (Best Model) ---\n")
summary(best_model)

# ==============================================================================
# TAHAP 4: UJI ASUMSI (DIAGNOSTIK RESIDUAL)
# ==============================================================================
cat("\n--- 4. Uji Asumsi Residual ---\n")
residual_model <- residuals(best_model)

# 4.1 Uji Autokorelasi Residual (Ljung-Box Test)
# H0: Tidak ada autokorelasi pada residual (Model sudah cukup baik)
# H1: Terdapat autokorelasi pada residual
uji_ljung <- Box.test(residual_model, type = "Ljung-Box", lag = 24, fitdf = length(best_model$coef))
print(uji_ljung)
if(uji_ljung$p.value > 0.05){
  print("Kesimpulan (Autokorelasi): Residual bersifat acak (white noise). Asumsi TERPENUHI.")
} else {
  print("Kesimpulan (Autokorelasi): Residual tidak acak. Model mungkin belum menangkap semua pola.")
}

# 4.2 Uji Normalitas Residual (Shapiro-Wilk Test)
# H0: Residual berdistribusi normal
# H1: Residual tidak berdistribusi normal
# Catatan: Shapiro-Wilk optimal untuk n < 5000.
uji_normal <- shapiro.test(residual_model)
print(uji_normal)
if(uji_normal$p.value > 0.05){
  print("Kesimpulan (Normalitas): Residual berdistribusi normal. Asumsi TERPENUHI.")
} else {
  print("Kesimpulan (Normalitas): Residual tidak normal. (Wajar untuk data ekonomi riil yang memiliki *outlier* saat krisis).")
}

# Visualisasi Plot Diagnostik Bawaan (Histogram, ACF Residual)
checkresiduals(best_model)

# ==============================================================================
# TAHAP 5: FORECASTING (PERAMALAN) & EVALUASI ERROR
# ==============================================================================
cat("\n--- 5. Peramalan (Forecasting) dan Evaluasi Error ---\n")

# Melakukan peramalan untuk 12 periode (12 bulan ke depan)
forecast_12 <- forecast(best_model, h = 12)

# Menampilkan hasil ramalan (Point Forecast, Lower 95%, Upper 95%)
print(forecast_12)

# Plot hasil peramalan
plot(forecast_12, 
     main = "Forecasting Inflasi Kota Makassar (12 Bulan ke Depan)", 
     ylab = "Tingkat Inflasi", 
     xlab = "Tahun",
     fcol = "red",       # Warna garis forecast
     flwd = 2,           # Ketebalan garis forecast
     shadecols = "pink") # Warna area confidence interval

# Mengevaluasi Error (Akurasi Model in-sample)
# Menampilkan nilai RMSE, MAE, dan MAPE
akurasi_model <- accuracy(best_model)
cat("\n--- Tingkat Error Model (In-Sample) ---\n")
print(akurasi_model)

#ARCH
# Memuat library yang dibutuhkan
# 1. Instal alat 'pak' terlebih dahulu
# install.packages("pak")

# 2. Gunakan 'pak' untuk menginstal rugarch dan FinTS beserta semua dependensinya
# pak::pkg_install(c("rugarch", "FinTS"))

library(rugarch)
library(FinTS)

# Mengambil nilai residual dari model SARIMA terbaik di tahap sebelumnya
res_sarima <- residuals(best_model)

# ==============================================================================
# TAHAP 1: UJI EFEK ARCH (HETEROSKEDASTISITAS)
# ==============================================================================
cat("\n--- 1. Uji Efek ARCH (ARCH-LM Test) ---\n")
# H0: Tidak ada efek ARCH (Varians konstan / Homoskedastis)
# H1: Terdapat efek ARCH (Varians tidak konstan / Heteroskedastis)
# Kita gunakan lag=12 karena data bulanan
uji_arch_awal <- ArchTest(res_sarima, lags = 12, demean = TRUE)
print(uji_arch_awal)

if(uji_arch_awal$p.value < 0.05){
  print("Kesimpulan: P-value < 0.05. Tolak H0. Terdapat efek ARCH. Lanjut ke pemodelan ARCH.")
} else {
  print("Kesimpulan: P-value > 0.05. Tidak ada efek ARCH. Secara teoritis data tidak butuh model ARCH, namun kita bisa tetap melanjutkan untuk eksperimen.")
}

# ==============================================================================
# TAHAP 2: IDENTIFIKASI & PENENTUAN BEST MODEL ARCH
# ==============================================================================
cat("\n--- 2. Mencari Best Model ARCH(q) ---\n")
# Karena ini model hybrid (SARIMA-ARCH), model mean (rata-rata) sudah ditangani SARIMA.
# Oleh karena itu, armaOrder diset c(0,0) agar rugarch hanya memodelkan varians dari residual.

best_aic <- Inf
best_q <- 1
best_arch_fit <- NULL

# Looping untuk menguji ARCH(1) hingga ARCH(5) dan mencari AIC terkecil
for (q in 1:4) {
  # Spesifikasi model ARCH(q) yang ekuivalen dengan GARCH(q, 0)
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(q, 0)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "norm" # Asumsi distribusi normal, bisa diganti "std" (Student-t) jika data sangat bergejolak
  )
  
  # Fitting model
  fit <- ugarchfit(spec = spec, data = res_sarima, solver = "hybrid")
  
  # Mengecek apakah algoritma berhasil konvergen (berhasil menemukan parameter)
  if (fit@fit$convergence == 0) { 
    aic_val <- infocriteria(fit)[1] # Mengambil nilai AIC
    cat("AIC untuk ARCH(", q, "): ", aic_val, "\n", sep="")
    
    if (aic_val < best_aic) {
      best_aic <- aic_val
      best_q <- q
      best_arch_fit <- fit
    }
  }
}

cat("\n--- Best Model ARCH Ditemukan ---\n")
cat("Model Terbaik: ARCH(", best_q, ") dengan AIC = ", best_aic, "\n", sep="")
# Tampilkan detail koefisien dari best model
show(best_arch_fit)
  
# ==============================================================================
# TAHAP 3: UJI ASUMSI (DIAGNOSTIK RESIDUAL ARCH)
# ==============================================================================
cat("\n--- 3. Uji Diagnostik Setelah Pemodelan ARCH ---\n")
# Mengecek apakah model ARCH sudah berhasil menghilangkan efek heteroskedastisitas
std_res <- residuals(best_arch_fit, standardize = TRUE)

uji_arch_akhir <- ArchTest(std_res, lags = 12, demean = TRUE)
print(uji_arch_akhir)

if(uji_arch_akhir$p.value > 0.05){
  print("Kesimpulan: Model ARCH BERHASIL menangkap volatilitas (tidak ada sisa efek ARCH).")
} else {
  print("Kesimpulan: Masih terdapat sisa efek ARCH. Diperlukan model GARCH (akan dilanjutkan pada tahap berikutnya).")
}

# ==============================================================================
# TAHAP 4: FORECASTING VOLATILITAS
# ==============================================================================
cat("\n--- 4. Peramalan (Forecasting) Volatilitas 12 Bulan ke Depan ---\n")
# Kita meramalkan volatilitas (risiko/simpangan baku), bukan nilai inflasinya
arch_forecast <- ugarchforecast(best_arch_fit, n.ahead = 12)
print(arch_forecast)

# Plot hasil ramalan volatilitas (Sigma)
plot(arch_forecast, which = 1) 
# Note: R mungkin meminta Anda menekan 'Enter' atau memilih angka 1 di console untuk memunculkan plot

# ==============================================================================
# TAHAP 5: EVALUASI ERROR (IN-SAMPLE)
# ==============================================================================
cat("\n--- 5. Evaluasi Akurasi Model Volatilitas ---\n")
# Menggunakan proksi absolut residual sebagai nilai "aktual" volatilitas
vol_aktual <- abs(res_sarima)
vol_prediksi <- sigma(best_arch_fit) # Volatilitas (sigma) yang diprediksi model in-sample

rmse_arch <- sqrt(mean((vol_aktual - vol_prediksi)^2))
mae_arch <- mean(abs(vol_aktual - vol_prediksi))

cat("RMSE (Root Mean Squared Error):", rmse_arch, "\n")
cat("MAE (Mean Absolute Error):", mae_arch, "\n")

#GARCH
# Memuat library yang dibutuhkan
library(rugarch)

# Menggunakan residual dari SARIMA (res_sarima)
# ==============================================================================
# TAHAP 1: IDENTIFIKASI & PENENTUAN BEST MODEL GARCH(p,q)
# ==============================================================================
cat("\n--- 1. Mencari Best Model GARCH(p,q) ---\n")

best_aic <- Inf
best_p <- 1
best_q <- 1
best_garch_fit <- NULL

# Kita akan mencoba kombinasi p dan q dari 1 sampai 2 (umumnya cukup untuk data inflasi)
for (p in 1:2) {
  for (q in 1:2) {
    # Spesifikasi model GARCH(p,q)
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm"
    )
    
    # Fitting model
    fit <- tryCatch(ugarchfit(spec = spec, data = res_sarima, solver = "hybrid"), 
                    error = function(e) NULL)
    
    # Cek konvergensi dan AIC
    if (!is.null(fit) && fit@fit$convergence == 0) {
      aic_val <- infocriteria(fit)[1]
      cat("AIC untuk GARCH(", p, ",", q, "): ", aic_val, "\n", sep="")
      
      if (aic_val < best_aic) {
        best_aic <- aic_val
        best_p <- p
        best_q <- q
        best_garch_fit <- fit
      }
    }
  }
}

cat("\n--- Best Model GARCH Ditemukan ---\n")
cat("Model Terbaik: GARCH(", best_p, ",", best_q, ") dengan AIC = ", best_aic, "\n", sep="")
show(best_garch_fit)

# ==============================================================================
# TAHAP 2: UJI ASUMSI DIAGNOSTIK RESIDUAL (STANDARDIZED RESIDUALS)
# ==============================================================================
cat("\n--- 2. Uji Diagnostik Residual GARCH ---\n")
# Kita periksa apakah residual sudah 'clean' (tidak ada autokorelasi)
std_res <- residuals(best_garch_fit, standardize = TRUE)

# Uji Ljung-Box pada Standardized Residuals
lb_test <- Box.test(std_res, lag = 12, type = "Ljung-Box", fitdf = (best_p + best_q))
print(lb_test)

# Uji ARCH-LM pada Standardized Residuals (Memastikan volatilitas sudah dimodelkan)
arch_test_final <- ArchTest(std_res, lags = 12, demean = TRUE)
print(arch_test_final)

# ==============================================================================
# TAHAP 3: FORECASTING VOLATILITAS
# ==============================================================================
cat("\n--- 3. Peramalan Volatilitas 12 Bulan ke Depan ---\n")
garch_forecast <- ugarchforecast(best_garch_fit, n.ahead = 12)
print(garch_forecast)

# Plot ramalan volatilitas
plot(garch_forecast, which = 1)

# ==============================================================================
# TAHAP 4: EVALUASI ERROR (IN-SAMPLE)
# ==============================================================================
cat("\n--- 4. Evaluasi Akurasi Model GARCH ---\n")
vol_aktual <- abs(res_sarima)
vol_prediksi <- sigma(best_garch_fit)

rmse_garch <- sqrt(mean((vol_aktual - vol_prediksi)^2))
mae_garch <- mean(abs(vol_aktual - vol_prediksi))

cat("RMSE GARCH:", rmse_garch, "\n")
cat("MAE GARCH:", mae_garch, "\n")