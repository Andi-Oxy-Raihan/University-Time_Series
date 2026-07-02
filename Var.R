library(readxl)
library(vars)
library(tseries)
library(ggplot2)
library(reshape2)
library(FinTS)
data_var<- `Data`

# 1. Konversi Tanggal dengan format yang benar
# Format "1/7/2025" artinya %m (bulan) / %d (hari) / %Y (tahun)
data_var$Tanggal <- as.Date(data_var$TIME, format = "%m/%d/%Y")

# 2. Balik urutan data agar dari yang TUA ke yang BARU
# Fungsi rev() membalik baris data
data_var <- data_var[order(data_var$Tanggal), ]

# Cek kembali hasilnya
head(data_var)

# 1. Ubah koma menjadi titik dan konversi ke numerik
# Kita terapkan ke kolom 2 sampai 4 (IDR, BCA, BRI)
data_var$IDR <- as.numeric(gsub(",", ".", data_var$IDR))
data_var$BCA <- as.numeric(gsub(",", ".", data_var$BCA))
data_var$BRI <- as.numeric(gsub(",", ".", data_var$BRI))

# 2. Cek kembali strukturnya
str(data_var)

summary(data_var)

# mengubah menjadi objek time series
y <- data_var[, c("IDR", "BCA", "BRI")]
y_ts <- ts(y, start = c(2025, 1), frequency = 252)

# Visualisasi data VAR -----------------------------------------------------
y_long <- melt(data.frame(Tanggal = data_var$Tanggal, y), id.vars = "Tanggal")

ggplot(y_long, aes(x = Tanggal, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = "Data VAR: Pertumbuhan Bulanan", x = "Periode", y = "Persen per Bulan") +
  theme_minimal()

g_idr <- y_ts[, "IDR"]
g_bca <- y_ts[, "BCA"]
g_bri <- y_ts[, "BRI"]

# Jalankan kode ini di RStudio
# lag.max = 10 berarti R akan mengecek dari lag 1 sampai 10
pilihan_lag <- VARselect(y_ts, lag.max = 10, type = "const")

# Lihat hasilnya
pilihan_lag$selection

# Uji ARCH (autocorrelation di varians)/ stasioneritas dalam varians
# Ho : Homoskedastisitas
arch_test_idr <- ArchTest(g_idr, lags = 6)  # lags: jumlah lag yang diperiksa
arch_test_bca <- ArchTest(g_bca, lags = 6)  
arch_test_bri <- ArchTest(g_bri, lags = 6)

# Uji stasioneritas ADF ----------------------------------------------------
# H0: data tidak stasioner. Jika p-value < 0,05, data dianggap stasioner.
adf_idr     <- adf.test(y_ts[, "IDR"])
adf_bca     <- adf.test(y_ts[, "BCA"])
adf_bri     <- adf.test(y_ts[, "BRI"])


#differencing variabel yang tidak stasioner
g_idr_diff <- diff(y_ts[,"IDR"], differences=1)
g_bca_diff <- diff(y_ts[,"BCA"], differences=1)

adf_idr_diff <- adf.test(g_idr_diff)
adf_idr_diff

adf_bca_diff <- adf.test(g_bca_diff)
adf_bca_diff

#update variabel y
y_ts_diff <- ts(cbind(
  g_idr = g_idr_diff, 
  g_bca = g_bca_diff,
  g_bri = y_ts[-1, "BRI"]  # Gunakan "BRI", dan sesuaikan panjang data
), 
start = c(2025, 2), 
frequency = 252)

# Pemilihan lag optimal 
lag_selection <- VARselect(y_ts, lag.max = 10, type = "const")
lag_selection

# Estimasi model VAR 
model_var <- VAR(y_ts, p = 3, type = "const")
summary(model_var)

# Fokus interpretasi: persamaan g_sales
summary(model_var)$varresult$IDR

# Uji Granger Causality 
# Apakah variabel lain membantu memprediksi growth IDR?
causality(model_var, cause = "BRI")
causality(model_var, cause = "BCA")

# Impulse Response Function / IRF 
# Melihat respons growth sales ketika terjadi shock pada variabel lain.
set.seed(123)
irf_bri <- irf(model_var, impulse = "BRI", response = "IDR",
               n.ahead = 8, boot = TRUE, ci = 0.95)
plot(irf_bri)


set.seed(123)
irf_bca <- irf(model_var, impulse = "BCA", response = "IDR",
                 n.ahead = 8, boot = TRUE, ci = 0.95)
plot(irf_bca)

# Forecast Error Variance Decomposition / FEVD 
# Melihat kontribusi masing-masing variabel terhadap variasi growth sales.
fevd_result <- fevd(model_var, n.ahead =15)
fevd_result
fevd_result$IDR

# Forecast 12 bulan ke depan 
forecast_result <- predict(model_var, n.ahead = 12, ci = 0.95)
forecast_result
plot(forecast_result)

