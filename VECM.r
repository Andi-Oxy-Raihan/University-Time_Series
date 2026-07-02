# install.packages(c("readr","dplyr","lubridate","tseries","urca",
#                    "vars","tsDyn","ggplot2","gridExtra","lmtest",
#                    "nortest","forecast","zoo"))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(tseries)
  library(urca)
  library(vars)
  library(tsDyn)
  library(ggplot2)
  library(gridExtra)
  library(lmtest)
  library(nortest)
  library(forecast)
  library(zoo)
})

# 1. IMPORT DATA

# Jalankan baris ini, lalu pilih file Anda lewat jendela yang muncul
file_path <- file.choose()

# Setelah dipilih, baca datanya
Data <- read_csv(file_path)

# Cek apakah sudah berhasil
head(Data)

# Baca CSV sebagai teks dulu agar semua kolom aman
raw_text <- read_csv(
  file_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("✔ Data berhasil dibaca:", nrow(raw_text), "baris,",
    ncol(raw_text), "kolom.\n")
cat("Kolom tersedia:", paste(names(raw_text), collapse = ", "), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. PREPROCESSING
# ─────────────────────────────────────────────────────────────────────────────

# 2a. Fungsi konversi angka format Indonesia (koma = desimal, titik = ribuan)
parse_id_number <- function(x) {
  x_clean <- gsub("\\.", "", x)          # hapus titik ribuan
  x_clean <- gsub(",", ".", x_clean)    # ganti koma desimal → titik
  as.numeric(x_clean)
}

# 2b. Parsing tanggal & konversi kolom numerik
kolom_angka <- c("IPR", "Inflasi", "BI_Rate", "KURS", "M2",
                 "LN_IPR", "LN_KURS", "LN_M2")

df <- raw_text %>%
  mutate(
    Date = as.Date(as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S")),
    across(all_of(kolom_angka), parse_id_number)
  ) %>%
  arrange(Date)

# 2c. Cek missing values
cat("\n--- Missing Values per Kolom ---\n")
print(colSums(is.na(df)))

# 2d. Pilih variabel model
vars_model <- c("LN_IPR", "Inflasi", "BI_Rate", "LN_KURS", "LN_M2")

df_model <- df[, c("Date", vars_model)]
df_model <- na.omit(df_model)

# Reset rownames setelah na.omit
rownames(df_model) <- NULL

cat("\n✔ Data model siap.")
cat("\nJumlah observasi bersih :", nrow(df_model))
cat("\nPeriode                  :", format(min(df_model$Date), "%b %Y"),
    "–", format(max(df_model$Date), "%b %Y"), "\n")

# Tampilkan ringkasan statistik
cat("\n--- Statistik Deskriptif ---\n")
print(summary(df_model[, vars_model]))

# 2e. Buat objek multivariate time series (mts)
ts_data <- ts(
  df_model[, vars_model],
  start     = c(year(min(df_model$Date)), month(min(df_model$Date))),
  frequency = 12
)


# ─────────────────────────────────────────────────────────────────────────────
# 3. VISUALISASI AWAL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== VISUALISASI TIME SERIES ===\n")

par(mfrow = c(3, 2), mar = c(3, 3, 2.5, 1))
for (v in vars_model) {
  plot(df_model$Date, df_model[[v]],
       type = "l", col = "steelblue", lwd = 1.5,
       main = paste("Plot Level:", v),
       xlab = "Waktu", ylab = v)
  abline(h   = mean(df_model[[v]], na.rm = TRUE),
         col = "red", lty = 2, lwd = 1)
  legend("topleft", legend = "Rata-rata", col = "red",
         lty = 2, cex = 0.7, bty = "n")
}
par(mfrow = c(1, 1))

# Plot ACF level
par(mfrow = c(3, 2), mar = c(3, 3, 2.5, 1))
for (v in vars_model) {
  acf(df_model[[v]], main = paste("ACF:", v), lag.max = 24)
}
par(mfrow = c(1, 1))


# ─────────────────────────────────────────────────────────────────────────────
# 4. UJI STASIONERITAS
# ─────────────────────────────────────────────────────────────────────────────

# ── 4a. Stasioneritas dalam VARIANS (Bartlett Test per segmen) ───────────────
cat("\n=== UJI STASIONERITAS DALAM VARIANS (Bartlett per segmen) ===\n")

bartlett_test_ts <- function(x, n_seg = 4) {
  seg <- cut(seq_along(x), breaks = n_seg, labels = FALSE)
  bartlett.test(x ~ seg)
}

hasil_bartlett <- data.frame(
  Variabel  = character(),
  Statistic = numeric(),
  p_value   = numeric(),
  Keputusan = character(),
  stringsAsFactors = FALSE
)

for (v in vars_model) {
  bt  <- bartlett_test_ts(df_model[[v]])
  kep <- ifelse(bt$p.value < 0.05,
                "Varians Tidak Homogen (Tidak Stasioner)",
                "Varians Homogen (Stasioner)")
  hasil_bartlett <- rbind(hasil_bartlett, data.frame(
    Variabel  = v,
    Statistic = round(bt$statistic, 4),
    p_value   = round(bt$p.value, 4),
    Keputusan = kep,
    stringsAsFactors = FALSE
  ))
}
print(hasil_bartlett, row.names = FALSE)

# ── 4b. Stasioneritas dalam RATA-RATA (ADF Test) ─────────────────────────────
cat("\n=== UJI ADF – LEVEL ===\n")

hasil_adf_level <- data.frame(
  Variabel  = character(),
  ADF_stat  = numeric(),
  p_value   = numeric(),
  Keputusan = character(),
  stringsAsFactors = FALSE
)

for (v in vars_model) {
  adf <- adf.test(df_model[[v]], alternative = "stationary")
  kep <- ifelse(adf$p.value < 0.05, "STASIONER ✔", "TIDAK STASIONER ✗")
  hasil_adf_level <- rbind(hasil_adf_level, data.frame(
    Variabel  = v,
    ADF_stat  = round(adf$statistic, 4),
    p_value   = round(adf$p.value, 4),
    Keputusan = kep,
    stringsAsFactors = FALSE
  ))
}
print(hasil_adf_level, row.names = FALSE)

cat("\n=== UJI ADF – FIRST DIFFERENCE ===\n")

hasil_adf_diff <- data.frame(
  Variabel  = character(),
  ADF_stat  = numeric(),
  p_value   = numeric(),
  Keputusan = character(),
  stringsAsFactors = FALSE
)

for (v in vars_model) {
  d_x <- diff(df_model[[v]])
  adf <- adf.test(d_x, alternative = "stationary")
  kep <- ifelse(adf$p.value < 0.05, "STASIONER ✔", "TIDAK STASIONER ✗")
  hasil_adf_diff <- rbind(hasil_adf_diff, data.frame(
    Variabel  = paste0("D.", v),
    ADF_stat  = round(adf$statistic, 4),
    p_value   = round(adf$p.value, 4),
    Keputusan = kep,
    stringsAsFactors = FALSE
  ))
}
print(hasil_adf_diff, row.names = FALSE)

# ── 4c. Konfirmasi ur.df (dengan trend) ──────────────────────────────────────
cat("\n=== UJI ADF URCA – LEVEL (trend & drift) ===\n")
for (v in vars_model) {
  cat("\n--- Variabel:", v, "---\n")
  adf_urca <- ur.df(df_model[[v]], type = "trend", selectlags = "AIC")
  print(summary(adf_urca))
}

# ── 4d. Visualisasi first difference ─────────────────────────────────────────
par(mfrow = c(3, 2), mar = c(3, 3, 2.5, 1))
for (v in vars_model) {
  plot(diff(df_model[[v]]),
       type = "l", col = "darkorange", lwd = 1.5,
       main = paste("First Difference:", v),
       xlab = "Index", ylab = paste0("Δ", v))
  abline(h = 0, col = "red", lty = 2)
}
par(mfrow = c(1, 1))


# ─────────────────────────────────────────────────────────────────────────────
# 5. MENENTUKAN LAG OPTIMAL (VAR)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== LAG SELECTION CRITERIA ===\n")
lag_select <- VARselect(ts_data, lag.max = 12, type = "const")
print(lag_select$selection)

# Ambil lag dari AIC; jika hasilnya > 4, gunakan SC sebagai alternatif
p_aic <- lag_select$selection["AIC(n)"]
p_sc  <- lag_select$selection["SC(n)"]
cat(sprintf("Lag AIC: %d | Lag SC: %d\n", p_aic, p_sc))

# Gunakan lag AIC, tapi batasi maks 4 untuk efisiensi
# ca.jo mensyaratkan K >= 2, jadi pastikan minimal 2
p_opt <- max(min(p_aic, 4), 2)
cat("Lag optimal yang digunakan:", p_opt, "\n")
cat("  (ca.jo mensyaratkan K >= 2; nilai di bawah 2 otomatis dinaikkan)\n")


# ─────────────────────────────────────────────────────────────────────────────
# 6. UJI KOINTEGRASI JOHANSEN
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== UJI KOINTEGRASI JOHANSEN (Trace Test) ===\n")
joh_trace <- ca.jo(
  ts_data,
  type  = "trace",
  ecdet = "const",
  K     = p_opt,
  spec  = "longrun"
)
print(summary(joh_trace))

cat("\n=== UJI KOINTEGRASI JOHANSEN (Max-Eigen Test) ===\n")
joh_eigen <- ca.jo(
  ts_data,
  type  = "eigen",
  ecdet = "const",
  K     = p_opt,
  spec  = "longrun"
)
print(summary(joh_eigen))

# ── Tentukan rank kointegrasi r ───────────────────────────────────────────────
# Cara baca: bandingkan Test Stat dengan Critical Value (10pct / 5pct / 1pct)
# Jika Test Stat > CV → tolak H0 (ada kointegrasi pada rank tersebut)
# r = jumlah baris H0 yang berhasil ditolak, mulai dari r=0

# ▶▶ SESUAIKAN NILAI r INI BERDASARKAN OUTPUT JOHANSEN DI ATAS ◀◀
r <- 1

cat(sprintf("\n✔ Rank kointegrasi yang digunakan: r = %d\n", r))
cat("  (Ubah nilai r di atas jika output Johansen menunjukkan angka berbeda)\n")


# ─────────────────────────────────────────────────────────────────────────────
# 7. ESTIMASI VECM
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== ESTIMASI VECM (via cajorls) ===\n")
vecm_model <- cajorls(joh_trace, r = r)
print(vecm_model)

cat("\n=== ESTIMASI VECM (via tsDyn – lebih detail) ===\n")
vecm_tsdyn <- VECM(
  ts_data,
  lag     = max(p_opt - 1, 1),   # VECM lag = VAR lag - 1, minimal 1
  r       = r,
  estim   = "ML",
  include = "const"
)
print(summary(vecm_tsdyn))


# ─────────────────────────────────────────────────────────────────────────────
# 8. MEMBACA ERROR CORRECTION TERM (ECT)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== ERROR CORRECTION TERM (ECT) ===\n")

# Ekstrak koefisien alpha (kecepatan penyesuaian) dari cajorls
ect_coef <- tryCatch(
  vecm_model$rlm$coefficients["ect1", ],
  error = function(e) {
    cat("  [Info] Nama baris ECT berbeda; mencoba 'ect1'...\n")
    rownames_coef <- rownames(vecm_model$rlm$coefficients)
    ect_row       <- rownames_coef[grepl("^ect", rownames_coef)][1]
    vecm_model$rlm$coefficients[ect_row, ]
  }
)

cat("\nKoefisien ECT (alpha – kecepatan penyesuaian per variabel):\n")
print(round(ect_coef, 6))

cat("\n[Panduan Interpretasi ECT]")
cat("\n  • Tanda NEGATIF  → variabel bergerak kembali menuju keseimbangan ✔")
cat("\n  • Tanda POSITIF  → variabel menjauhi keseimbangan (perlu diperiksa)")
cat("\n  • |nilai|         → proporsi penyimpangan yang dikoreksi per bulan")
cat("\n    Contoh: -0.25 → 25% penyimpangan dikoreksi setiap bulan")
cat("\n  • Cek signifikansi via summary(vecm_tsdyn) di atas\n")


# ─────────────────────────────────────────────────────────────────────────────
# 9. KONVERSI VECM → VAR (untuk IRF, FEVD, Diagnostik)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== KONVERSI VECM → VAR ===\n")
var_from_vecm <- vec2var(joh_trace, r = r)
cat("✔ Konversi berhasil.\n")


# ─────────────────────────────────────────────────────────────────────────────
# 10. IMPULSE RESPONSE FUNCTION (IRF)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== IMPULSE RESPONSE FUNCTION (IRF) ===\n")
cat("  Menghitung IRF 24 bulan dengan 500 bootstrap... (mungkin perlu beberapa detik)\n")

irf_result <- irf(
  var_from_vecm,
  n.ahead = 24,
  boot    = TRUE,
  ci      = 0.95,
  runs    = 500
)

# Plot IRF per variabel impulse
for (v in vars_model) {
  plot(irf(var_from_vecm, impulse = v, n.ahead = 24,
           boot = TRUE, ci = 0.95, runs = 200),
       main = paste("IRF – Impulse dari:", v))
}


# ─────────────────────────────────────────────────────────────────────────────
# 11. FORECAST ERROR VARIANCE DECOMPOSITION (FEVD)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== FORECAST ERROR VARIANCE DECOMPOSITION (FEVD) ===\n")
fevd_result <- fevd(var_from_vecm, n.ahead = 24)

# Tampilkan tabel FEVD untuk setiap variabel
for (v in vars_model) {
  cat(sprintf("\n--- FEVD untuk: %s ---\n", v))
  tabel_fevd <- round(fevd_result[[v]] * 100, 2)   # dalam persen
  # Ambil periode kunci: 1, 3, 6, 12, 24 bulan
  idx <- pmin(c(1, 3, 6, 12, 24), nrow(tabel_fevd))
  print(tabel_fevd[idx, ])
}

plot(fevd_result, addbars = 4)


# ─────────────────────────────────────────────────────────────────────────────
# 12. UJI KAUSALITAS GRANGER (PAIRWISE)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== UJI KAUSALITAS GRANGER (Pairwise) ===\n")

hasil_granger <- data.frame(
  Cause     = character(),
  Effect    = character(),
  F_stat    = numeric(),
  p_value   = numeric(),
  Keputusan = character(),
  stringsAsFactors = FALSE
)

for (cause_var in vars_model) {
  for (effect_var in vars_model) {
    if (cause_var == effect_var) next
    
    gc <- tryCatch(
      grangertest(
        df_model[[effect_var]] ~ df_model[[cause_var]],
        order = p_opt
      ),
      error = function(e) NULL
    )
    
    if (!is.null(gc)) {
      p_val  <- gc$`Pr(>F)`[2]
      f_val  <- gc$F[2]
      kep    <- ifelse(p_val < 0.05,
                       "Signifikan ✔ (Granger-cause)",
                       "Tidak Signifikan ✗")
      hasil_granger <- rbind(hasil_granger, data.frame(
        Cause     = cause_var,
        Effect    = effect_var,
        F_stat    = round(f_val, 4),
        p_value   = round(p_val, 4),
        Keputusan = kep,
        stringsAsFactors = FALSE
      ))
    }
  }
}

print(hasil_granger, row.names = FALSE)


# ─────────────────────────────────────────────────────────────────────────────
# 13. UJI DIAGNOSTIK MODEL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== UJI DIAGNOSTIK MODEL ===\n")

# 13a. Autokorelasi residual (Portmanteau)
cat("\n--- [1] Autokorelasi Residual (Portmanteau) ---\n")
cat("H0: Tidak ada autokorelasi → gagal tolak jika p-value > 0.05\n")
serial_test <- tryCatch(
  serial.test(var_from_vecm, lags.pt = 12, type = "PT.asymptotic"),
  error = function(e) {
    cat("  [Warning] serial.test gagal:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(serial_test)) print(serial_test)

# 13b. Normalitas residual (Jarque-Bera multivariat)
cat("\n--- [2] Normalitas Residual (Jarque-Bera) ---\n")
cat("H0: Residual berdistribusi normal\n")
norm_test <- tryCatch(
  normality.test(var_from_vecm),
  error = function(e) {
    cat("  [Warning] normality.test gagal:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(norm_test)) print(norm_test)

# 13c. Heteroskedastisitas (ARCH-LM)
cat("\n--- [3] Heteroskedastisitas ARCH ---\n")
cat("H0: Tidak ada efek ARCH (varians konstan)\n")
arch_test <- tryCatch(
  arch.test(var_from_vecm, lags.multi = 5),
  error = function(e) {
    cat("  [Warning] arch.test gagal:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(arch_test)) print(arch_test)

# 13d. Stabilitas model (AR roots)
cat("\n--- [4] Stabilitas Model (Modulus Akar) ---\n")
cat("Syarat stabil: semua modulus < 1\n")
roots_var <- tryCatch(
  roots(var_from_vecm),
  error = function(e) NULL
)
if (!is.null(roots_var)) {
  cat("Modulus akar:\n")
  print(round(Mod(roots_var), 6))
  n_unstable <- sum(Mod(roots_var) >= 1 - 1e-6)
  cat("Akar dengan modulus ≥ 1 :", n_unstable, "\n")
  cat("Status model             :",
      ifelse(n_unstable == 0, "STABIL ✔", "TIDAK STABIL – periksa spesifikasi!"), "\n")
} else {
  cat("[Info] Stabilitas tidak dapat dihitung otomatis.\n")
  cat("       Periksa manual: semua eigenvalue |λ| harus < 1.\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# 14. FORECASTING
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== FORECASTING 12 BULAN KE DEPAN ===\n")
forecast_result <- predict(var_from_vecm, n.ahead = 12, ci = 0.95)

# Plot prediksi dengan garis historis
plot(forecast_result)

# Tampilkan tabel prediksi rapi per variabel
cat("\n--- Tabel Prediksi 12 Bulan ---\n")

# Buat tanggal prediksi
last_date    <- max(df_model$Date)
future_dates <- seq.Date(
  from = ceiling_date(last_date, "month"),
  by   = "month",
  length.out = 12
)

for (v in vars_model) {
  fc_mat <- forecast_result$fcst[[v]]
  fc_df  <- data.frame(
    Periode  = format(future_dates, "%b %Y"),
    Prediksi = round(fc_mat[, "fcst"],  4),
    Lower_95 = round(fc_mat[, "lower"], 4),
    Upper_95 = round(fc_mat[, "upper"], 4),
    row.names = NULL
  )
  cat(sprintf("\n▶ Variabel: %s\n", v))
  print(fc_df)
}


# ─────────────────────────────────────────────────────────────────────────────
# 15. RINGKASAN AKHIR
# ─────────────────────────────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║           RINGKASAN HASIL ANALISIS VECM                 ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ Variabel    : %-44s║\n",
            paste(vars_model, collapse = ", ")))
cat(sprintf("║ Observasi   : %-44s║\n", nrow(df_model)))
cat(sprintf("║ Periode     : %s – %-32s║\n",
            format(min(df_model$Date), "%b %Y"),
            format(max(df_model$Date), "%b %Y")))
cat(sprintf("║ Lag Optimal : %-44s║\n", p_opt))
cat(sprintf("║ Rank (r)    : %-44s║\n", r))
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Tahap selesai: Import → Preprocessing → Stasioneritas   ║\n")
cat("║                → Lag → Johansen → VECM → ECT → IRF      ║\n")
cat("║                → FEVD → Granger → Diagnostik → Forecast ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")
