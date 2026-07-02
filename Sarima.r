library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(lubridate)

data <- AirPassengers
data_ts <- ts(data, start= c(1949,1), frequency = 12)

plot(
  data_ts,
  main = "Air Passanger 1949-1960",
  xlab = "Tahun",
  ylab = "Passanger"
)

#-----------------------------------------------------
#train dan test
train_ts <- window(data_ts, start = c(1949,1), end = c(1959, 12))
test_ts  <- window(data_ts, start = c(1960,1), end = c(1960, 12))

length(train_ts)
length(test_ts)

# Visualisasi train dan test
autoplot(train_ts, series = "Train") +
  autolayer(test_ts, series = "Test/Aktual 1965") +
  ggtitle("Pembagian Data Train dan Test") +
  xlab("Tahun") +
  ylab("Sales") +
  guides(colour = guide_legend(title = "Data"))

# 6. Cek pola dekomposisi
# Dekomposisi membantu membaca trend dan seasonality.
decomp <- decompose(train_ts)
plot(decomp)

# 6. Uji Stasioneritas dalam Varians Box-Cox

# --------------------------------------------------------------
# 7. Uji stasioneritas dengan ADF pada data awal
# --------------------------------------------------------------
# Hipotesis ADF:
# H0: data tidak stasioner
# H1: data stasioner
adf_raw <- adf.test(train_ts)
adf_raw

# Interpretasi:
# Jika p-value > 0,05, data belum stasioner.
# Jika p-value <= 0,05, data cenderung stasioner.

# --------------------------------------------------------------
# 8. Transformasi differencing
# --------------------------------------------------------------
# Non-seasonal differencing: mengurangi tren.
diff_nonseasonal <- diff(train_ts, differences = 1)

# Seasonal differencing: mengurangi pola musiman tahunan.
diff_seasonal <- diff(train_ts, lag = 12, differences = 1)

# Gabungan non-seasonal dan seasonal differencing.
diff_combined <- diff(diff(train_ts, differences = 1), lag = 12, differences = 1)

# Plot hasil differencing
plot(diff_combined,
     main = "Data Setelah Non-Seasonal dan Seasonal Differencing",
     ylab = "Differenced Sales",
     xlab = "Tahun")

# Uji ADF setelah differencing gabungan
adf_diff <- adf.test(na.omit(diff_combined))
adf_diff

# --------------------------------------------------------------
# 9. Membaca ACF dan PACF
# --------------------------------------------------------------
# ACF membantu membaca kandidat q dan Q.
# PACF membantu membaca kandidat p dan P.
acf(diff_combined,
    main = "ACF Setelah Differencing")

pacf(diff_seasonal,
     main = "PACF Setelah Differencing")

# --------------------------------------------------------------
# 10. Mencoba beberapa kandidat model SARIMA
# --------------------------------------------------------------
# Catatan:
# order = c(p, d, q)
# seasonal = c(P, D, Q)
# period = 12 karena data bulanan.
model_011_011 <- Arima(train_ts,
                       order = c(0, 1, 1),
                       seasonal = c(0, 1, 1),
                       method = "ML")

model_110_110 <- Arima(train_ts,
                       order = c(1, 1, 0),
                       seasonal = c(1, 1, 0),
                       method = "ML")

model_012_011 <- Arima(train_ts,
                       order = c(0, 1, 2),
                       seasonal = c(0, 1, 1),
                       method = "ML")

model_112_110 <- Arima(train_ts,
                       order = c(1, 1, 2),
                       seasonal = c(1, 1, 0),
                       method = "ML")

model_212_110 <- Arima(train_ts,
                       order = c(2, 1, 2),
                       seasonal = c(1, 1, 0),
                       method = "ML")

# --------------------------------------------------------------
# 11. Membuat fungsi evaluasi model
# --------------------------------------------------------------
evaluate_model <- function(model, model_name, test_data) {
  fc <- forecast(model, h = length(test_data))
  acc <- accuracy(fc, test_data)

  data.frame(
    Model = model_name,
    AIC = AIC(model),
    BIC = BIC(model),
    MAE = acc["Test set", "MAE"],
    RMSE = acc["Test set", "RMSE"],
    MAPE = acc["Test set", "MAPE"]
  )
}

hasil_model <- bind_rows(
  evaluate_model(model_011_011, "SARIMA(0,1,1)(0,1,1)[12]", test_ts),
  evaluate_model(model_110_110, "SARIMA(1,1,0)(1,1,0)[12]", test_ts),
  evaluate_model(model_012_011, "SARIMA(0,1,2)(0,1,1)[12]", test_ts),
  evaluate_model(model_112_110, "SARIMA(1,1,2)(1,1,0)[12]", test_ts),
  evaluate_model(model_212_110, "SARIMA(2,1,2)(1,1,0)[12]", test_ts)
)

hasil_model <- hasil_model %>%
  arrange(RMSE)

print(hasil_model)

# Simpan tabel perbandingan model
write_csv(hasil_model, "output_perbandingan_model_sarima.csv")

# --------------------------------------------------------------
# 12. Memilih model terbaik
# --------------------------------------------------------------
# Sesuai studi kasus dalam PPT, model praktik:
# SARIMA(2,1,2)(1,1,0)[12]
best_model <- model_212_110

summary(best_model)

# --------------------------------------------------------------
# 13. Diagnostik residual
# --------------------------------------------------------------
# Residual yang baik cenderung acak dan tidak menyimpan pola kuat.
checkresiduals(best_model)

# Uji Ljung-Box
Box.test(residuals(best_model),
         lag = 12,
         type = "Ljung-Box",
         fitdf = 5)

# Interpretasi:
# Jika p-value > 0,05, residual relatif tidak memiliki autokorelasi kuat.
# Jika p-value <= 0,05, model masih menyisakan pola yang perlu diperbaiki.

# --------------------------------------------------------------
# 14. Forecast 12 bulan untuk tahun 1965
# --------------------------------------------------------------
forecast_1965 <- forecast(best_model, h = 12)

autoplot(forecast_1965) +
  autolayer(test_ts, series = "Aktual 1965") +
  ggtitle("Forecast SARIMA vs Aktual 1965") +
  xlab("Tahun") +
  ylab("Sales") +
  guides(colour = guide_legend(title = "Keterangan"))

# --------------------------------------------------------------
# 15. Membuat tabel forecast
# --------------------------------------------------------------
forecast_table <- data.frame(
  Bulan = seq(as.Date("1965-01-01"), by = "month", length.out = 12),
  Aktual = as.numeric(test_ts),
  Forecast = as.numeric(forecast_1965$mean),
  Lower_95 = as.numeric(forecast_1965$lower[, "95%"]),
  Upper_95 = as.numeric(forecast_1965$upper[, "95%"])
)

forecast_table <- forecast_table %>%
  mutate(
    Error = Aktual - Forecast,
    Absolute_Error = abs(Error),
    Percentage_Error = abs(Error / Aktual) * 100
  )

print(forecast_table)

# Simpan output forecast
write_csv(forecast_table, "output_forecast_1965.csv")

# --------------------------------------------------------------
# 16. Menghitung akurasi akhir
# --------------------------------------------------------------
akurasi_final <- accuracy(forecast_1965, test_ts)
print(akurasi_final)

# --------------------------------------------------------------
# 17. Auto ARIMA sebagai pembanding
# --------------------------------------------------------------
# Bagian ini opsional untuk menunjukkan model otomatis.
auto_model <- auto.arima(train_ts,
                         seasonal = TRUE,
                         stepwise = FALSE,
                         approximation = FALSE)

summary(auto_model)
forecast_auto <- forecast(auto_model, h = 12)
accuracy(forecast_auto, test_ts)

autoplot(forecast_auto) +
  autolayer(test_ts, series = "Aktual 1965") +
  ggtitle("Forecast Auto ARIMA vs Aktual 1965") +
  xlab("Tahun") +
  ylab("Sales") +
  guides(colour = guide_legend(title = "Keterangan"))

# --------------------------------------------------------------
# 18. Contoh narasi interpretasi
# --------------------------------------------------------------
# Model SARIMA digunakan karena data penjualan bulanan memiliki pola trend
# dan musiman. Setelah dilakukan differencing non-musiman dan musiman,
# data menjadi lebih layak dimodelkan. Forecast tahun 1965 kemudian
# dibandingkan dengan data aktual untuk menilai kemampuan prediksi model.
# Nilai RMSE dan MAPE digunakan untuk membaca besar kesalahan prediksi.
