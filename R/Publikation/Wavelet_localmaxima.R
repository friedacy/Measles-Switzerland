# Wavelet analysis
library(WaveletComp)
library(pracma)
library(dplyr)

# Funktion zum Abrufen der Zeitreihe für einen bestimmten Kanton
function_TS <- function(Kt) {
  data_cases3 <- cases_canton_monthly %>%
    filter(Canton2 == Kt) %>%
    ungroup() %>%
    mutate(inc_cases = cases / population * 100000) %>%
    select(Datum, inc_cases) %>%
    rename(date = Datum)
  
  return(data_cases3)
}

# Kantone für die Analyse
cantons <- c("ZH", "BE", "LU", "BS", "SG", "GR", "GE")

# Layout für Plots
pdf(file = "output/Wavelet_all_cantons.pdf", width = 12, height = 8)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1) + 0.1)


### --- Teil für "Total of selected cantons" bleibt unverändert, wird aber erweitert ---

# Total ausgewählte Kantone
total_selected <- cases_canton_monthly %>%
  filter(Canton2 == "ZH") %>%  # Oder mehrere zusammenfassen falls gewünscht
  ungroup() %>%
  mutate(inc_cases = cases_r / pop_r * 100000) %>%
  select(Datum, inc_cases) %>%
  rename(date = Datum)

# Wavelet-Analyse
test_total <- analyze.wavelet(my.data = total_selected, my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")

# Plot
wt.avg(test_total, siglvl = 0.05, sigcol = "red", 
       main = "Total of selected cantons",
       periodlab = "Period (months)", 
       averagelab = "Average wavelet power", 
       show.legend = FALSE, 
       maximum.level = 0.45,
       spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)))

# Liste zum Speichern aller Peak-Daten
all_peaks <- list()

# Schleife über alle Kantone
for (Kt in cantons) {
  x <- function_TS(Kt)
  
  test <- analyze.wavelet(my.data = x, my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
  
  # Plot
  wt.avg(test, siglvl = 0.05, sigcol = "red", 
         main = paste("Canton:", Kt), 
         periodlab = "Period (months)", 
         averagelab = "Average wavelet power", 
         show.legend = FALSE,
         maximum.level = 0.45,
         spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)))
  
  # Speichern der avg. Power-Werte
  df <- data.frame(Period = test$Period, Power.avg = test$Power.avg)
  assign(paste0("Awp_", Kt), df)
  
  # Peak-Erkennung
  power_avg <- test$Power.avg
  periods <- test$Period
  peaks <- findpeaks(power_avg, minpeakheight = 0.05, minpeakdistance = 3)
  
  if (!is.null(peaks)) {
    peak_indices <- peaks[,2]
    peak_df <- data.frame(
      Canton = Kt,
      Period = periods[peak_indices],
      Power = power_avg[peak_indices]
    )
    assign(paste0("Peak_", Kt), peak_df)
    all_peaks[[Kt]] <- peak_df
  }
}

# PDF schließen
dev.off()

# Peak-Erkennung für total_selected
power_avg_total <- test_total$Power.avg
periods_total <- test_total$Period
peaks_total <- findpeaks(power_avg_total, minpeakheight = 0.05, minpeakdistance = 3)

if (!is.null(peaks_total)) {
  peak_indices_total <- peaks_total[,2]
  peak_df_total <- data.frame(
    Canton = "Total",
    Period = periods_total[peak_indices_total],
    Power = power_avg_total[peak_indices_total]
  )
  
  print(peak_df_total)
  
  # Auch in die Liste aufnehmen
  all_peaks[["Total"]] <- peak_df_total
}

# Gemeinsamer DataFrame für alle Peaks
all_peak_df <- do.call(rbind, all_peaks)

# Ausgabe
print(all_peak_df)
# write.csv(all_peak_df, "output/wavelet_peaks_all_cantons.csv", row.names = FALSE)
write.xlsx(all_peak_df, file ="output/wavelet_peaks_all_cantons.xlsx")
