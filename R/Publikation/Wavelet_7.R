# Bibliotheken
library(WaveletComp)

# Ausgewählte Kantone
total_selected <- cases_canton_monthly %>%
  filter(Canton2 == "ZH") %>%
  ungroup() %>%
  mutate(inc_cases = cases_r / pop_r * 100000) %>%
  select(Datum, inc_cases) %>%
  rename(date = Datum)

# Wavelet-Analyse durchführen
test <- analyze.wavelet(my.data = total_selected, my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")

# Speichern des wt.image Plots in einer PDF-Datei
pdf(file = "output/Wavelet_Power_Spectrum_7.pdf", width = 12, height = 8)
wt.image(test, color.key = "interval", main = "Wavelet power spectrum of all selected cantons", spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)")
dev.off()  # Schließt die PDF-Datei

# Speichern des wt.avg Plots in einer PDF-Datei
pdf(file = "output/Average_Wavelet_Power.pdf", width = 12, height = 8)
wt.avg(test, siglvl = 0.05, sigcol = "red", main = "Selected Cantons Overall",
       periodlab = "Period (months)", averagelab = "Average wavelet Power", show.legend = FALSE, 
       spec.period.axis = list(at = c(2,6,12,18,24,33,36,48,60,72,96,120)))
dev.off()  # Schließt die PDF-Datei
