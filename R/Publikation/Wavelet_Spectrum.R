#Wavelet analysis
library(WaveletComp)

# Funktion zum Abrufen der Zeitreihe für einen bestimmten Kanton
function_TS <- function(Kt) {
  data_cases3 <- cases_canton_monthly %>%
    filter(Canton2 == Kt) %>%
    ungroup() %>%
    mutate(inc_cases = cases / population * 100000) %>%
    select(Datum, inc_cases)%>%
    rename(date = Datum)
  
  return(data_cases3)
}

#Daten für das Spektrum aller ausgewählten Kantone
total_selected <- cases_canton_monthly %>%
  filter(Canton2 == "ZH") %>%
  ungroup() %>%
  mutate(inc_cases = cases_r / pop_r * 100000) %>%
  select(Datum, inc_cases)%>%
  rename(date = Datum)


# Kantone für die Analyse
cantons <- c("ZH", "BE", "LU", "BS", "SG", "GR", "GE")

spec_ZH <- analyze.wavelet(my.data = function_TS("ZH"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_BE <- analyze.wavelet(my.data = function_TS("BE"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_LU <- analyze.wavelet(my.data = function_TS("LU"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_BS <- analyze.wavelet(my.data = function_TS("BS"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_SG <- analyze.wavelet(my.data = function_TS("SG"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_GR <- analyze.wavelet(my.data = function_TS("GR"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
spec_GE <- analyze.wavelet(my.data = function_TS("GE"), my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")

spec_7 <- analyze.wavelet(my.data= total_selected, my.series="inc_cases",loess.span = 0.75, dt= 1, date.format = "%Y-%m-%d")

max_power <- max(spec_ZH$Power, spec_BE$Power, spec_LU$Power, spec_BS$Power, spec_SG$Power, spec_GR$Power, spec_GE$Power, na.rm = TRUE)

#Spectrum ZH
pdf(file = "output/Wavelet_Spectrum_ZH.pdf", width = 15, height = 10)
wt.image(spec_ZH, color.key = "interval", main = "Wavelet power spectrum Zurich", spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum BE
pdf(file = "output/Wavelet_Spectrum_BE.pdf", width = 15, height = 10)
wt.image(spec_BE, color.key = "interval", main = "Wavelet power spectrum Bern",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum LU
pdf(file = "output/Wavelet_Spectrum_LU.pdf", width = 15, height = 10)
wt.image(spec_LU, color.key = "interval", main = "Wavelet power spectrum Lucerne",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum BS
pdf(file = "output/Wavelet_Spectrum_BS.pdf", width = 15, height = 10)
wt.image(spec_BS, color.key = "interval", main = "Wavelet power spectrum Basel-City",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum SG
pdf(file = "output/Wavelet_Spectrum_SG.pdf", width = 15, height = 10)
wt.image(spec_SG, color.key = "interval", main = "Wavelet power spectrum St. Gallen",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum GR
pdf(file = "output/Wavelet_Spectrum_GR.pdf", width = 15, height = 10)
wt.image(spec_GR, color.key = "interval", main = "Wavelet power spectrum Grisons",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum GE
pdf(file = "output/Wavelet_Spectrum_GE.pdf", width = 15, height = 10)
wt.image(spec_GE, color.key = "interval", main = "Wavelet power spectrum Geneva",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

#Spectrum 7

pdf(file = "output/Wavelet_Spectrum_7.pdf", width = 15, height = 10)
wt.image(spec_7, color.key = "interval", main = "Wavelet power spectrum total selected cantons",spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (months)", plot.coi = FALSE, plot.contour = FALSE, plot.ridge = FALSE, show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
dev.off()

# Funktion zum Speichern von Wavelet-Plots als JPEG
save_wavelet_plot <- function(spec, filename, title) {
  jpeg(file = filename, width = 1500, height = 1000, res = 150)
  wt.image(spec, color.key = "interval", 
           main = title, 
           spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)),
           legend.params = list(lab = "wavelet power levels"),
           periodlab = "Period (months)", plot.coi = FALSE, 
           plot.contour = FALSE, plot.ridge = FALSE, 
           show.date = TRUE, timelab = "Time (years)", maximum.level = max_power)
  dev.off()
}

# Speichern der einzelnen Wavelet-Plots als JPEG
save_wavelet_plot(spec_ZH, "output/Wavelet_Spectrum_ZH.jpeg", "Wavelet power spectrum Zurich")
save_wavelet_plot(spec_BE, "output/Wavelet_Spectrum_BE.jpeg", "Wavelet power spectrum Bern")
save_wavelet_plot(spec_LU, "output/Wavelet_Spectrum_LU.jpeg", "Wavelet power spectrum Lucerne")
save_wavelet_plot(spec_BS, "output/Wavelet_Spectrum_BS.jpeg", "Wavelet power spectrum Basel-City")
save_wavelet_plot(spec_SG, "output/Wavelet_Spectrum_SG.jpeg", "Wavelet power spectrum St. Gallen")
save_wavelet_plot(spec_GR, "output/Wavelet_Spectrum_GR.jpeg", "Wavelet power spectrum Grisons")
save_wavelet_plot(spec_GE, "output/Wavelet_Spectrum_GE.jpeg", "Wavelet power spectrum Geneva")
save_wavelet_plot(spec_7, "output/Wavelet_Spectrum_7.jpeg", "Wavelet power spectrum total selected cantons")

