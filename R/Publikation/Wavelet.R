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

# Kantone für die Analyse
cantons <- c("ZH", "BE", "LU", "BS", "SG", "GR", "GE")


# Definiere die Layout-Einstellungen für die Plots (hier 2x4, weil 7 Kantone)
pdf(file = "output/Wavelet_all_cantons.pdf", width = 12, height = 8)

# Setze das Layout für die Plots: z.B., 2 Zeilen und 4 Spalten
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1) + 0.1)

# Schleife über alle ausgewählten Kantone
for (Kt in cantons) {
  # Abrufen der Inzidenz-Zeitreihe für den aktuellen Kanton
  x <- function_TS(Kt)
  
  # Durchführung der Wavelet-Analyse für den aktuellen Kanton
  test <- analyze.wavelet(my.data = x, my.series = "inc_cases", loess.span = 0.75, dt = 1, date.format = "%Y-%m-%d")
  
  # Plot der durchschnittlichen Wavelet-Power für den aktuellen Kanton
  wt.avg(test, siglvl = 0.05, sigcol = "red", 
         main = paste("Canton:", Kt), 
         periodlab = "Period (months)", 
         averagelab = "Average wavelet power", 
         show.legend = FALSE,
         maximum.level = 0.45,
         spec.period.axis = list(at = c(2, 6, 12, 18, 24, 36, 48, 60, 72, 96, 120)))
  # Erstelle einen DataFrame mit Power.avg und Period
  df <- data.frame(Period = test$Period, Power.avg = test$Power.avg)
  
  # Speichere den DataFrame unter dem Namen des Kantons + "_awp"
  assign(paste0("Awp_", Kt), df)
}

#Ausgewählte Kantone
total_selected <- cases_canton_monthly %>%
  filter(Canton2 == "ZH") %>%
  ungroup() %>%
  mutate(inc_cases = cases_r / pop_r * 100000) %>%
  select(Datum, inc_cases)%>%
  rename(date = Datum)

test <- analyze.wavelet(my.data= total_selected, my.series="inc_cases",loess.span = 0.75, dt= 1, date.format = "%Y-%m-%d")
wt.avg(test, siglvl = 0.05, sigcol = "red", main= "Total of selected cantons",
       periodlab = "Period (months)", averagelab= "Average wavelet power", show.legend = FALSE, maximum.level = 0.45, spec.period.axis = list(at= c(2,6,12,18,24,36,48,60,72,96,120)))
# Schließen des PDF-Dokuments
dev.off()

