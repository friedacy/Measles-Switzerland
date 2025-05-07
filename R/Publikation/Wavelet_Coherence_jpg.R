# Bibliotheken laden
library(strucchange)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(WaveletComp)
library(biwavelet)
library(grid)

load("data/cases_canton_monthly.RData")

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

# Erstellen eines leeren DataFrames
df_combined <- NULL

# Schleife über alle Kantone und die Inzidenzreihen als Spalten hinzufügen
for (Kt in cantons) {
  # Abrufen der Inzidenz-Zeitreihe für den aktuellen Kanton
  temp_df <- function_TS(Kt)
  
  # Umbenennen der Inzidenz-Spalte auf den jeweiligen Kantonsnamen
  colnames(temp_df)[2] <- Kt
  
  # Falls df_combined noch leer ist, initialisiere ihn mit der ersten Zeitreihe
  if (is.null(df_combined)) {
    df_combined <- temp_df
  } else {
    # Füge die neue Inzidenzreihe anhand des Datums (Merge) hinzu
    df_combined <- merge(df_combined, temp_df, by = "date", all = TRUE)
  }
}

# Erstelle alle möglichen einzigartigen Kantonspaare
canton_pairs <- combn(cantons, 2, simplify = FALSE)

# Stelle sicher, dass der Output-Ordner existiert
dir.create("output/Coherence", recursive = TRUE, showWarnings = FALSE)

# Schleife über alle Kantonspaare
for (pair in canton_pairs) {
  cat("Berechne Coherence für:", pair[1], "und", pair[2], "\n")
  
  # Berechne Cross-Wavelet power für das aktuelle Kantonspaar
  coh_result <- analyze.coherency(my.data = df_combined, 
                                  my.pair = pair, 
                                  loess.span = 0.75,
                                  dt = 1,
                                  make.pval = TRUE, method = "white.noise", params = NULL,
                                  n.sim = 10,
                                  date.format = "%Y-%m-%d", date.tz = NULL,
                                  verbose = FALSE, upperPeriod = 144 )
  
  # Dateiname für den Plot
  filename <- paste0("output/Coherence/wpavg_", pair[1], "_", pair[2], ".jpg")
  filename_image <- paste0("output/Coherence/wpimage_", pair[1], "_", pair[2], ".jpg")
  
  # Erstelle den Coherence-Plot als JPG
  jpeg(file = filename, width = 1500, height = 1000, quality = 90)
  par(cex.axis=1.8, cex.lab=1.8, cex.main=2) # Schrift grösser
  wc.avg(coh_result, which.avg = "wp",
         main = paste("Cross-wavelet power", pair[1], "&", pair[2]),
         show.siglvl = FALSE,
         legend.coords = "topleft",
         periodlab = "Period (months)",
         averagelab = "Average cross-wavelet power",
         spec.period.axis = list(at = c(2,6,12,18,24,36,48,60,72,96,120)),
         maximum.level = 0.4,
         show.legend = FALSE,
         lwd= 4, lwd.axis = 4)
  
  dev.off()
  
  jpeg(file = filename_image, width = 1500, height = 1000, quality = 90)
  par(cex.axis=1.8, cex.lab=1.8, cex.main=2) # Schrift grösser
  wc.image(coh_result, which.image = "wp",
           main = paste("Cross-wavelet power", pair[1], "&", pair[2]),
           periodlab = "Period (months)",
           maximum.level = 2.2,
           plot.arrow = FALSE,
           plot.contour = FALSE,
           plot.coi = FALSE,
           show.date = TRUE,  date.format = "%Y-%m-%d",timelab = "Time (years)",
           color.key = "i",
           lwd= 4, lwd.axis = 4,
           spec.period.axis = list(at = c(2,6,12,18,24,36,48,60,72,96,120)))
  dev.off()
  
  cat("Plot gespeichert:", filename, "\n")
}

