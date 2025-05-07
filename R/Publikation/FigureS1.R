#plot parameter

size_plot <- 35
lwd_line <- 1.5

# Definiere die gleichen y-Achsenlimits für alle Plots
common_ylim <- c(0, 650)  # Ändere diesen Bereich nach Bedarf entsprechend deinen Daten

# Funktion zur Anpassung der Y-Achse
function_plot_quartal <- function (Start_year, End_year, Title, legend_no) {

  load("data/cases_canton_monthly.RData")
  
  data_cases <- cases_canton_monthly %>%
    filter(Type_reporting =="Obligatory") %>%
    mutate(inc_cases = cases/population*100000,
           year_month = ymd(paste0(Year,"-",Month, "-01")),
           Quarter = as.factor(Month),
           Quarter  = recode(Quarter, 
                             "1"="1",
                             "2"="1",
                             "3"="3",
                             "4"="2",
                             "5"="2",
                             "6"="2",
                             "7"="3",
                             "8"="3",
                             "9"="3",
                             "10"="4",
                             "11"="4",
                             "12"="4")) %>%
    group_by(Year, Month, Canton2) %>%
    mutate(Quarter_cases = sum(cases)) %>%
    ungroup() 
  
  data_cases <- data_cases %>%
    distinct(Year, Month, Canton2,.keep_all = TRUE) %>%
    mutate( Quarter_inc = Quarter_cases/population*100000) %>%
    mutate(Year_quarter = paste0(Year,"-",Quarter),
           Year_month = ymd(paste0(Year,"-",Month, "-01")),
           id=1:nrow(.),
           Canton=as.factor(Canton)) %>%
    filter(Year >=Start_year & Year < End_year) 
  
  plot_Month <- ggplot()+
    geom_line(data=data_cases, aes(x=as.POSIXct(year_month), y= Quarter_inc, col=Canton2), lwd= lwd_line) +
    xlab("Year") +
    ylab("Incidence per 100,000 inhabitants") +
    scale_x_datetime(breaks = date_breaks("6 month"), labels = label_date_short()) +
    scale_color_manual("", values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
    ggtitle(Title) +
    ylim(common_ylim) +  # Feste y-Achse für alle Plots
    theme_bw() +
    theme(
      axis.line = element_line(),
      plot.margin = margin(10, 10, 10, 30),
      axis.text.y = element_text(size=size_plot),
      plot.title = element_text(size = size_plot),
      legend.position = "right",
      legend.key.size = unit(3.5, 'cm'),
      legend.spacing.x = unit(2.5, 'cm'),
      legend.text=element_text(size=size_plot),
      axis.text.x = element_text(size=size_plot-10),
      axis.title.x  = element_text(size=size_plot),
      axis.title.y  = element_text(size=size_plot)) 
  
  return(plot_Month)
}

# Erstellen der Plots für jedes Jahrzehnt
plot1 <- function_plot_quartal(Start_year=1910, End_year=1920, Title ="1910-1920")
plot2 <- function_plot_quartal(Start_year=1920, End_year=1930, Title ="1920-1930", legend_no="none")
plot3 <- function_plot_quartal(Start_year=1930, End_year=1940, Title ="1930-1940", legend_no="none")
plot4 <- function_plot_quartal(Start_year=1940, End_year=1950, Title ="1940-1950", legend_no="none")
plot5 <- function_plot_quartal(Start_year=1950, End_year=1960, Title ="1950-1960", legend_no="none")
plot6 <- function_plot_quartal(Start_year=1960, End_year=1970, Title ="1960-1970", legend_no="none")

# Aufteilung der Plots auf zwei Seiten (PDF)
# Erstellen der ersten Seite (mit den ersten 3 Plots)
Figure5_page1 <- cowplot::plot_grid(plot1, plot2, plot3, ncol=1)

# Erstellen der zweiten Seite (mit den restlichen 3 Plots)
Figure5_page2 <- cowplot::plot_grid(plot4, plot5, plot6, ncol=1)

# Speichern der beiden Seiten als separate PDF-Dateien oder aufeinanderfolgende Seiten in einer Datei
#cowplot::save_plot("output/Figure5_page1.pdf", Figure5_page1, base_height=40, base_width=30)
#cowplot::save_plot("output/Figure5_page2.pdf", Figure5_page2, base_height=40, base_width=30)

# Alternative: Beide Seiten in einem PDF speichern (mit grDevices)
pdf("output/Figure_Anhang1.pdf", height = 40, width = 30)
print(Figure5_page1)
print(Figure5_page2)
dev.off()

