library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

population_u5 <- read.csv("data_raw/population_hmdb.txt", header=TRUE, sep="") %>%
  mutate(Age= recode(Age, "110+" = "110"),
         Age= as.integer(Age),
         age_group = case_when(Age<=4 ~ "<5",
                               Age>=5 ~ ">=5")) %>%
  select(Year, age_group, Total) %>%
  group_by(Year, age_group) %>%
  mutate(pop_age = sum(Total)) %>%
  distinct(Year, age_group, .keep_all = TRUE) %>%
  ungroup()%>%
  select(-Total)%>%
  mutate(pop_age= round(pop_age)) %>%
  group_by(Year)%>%
  filter(age_group == "<5")%>%  # Nur Einträge mit age_group "<5"
  filter(Year >= 1910 & Year <= 1974)

# Excel-Datei importieren
add_data <- read_excel("data_raw/add_data.xlsx")

# Sicherstellen, dass die "Year"-Spalte im richtigen Format vorliegt (integer)
add_data <- add_data %>%
  mutate(Year = as.integer(Year))

# Join mit den neuen Daten auf Basis der Year-Spalte
add_data <- left_join(population_u5, add_data, by = "Year")
# Spalte umbenennen und age_group ausblenden
add_data <- add_data %>%
  rename(pop_u5 = pop_age) %>%   # Umbenennen der Spalte
  mutate(pop_u5 = pop_u5/Population*100)%>%
  select(-age_group)            # Entfernen der age_group Spalte

#Inzidenzzahlen importieren & add_data hinzufügen
Total_inc <- cases_canton_yearly%>%
  filter(Canton == "Total")%>%
  filter(Year >= 1910 & Year <= 1974)%>%
  mutate(inc_cases = cases_year/population*100000)%>%
  select(Year, inc_cases)

#Mortalitätsdaten importieren und add_data hizufügen
Total_mor <- death_canton_year%>%
  filter(Canton == "Total")%>%
  filter(Year >= 1910 & Year <= 1974)%>%
  mutate(mort = death/population*100000)%>%
  select(Year, mort)

add_data <- left_join(Total_inc, add_data, by = "Year")
add_data <- left_join(Total_mor, add_data, by = "Year")

#Plot der Daten
plot_incidence <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y=inc_cases, col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y=inc_cases), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("Incidence per 100,000 inhabitants")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("A) Yearly incidence") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))

plot_pop_u5 <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y=pop_u5, col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y=pop_u5), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("Population under 5 years in %")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("C) Population under 5 years in % of total population") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))

plot_birthrate <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y=`Crude Birth rate` , col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y=`Crude Birth rate`), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("Crude birth rate")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("D) Crude birth rate") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))

plot_GDP <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y= GDP, col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y= GDP), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("GDP per Capita")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("F) GDP per Capita") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))

plot_Livebirths <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y= `Live births`, col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y=`Live births`), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("Live births")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("E) Total live births") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))

plot_mort <- ggplot()+
  geom_line(data=add_data, aes(x=Year, y= mort, col="notified cases"),lwd= lwd_line)+
  geom_smooth(data=add_data, aes(x=Year, y= mort), method="loess", col="red", linetype="solid") +
  xlab("Year") +
  ylab("Mortality per 100,000 inhabitants")+
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("B) Mortality") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_plot),
    axis.text.y = element_text(size=size_plot),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot))


pdf(file = "output/add_data.pdf", width = 40, height = 20)
plot_incidence + plot_mort + plot_pop_u5 + plot_birthrate  + plot_Livebirths+ plot_GDP
dev.off()


#Cross-Korrelationen

# Leerer DataFrame zum Speichern der Ergebnisse
cor_results <- data.frame(
  Variable = character(),
  Reference = character(),  # Neue Spalte, um die Referenzvariable zu kennzeichnen (inc_cases oder mort)
  Max_Correlation = numeric(),
  Lag = integer(),
  stringsAsFactors = FALSE
)

# Korrelation berechnen und Ergebnisse speichern
variables <- c("pop_u5", "Crude Birth rate", "GDP", "Live births")

for (var in variables) {
  # Berechnung für inc_cases
  result_inc <- ccf(add_data$inc_cases, add_data[[var]], lag.max = 5, plot = FALSE)
  max_corr_inc <- max(result_inc$acf)
  lag_value_inc <- result_inc$lag[which.max(result_inc$acf)]
  
  cor_results <- rbind(cor_results, data.frame(
    Variable = var,
    Reference = "inc_cases",
    Max_Correlation = max_corr_inc,
    Lag = lag_value_inc
  ))
  
  # Berechnung für mort
  result_mort <- ccf(add_data$mort, add_data[[var]], lag.max = 5, plot = FALSE)
  max_corr_mort <- max(result_mort$acf)
  lag_value_mort <- result_mort$lag[which.max(result_mort$acf)]
  
  cor_results <- rbind(cor_results, data.frame(
    Variable = var,
    Reference = "mort",
    Max_Correlation = max_corr_mort,
    Lag = lag_value_mort
  ))
}

# Ergebnis anzeigen
print(cor_results)



#Lineare Korrelationen
# Berechnen der Korrelation zwischen var1 und den anderen Variablen
cor_inc_cases <- cor(add_data$inc_cases, add_data[, c("pop_u5", "Crude Birth rate", "GDP", "Live births")], use = "complete.obs")
cor_mort <- cor(add_data$mort, add_data[, c("pop_u5", "Crude Birth rate", "GDP", "Live births")], use = "complete.obs")

# Ausgabe der Korrelation
print(cor_inc_cases)
print(cor_mort)

