#plot parameter

size_plot <- 25
lwd_line <- 1.5

#Daten Death auswählen

load("data/death_canton_year.RData")
data_death <- death_canton_year %>%
  filter(Canton2=="ZH") %>%           #Filter ZH, da ZH einer der ausgewählten Kantone. death_r stellt die Summe der ausgewählten Kantone dar. 
  ungroup() %>%
  mutate(
    inc= death_r/pop_r*100000,        #anstelle von death habe ich death_r (und auch entsprechende. Population)
    datum=ymd(paste0(Year, "01-01")),
    var= "death") %>%
  select(var, Year, datum, inc)

#Daten Cases auswählen

load("data/cases_canton_yearly.RData")
data_cases <-  cases_canton_yearly %>%
  filter(Canton2=="ZH") %>%
  ungroup() %>%
  distinct(Year, .keep_all = TRUE) %>%
  mutate(inc = cases_year_r/pop_r*100000,   #anstelle von cases_year habe ich cases_year_r (und entspr. Population) genommen damit eine Inzidenz von allen ausgewählten Kantonen ausgerechnet wird. Ansonsten wird nur diese von ZH genommen. 
         datum=ymd(paste0(Year, "01-01")),
         var= "cases") %>%
  select(var, Year, datum, inc)
data_cases$inc[data_cases$Year > 1974 & data_cases$Year < 1988 & data_cases$inc == 0] <- NA #sollte die Lücke als NA deklarieren und keine 0 Werte einsetzen

#save(data_cases ,file="data/data_cases.RData")
#write.xlsx(data_cases,file="data/data_cases.xlsx",rowNames=FALSE, overwrite = TRUE)


data_death2 <- death_canton_year %>%
  filter(Canton2=="ZH")%>% #Auswahl irgend eines Kantones mit reporting == Obligatory, da sonst nur unnötig viele Zeilen
  filter(Year > 1909)%>%
  filter(Year < 2021)%>%
  select(Year, death_r)

data_fat <- cases_canton_yearly %>%
  filter(Canton2=="ZH") %>%
  filter(Year < 2021)%>%
  ungroup() %>%
  select(Year, cases_year_r, pop_r)

data_fat <- merge(data_fat, data_death2, by="Year") %>%                     #full_join hat nicht richtig funktioniert
  mutate(inc = death_r/na_if(cases_year_r, 0) *100,                         #na_if verhindert dass Inf rauskommt, wenn man durch 0 Cases teilt
         datum=ymd(paste0(Year, "01-01")),
         var="case fatality") %>%
  select(var, Year, datum, inc)%>%
  #replace_with_na_all(condition = ~.inc == "Inf")
  mutate(inc=ifelse(Year > 1974, NA,inc))


         
data_all <- rbind(data_death,data_cases, data_fat) %>%
  mutate(var= as.factor(var))


#Plots zu einzelnen Komponenten zum Test
plot_cases <- ggplot(data_cases, aes(Year, inc)) +                                                    #col="Incidence" entfernt aus aes()
  geom_col(fill= mypalette_c[3], color= "black") +                                                                             #Um anstelle einer durchgehenden Linie Balken darzustellen, kannst du den geom_step()-Befehl durch geom_bar() oder geom_col() ersetzen.
  ylab("Incidence per 100'000 inhabitants")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot)) 

plot_deaths <- ggplot(data_death, aes(Year, inc)) +      #,col="Mortality"
  geom_col(fill= mypalette_c[4], color= "black") +
  ylab("Mortality per 100'000 inhabitants")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot)) 


plot_fat<- ggplot(data_fat, aes(Year, inc)) +            #col="Case fatality rate"             
  geom_col(fill= mypalette_c[1], color= "black") +
  ylab("Case fatality ratio in %")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot)) 

#cowplot::save_plot(paste0("output/Figure2_cases.pdf"), plot_cases ,base_height=20,base_width=20)
#cowplot::save_plot(paste0("output/Figure2_deaths.pdf"), plot_deaths ,base_height=10,base_width=20)
#cowplot::save_plot(paste0("output/Figure2_fat.pdf"), plot_fat ,base_height=10,base_width=20)


#Erstellen eines Facettenplots, um alles zu kombinieren

# Zuerst Daten anpassen, um für Facetten geeignet zu sein
data_cases$Variable <- "Incidence"
data_death$Variable <- "Mortality"
data_fat$Variable <- "Case fatality rate"

# Kombinieren der Datensätze zu einem
data_combined <- rbind(
  data_cases[, c("Year", "inc", "Variable")],
  data_death[, c("Year", "inc", "Variable")],
  data_fat[, c("Year", "inc", "Variable")]
)

# Definiere die Beschriftungen für jede Facette
facet_labels <- c(
  "Incidence" = "Incidence per 100,000 inhabitants",
  "Mortality" = "Mortality per 100,000 inhabitants",
  "Case fatality rate" = "Case fatality ratio in %"
)

# Definiere die Reihenfolge der Variablen als Faktor
data_combined$Variable <- factor(data_combined$Variable, 
                                 levels = c("Mortality", "Incidence", "Case fatality rate"))

# Facetten-Plot erstellen
plot_total <- ggplot(data_combined, aes(x = Year, y = inc, fill = Variable)) +
  
  geom_col(color = "black", size= 0.2) +
  geom_vline(xintercept = 1976, col = "red", lwd = 1.0) +  # Füge die vertikale rote Linie bei 1976 hinzu
  ylab(NULL) +
  xlab("Year") +
  
  # Facetten nach der Variable, jeweils ein separates Panel für Incidence, Mortality, Case fatality rate
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, labeller = labeller(Variable = facet_labels)) +
  
  scale_fill_manual(
    values = c("Incidence" = mypalette_c[3], 
               "Mortality" = mypalette_c[4], 
               "Case fatality rate" = mypalette_c[1])
  ) +
  
  scale_x_continuous(
    breaks = seq(1880, 2020, 10),
    labels = seq(1880, 2020, 10)
  ) +
  
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    axis.text = element_text(size = size_plot),
    legend.position = "none",  # Legende kann weggelassen werden, da jede Facette klar ist
    strip.text = element_text(size = size_plot),  # Titel der Facetten anpassen
    axis.title.x = element_text(size = size_plot),
    axis.title.y = element_text(size = size_plot)
  )

# Grafik speichern
cowplot::save_plot(paste0("output/Figure2.pdf"), plot_total, base_height = 10, base_width = 20)

