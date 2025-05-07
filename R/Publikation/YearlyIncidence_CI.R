# Berechnung des Standardfehlers und Konfidenzintervalle hinzufügen
load("data/cases_canton_yearly.RData")

data_cases <- cases_canton_yearly %>%
  filter(Canton2=="ZH") %>%
  ungroup() %>%
  distinct(Year, .keep_all = TRUE)%>%
  mutate(inc = cases_year_r/pop_r*100000)

# Falls du Werte in der Zeitperiode 1974-1988 auf NA setzen möchtest
data_cases$inc[data_cases$Year > 1974 & data_cases$Year < 1988 & data_cases$inc == 0] <- NA


data_cases_ci <- data_cases %>%
  select(obs=cases_year_r, pop=pop_r) %>%
  as.matrix(.) %>%
  epi.conf(., ctype = "inc.rate", method ="exact" ,
           conf.level = 0.95) %>%
  mutate(est=round(est*100000,2),
         lower=round(lower*100000,2),
         upper=round(upper*100000,2),
         `Incidence (95% CI)` = paste0(est," (",lower, "-", upper, ")"))%>%
  cbind(data_cases) 


Std_abweichung <- sd(data_cases$inc[data_cases$Year < 1975] )
print(Std_abweichung)
print(data_cases$inc[data_cases$Year < 1975])
