population_canton <- read.xlsx("data_raw/Population_Canton.xlsx", detectDates = TRUE) %>%
  gather(., Canton2, population,2:27, factor_key=TRUE)  %>%
  mutate(Canton2=recode(Canton2, "CH" ="Total")) %>%
  rename(Year=Jahr)
  

cases_canton_monthly <- read.xlsx("data_raw/Cases_canton_monthly_measles.xlsx", detectDates = TRUE) %>%
  gather(., Datum, cases,2:775, factor_key=TRUE) %>%
  mutate(Datum = ymd(Datum),
         Year = data.table::year(Datum),
         Month = data.table::month(Datum),
         Canton2 = Canton,
         Canton2 = recode(Canton2, 
                          "Zürich" = "ZH",
                          "Bern" = "BE",
                          "Luzern" = "LU",
                          "Uri" = "UR",
                          "Schwyz" = "SZ",
                          "Obwalden" ="OW",
                          "Nidwalden" ="NW",
                          "Glarus" = "GL",
                          "Zug" = "ZG",
                          "Fribourg" = "FR",
                          "Solothurn" = "SO",
                          "Baselstadt" = "BS",
                          "Baselland" = "BL",
                          "Schaffhausen" = "SH",
                          "Appenzell AR" = "AR",
                          "Appenzell IR" = "AI",
                          "St. Gallen" = "SG",
                          "Graubünden" = "GR",
                          "Aargau" = "AG",
                          "Thurgau" = "TG",
                          "Tessin" = "TI",
                          "Vaud" = "VD",
                          "Valais" = "VS",
                          "Neuchâtel" = "NE",
                          "Genève" = "GE"),
         Type_reporting = Canton2,
         Type_reporting = recode(Type_reporting,
                           "ZH" = "Obligatory",
                           "BE" = "Obligatory",
                           "LU" = "Obligatory",
                           "UR" = "voluntary",
                           "SZ" = "voluntary",
                           "OW" = "voluntary",
                           "NW" = "voluntary",
                           "GL" = "voluntary",
                           "ZG" = "voluntary",
                           "FR" = "voluntary",
                           "SO" = "voluntary",
                           "BS" = "Obligatory",
                           "BL" = "voluntary",
                           "SH" = "voluntary",
                           "AR" = "voluntary",
                           "AI" = "voluntary",
                           "SG" = "Obligatory",
                           "GR" = "Obligatory",
                           "AG" = "voluntary",
                           "TG" = "voluntary",
                           "TI" = "voluntary",
                           "VD" = "voluntary",
                           "VS" = "voluntary",
                           "NE" = "voluntary",
                           "GE" = "Obligatory")) %>%
  arrange(Datum) %>%
  group_by(Canton2) %>%
  mutate(cases =  as.integer(round(na_kalman(cases),0))) %>%
  ungroup() %>%
  dplyr::group_by(Type_reporting,Datum) %>%
  dplyr::mutate(cases_r = sum(cases, na.rm =TRUE)) %>%
  ungroup() %>%
#  filter(!Year==1909) %>%
  left_join(population_canton) %>%
  dplyr::group_by(Type_reporting,Datum) %>%
  dplyr::mutate(pop_r =sum(population, na.rm =TRUE)) %>%
  ungroup()

save(cases_canton_monthly  ,file="data/cases_canton_monthly.RData")
write.xlsx(cases_canton_monthly,file="data/cases_canton_monthly.xlsx",rowNames=FALSE, overwrite = TRUE)

cases_canton_yearly <- cases_canton_monthly %>%
  group_by(Canton2,Year) %>%
  mutate(cases_year = sum(cases, na.rm =TRUE)) %>%
  ungroup() %>% 
  group_by(Type_reporting, Canton,Year) %>%
  mutate(cases_year_r = sum(cases_r, na.rm =TRUE)) %>%
  ungroup() %>%
  distinct(Year, Canton2, .keep_all = TRUE) 


cases_1975 <- read.xlsx("data_raw/cases_1975_measles.xlsx", detectDates = TRUE) %>%              #Fallzahlen aus der "Neuzeit". CAVE Datenlücke 1975-1989
  gather(., Datum, cases,2:49, factor_key=TRUE) %>%
  mutate(Datum = ymd(Datum),
         Year = data.table::year(Datum),
         Month = data.table::month(Datum),
         Canton2 = Canton,
         Canton2 = recode(Canton2, 
                          "Zürich" = "ZH",
                          "Bern" = "BE",
                          "Luzern" = "LU",
                          "Uri" = "UR",
                          "Schwyz" = "SZ",
                          "Obwalden" ="OW",
                          "Nidwalden" ="NW",
                          "Glarus" = "GL",
                          "Zug" = "ZG",
                          "Fribourg" = "FR",
                          "Solothurn" = "SO",
                          "Baselstadt" = "BS",
                          "Baselland" = "BL",
                          "Schaffhausen" = "SH",
                          "Appenzell AR" = "AR",
                          "Appenzell IR" = "AI",
                          "St. Gallen" = "SG",
                          "Graubünden" = "GR",
                          "Aargau" = "AG",
                          "Thurgau" = "TG",
                          "Tessin" = "TI",
                          "Vaud" = "VD",
                          "Valais" = "VS",
                          "Neuchâtel" = "NE",
                          "Genève" = "GE"),
         Type_reporting = Canton2,
         Type_reporting = recode(Type_reporting,
                                 "ZH" = "Obligatory",
                                 "BE" = "Obligatory",
                                 "LU" = "Obligatory",
                                 "UR" = "voluntary",
                                 "SZ" = "voluntary",
                                 "OW" = "voluntary",
                                 "NW" = "voluntary",
                                 "GL" = "voluntary",
                                 "ZG" = "voluntary",
                                 "FR" = "voluntary",
                                 "SO" = "voluntary",
                                 "BS" = "Obligatory",
                                 "BL" = "voluntary",
                                 "SH" = "voluntary",
                                 "AR" = "voluntary",
                                 "AI" = "voluntary",
                                 "SG" = "Obligatory",
                                 "GR" = "Obligatory",
                                 "AG" = "voluntary",
                                 "TG" = "voluntary",
                                 "TI" = "voluntary",
                                 "VD" = "voluntary",
                                 "VS" = "voluntary",
                                 "NE" = "voluntary",
                                 "GE" = "Obligatory")) %>%
  arrange(Datum) %>%
  group_by(Canton2) %>%
  #mutate(cases =  as.integer(round(na_kalman(cases),0))) %>% #Modell, um fehlende Werte zu ersetzen, hier deaktiviert, da sonst die grosse Zeitlücke gefüllt wird
  ungroup() %>%
  dplyr::group_by(Type_reporting,Datum) %>%
  dplyr::mutate(cases_r = sum(cases, na.rm =TRUE)) %>%
  ungroup() %>%
  #  filter(!Year==1909) %>%
  left_join(population_canton) %>%
  dplyr::group_by(Type_reporting,Datum) %>%
  dplyr::mutate(pop_r =sum(population, na.rm =TRUE)) %>%
  ungroup()%>%
  #mutate(population = ifelse(Year==2023, 8902308, population))%>%
  #Damit Jahreszahlen nicht mit Monatszahlen vermischt werden beim zusammenfügen
  rename(cases_year=cases)%>%
  rename(cases_year_r=cases_r)

#zusammenführen der Datensätze
cases_canton_yearly  <- cases_canton_yearly %>%
  bind_rows(.,cases_1975) %>%
  arrange(Year)

save(cases_canton_yearly  ,file="data/cases_canton_yearly.RData")
write.xlsx(cases_canton_yearly,file="data/cases_canton_yearly.xlsx",rowNames=FALSE, overwrite = TRUE)

death_canton_year <- read.xlsx("data_raw/Death_Canton_measles.xlsx", detectDates = TRUE) %>%
  gather(., Canton2, death,2:27, factor_key=TRUE) %>%
  #filter(Year > 1909) %>%
  mutate( Canton = Canton2,
          Canton = recode(Canton2, 
                           "ZH" = "Zürich",
                           "BE" = "Bern",
                           "LU" = "Luzern",
                           "UR" ="Uri",
                           "SZ" = "Schwyz",
                           "OW" = "Obwalden",
                           "NW" = "Nidwalden",
                           "GL" = "Glarus",
                           "ZG" = "Zug",
                           "FR" = "Fribourg",
                           "SO" = "Solothurn",
                           "BS" = "Baselstadt",
                           "BL" = "Baselland",
                           "SH" = "Schaffhausen",
                           "AR" = "Appenzell AR",
                           "AI" = "Appenzell IR",
                           "SG" = "St. Gallen",
                           "GR" = "Graubünden",
                           "AG" = "Aargau",
                           "TG" = "Thurgau",
                           "TI" = "Tessin",
                           "VD" = "Vaud",
                           "VS" = "Valais",
                           "NE" = "Neuchâtel",
                           "GE" = "Genève"),
          Type_reporting = Canton2,
          Type_reporting = recode(Type_reporting,
                                    "ZH" = "Obligatory",
                                    "BE" = "Obligatory",
                                    "LU" = "Obligatory",
                                    "UR" = "voluntary",
                                    "SZ" = "voluntary",
                                    "OW" = "voluntary",
                                    "NW" = "voluntary",
                                    "GL" = "voluntary",
                                    "ZG" = "voluntary",
                                    "FR" = "voluntary",
                                    "SO" = "voluntary",
                                    "BS" = "Obligatory",
                                    "BL" = "voluntary",
                                    "SH" = "voluntary",
                                    "AR" = "voluntary",
                                    "AI" = "voluntary",
                                    "SG" = "Obligatory",
                                    "GR" = "Obligatory",
                                    "AG" = "voluntary",
                                    "TG" = "voluntary",
                                    "TI" = "voluntary",
                                    "VD" = "voluntary",
                                    "VS" = "voluntary",
                                    "NE" = "voluntary",
                                    "GE" = "Obligatory")) %>%
  arrange(Year) %>%
  group_by(Canton2) %>%
  #mutate(death =  as.integer(round(na_kalman(death),0)))%>%
  ungroup() %>%
  dplyr::group_by(Type_reporting,Year)%>%
  dplyr::mutate(death_r = sum(death, na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(population_canton ) %>%
  dplyr::group_by(Type_reporting,Year) %>%
  dplyr::mutate(pop_r =sum(population, na.rm=TRUE)) %>%
  ungroup() 

#death_1969 <- read.xlsx("data_raw/Deaths_1969.xlsx", detectDates = TRUE) %>%
#  mutate(Canton2 = "Total") %>%
#  left_join(population_canton) %>%
#  mutate(population = ifelse(Year==2023, 8902308, population))


#death_canton_year <- death_canton_year %>%
#  bind_rows(.,death_1969) %>%
#  arrange(Year)

save(death_canton_year ,file="data/death_canton_year.RData")
write.xlsx(death_canton_year,file="data/death_canton_year.xlsx",rowNames=FALSE, overwrite = TRUE)

