
function_5_year <- function(Kt) {

# Incidence
load("data/cases_canton_yearly.RData")
  
data_Total <-  cases_canton_yearly %>%
  filter(Canton2 == Kt)

#data_Total <-  cases_canton_yearly %>%
  #filter(Canton2 == "Total")

data_cases <- data_Total %>%
  ungroup() %>%
  mutate(n_year = 1,
         Year_gr =  Year,
         Year_gr = ifelse(Year_gr>=2020,  "2020-2023", Year_gr),
         Year_gr = ifelse(Year_gr>=2015 & Year_gr < 2020, "2015-2019", Year_gr),
         Year_gr = ifelse(Year_gr>=2010 & Year_gr < 2015, "2010-2014", Year_gr),
         Year_gr = ifelse(Year_gr>=2005 & Year_gr < 2010, "2005-2009", Year_gr),
         Year_gr = ifelse(Year_gr>=2000 & Year_gr < 2005, "2000-2004", Year_gr),
         Year_gr = ifelse(Year_gr>=1995 & Year_gr < 2000, "1995-1999", Year_gr),
         Year_gr = ifelse(Year_gr>=1990 & Year_gr < 1995, "1990-1994", Year_gr),
         Year_gr = ifelse(Year_gr>=1985 & Year_gr < 1990, "1985-1989", Year_gr),
         Year_gr = ifelse(Year_gr>=1980 & Year_gr < 1985, "1980-1984", Year_gr),
         Year_gr = ifelse(Year_gr>=1975 & Year_gr < 1980, "1975-1979", Year_gr),
         Year_gr = ifelse(Year_gr>=1970 & Year_gr < 1975, "1970-1974", Year_gr),
         Year_gr = ifelse(Year_gr>=1965 & Year_gr < 1970, "1965-1969", Year_gr),
         Year_gr = ifelse(Year_gr>=1960 & Year_gr < 1965, "1960-1964", Year_gr),
         Year_gr = ifelse(Year_gr>=1955 & Year_gr < 1960, "1955-1959", Year_gr),
         Year_gr = ifelse(Year_gr>=1950 & Year_gr < 1955, "1950-1954", Year_gr),
         Year_gr = ifelse(Year_gr>=1945 & Year_gr < 1950, "1945-1949", Year_gr),
         Year_gr = ifelse(Year_gr>=1940 & Year_gr < 1945, "1940-1944", Year_gr),
         Year_gr = ifelse(Year_gr>=1935 & Year_gr < 1940, "1935-1939", Year_gr),
         Year_gr = ifelse(Year_gr>=1930 & Year_gr < 1935, "1930-1934", Year_gr),
         Year_gr = ifelse(Year_gr>=1925 & Year_gr < 1930, "1925-1929", Year_gr),
         Year_gr = ifelse(Year_gr>=1920 & Year_gr < 1925, "1920-1924", Year_gr),
         Year_gr = ifelse(Year_gr>=1915 & Year_gr < 1920, "1915-1919", Year_gr),
         Year_gr = ifelse(Year_gr>=1910 & Year_gr < 1915, "1910-1914", Year_gr),
         Year_gr = ifelse(Year_gr>=1905 & Year_gr < 1910, "1905-1909", Year_gr),
         Year_gr = ifelse(Year_gr>=1900 & Year_gr < 1905, "1900-1904", Year_gr),
         Year_gr = ifelse(Year_gr>=1895 & Year_gr < 1900, "1895-1899", Year_gr),
         Year_gr = ifelse(Year_gr>=1890 & Year_gr < 1895, "1890-1894", Year_gr),
         Year_gr = ifelse(Year_gr>=1885 & Year_gr < 1890, "1885-1889", Year_gr),
         Year_gr = ifelse(Year_gr>=1880 & Year_gr < 1885, "1880-1884", Year_gr),
         Year_gr = ifelse(Year_gr>=1877 & Year_gr < 1880, "1877-1879", Year_gr)) %>%
  group_by(Year_gr) %>%
  summarise(Inc_gr_sum = sum(cases_year, na.rm =TRUE),
            Pop_gr_sum = sum(population,na.rm =TRUE)) %>%
  ungroup()
  # filter( Year_gr == "1935-1939") %>%


data_inc <- data_cases %>%
  select(obs=Inc_gr_sum, pop=Pop_gr_sum) %>%
  as.matrix(.) %>%
  # epi.conf(., ctype = "inc.risk", method ="wilson" ,
  #          conf.level = 0.95) %>%
  epi.conf(., ctype = "inc.rate", method ="exact" ,
           conf.level = 0.95) %>%
  mutate(est=round(est*100000,2),
         lower=round(lower*100000,2),
         upper=round(upper*100000,2),
         `Incidence (95% CI)` = paste0(est," (",lower, "-", upper, ")")) %>%
  cbind(data_cases) %>%
  select(Year = Year_gr,   `Incidence (95% CI)`) %>%
  mutate(`Incidence (95% CI)` = recode( `Incidence (95% CI)`,
                                        "0 (0-0.05)" = "-",
                                        "0 (0-0.04)" = "-",
                                        "0 (0-0.03)" = "-",
                                        "0 (0-0.02)" = "-",
                                        "0 (0-0.01)" = "-",
                                        "0 (0-0.05)" = "-"))%>%
  mutate(`Incidence (95% CI)` = ifelse(Year == "1975-1979", "NA", `Incidence (95% CI)`))%>%             #Zwischen 1975 und 1989 liegt die Datenlücke, deswegen manuell NA
  mutate(`Incidence (95% CI)` = ifelse(Year == "1980-1984", "NA", `Incidence (95% CI)`))%>%
  mutate(`Incidence (95% CI)` = ifelse(Year == "1985-1989", "NA", `Incidence (95% CI)`))

# Mortality

load("data/death_canton_year.RData")
data_Total <-  death_canton_year %>%
  filter(Canton2 ==Kt)



 #data_Total <-  death_canton_year %>%
   #filter(Canton2 == "Total")


data_death <- data_Total %>%
  ungroup() %>%
  mutate(n_year = 1,
         Year_gr =  Year,
         Year_gr = ifelse(Year_gr>=2020,  "2020-2023", Year_gr),
         Year_gr = ifelse(Year_gr>=2015 & Year_gr < 2020, "2015-2019", Year_gr),
         Year_gr = ifelse(Year_gr>=2010 & Year_gr < 2015, "2010-2014", Year_gr),
         Year_gr = ifelse(Year_gr>=2005 & Year_gr < 2010, "2005-2009", Year_gr),
         Year_gr = ifelse(Year_gr>=2000 & Year_gr < 2005, "2000-2004", Year_gr),
         Year_gr = ifelse(Year_gr>=1995 & Year_gr < 2000, "1995-1999", Year_gr),
         Year_gr = ifelse(Year_gr>=1990 & Year_gr < 1995, "1990-1994", Year_gr),
         Year_gr = ifelse(Year_gr>=1985 & Year_gr < 1990, "1985-1989", Year_gr),
         Year_gr = ifelse(Year_gr>=1980 & Year_gr < 1985, "1980-1984", Year_gr),
         Year_gr = ifelse(Year_gr>=1975 & Year_gr < 1980, "1975-1979", Year_gr),
         Year_gr = ifelse(Year_gr>=1970 & Year_gr < 1975, "1970-1974", Year_gr),
         Year_gr = ifelse(Year_gr>=1965 & Year_gr < 1970, "1965-1969", Year_gr),
         Year_gr = ifelse(Year_gr>=1960 & Year_gr < 1965, "1960-1964", Year_gr),
         Year_gr = ifelse(Year_gr>=1955 & Year_gr < 1960, "1955-1959", Year_gr),
         Year_gr = ifelse(Year_gr>=1950 & Year_gr < 1955, "1950-1954", Year_gr),
         Year_gr = ifelse(Year_gr>=1945 & Year_gr < 1950, "1945-1949", Year_gr),
         Year_gr = ifelse(Year_gr>=1940 & Year_gr < 1945, "1940-1944", Year_gr),
         Year_gr = ifelse(Year_gr>=1935 & Year_gr < 1940, "1935-1939", Year_gr),
         Year_gr = ifelse(Year_gr>=1930 & Year_gr < 1935, "1930-1934", Year_gr),
         Year_gr = ifelse(Year_gr>=1925 & Year_gr < 1930, "1925-1929", Year_gr),
         Year_gr = ifelse(Year_gr>=1920 & Year_gr < 1925, "1920-1924", Year_gr),
         Year_gr = ifelse(Year_gr>=1915 & Year_gr < 1920, "1915-1919", Year_gr),
         Year_gr = ifelse(Year_gr>=1910 & Year_gr < 1915, "1910-1914", Year_gr),
         Year_gr = ifelse(Year_gr>=1905 & Year_gr < 1910, "1905-1909", Year_gr),
         Year_gr = ifelse(Year_gr>=1900 & Year_gr < 1905, "1900-1904", Year_gr),
         Year_gr = ifelse(Year_gr>=1895 & Year_gr < 1900, "1895-1899", Year_gr),
         Year_gr = ifelse(Year_gr>=1890 & Year_gr < 1895, "1890-1894", Year_gr),
         Year_gr = ifelse(Year_gr>=1885 & Year_gr < 1890, "1885-1889", Year_gr),
         Year_gr = ifelse(Year_gr>=1880 & Year_gr < 1885, "1880-1884", Year_gr),
         Year_gr = ifelse(Year_gr>=1877 & Year_gr < 1880, "1877-1879", Year_gr)) %>%
  group_by(Year_gr) %>%
  summarise(Death_gr_sum = sum(death, na.rm =TRUE),
            Pop_gr_sum = sum(population,na.rm =TRUE)) %>%
  ungroup()
# filter( Year_gr == "1935-1939") %>%


data_mor <- data_death  %>%
  select(obs=Death_gr_sum, pop=Pop_gr_sum) %>%
  as.matrix(.) %>%
  # epi.conf(., ctype = "inc.risk", method ="wilson" ,
  #          conf.level = 0.95) %>%
  epi.conf(., ctype = "inc.rate", method ="exact" ,
           conf.level = 0.95) %>%
  mutate(est=round(est*100000,2),
         lower=round(lower*100000,2),
         upper=round(upper*100000,2),
         `Mortality (95% CI)` = paste0(est," (",lower, "-", upper, ")")) %>%
  cbind(data_death) %>%
  select(Year = Year_gr, `Mortality (95% CI)`) 
  #mutate(`Mortality (95% CI)` = recode( `Mortality (95% CI)`,
   #                                     "0 (0-0.05" = "-",
    #                                    "0 (0-0.03)" = "-",
     #                                   "0 (0-0.02)" = "-",
      #                                  "0 (0-0.01)" = "-"))


# Case fatality

data_fatality <- data_cases %>%
  full_join(data_death)


data_fat <- data_fatality   %>%
  select(obs=Death_gr_sum, pop=Inc_gr_sum) %>%
  as.matrix(.) %>%
  # epi.conf(., ctype = "inc.risk", method ="wilson" ,
  #          conf.level = 0.95) %>%
  epi.conf(., ctype = "inc.rate", method ="exact" ,
           conf.level = 0.95) %>%
  mutate(est=round(est*100,2),
         lower=round(lower*100,2),
         upper=round(upper*100,2),
         `Case fatality rate (95% CI)` = paste0(est," (",lower, "-", upper, ")")) %>%
  cbind(data_fatality) %>%
  select(Year = Year_gr, `Case fatality rate (95% CI)`)  %>%
  mutate(`Case fatality rate (95% CI)` = recode(`Case fatality rate (95% CI)`, 
                                                "NaN (NaN-Inf)" = "-",
                                                "Inf (Inf-Inf)" = "-",
                                                "NA (NA-NA)" = "-",
                                                "0 (0-13.32)" = "-",
                                                "0 (0-2.89)" = "-")) %>%
  mutate(`Case fatality rate (95% CI)` = ifelse(Year == "1985-1989", "-", `Case fatality rate (95% CI)`))
         
                                                

data_all <- data_mor %>%
  #full_join(data_inc) %>%
  #full_join(data_fat) %>%
  mutate(Canton = Kt) %>%
  relocate(Canton) %>%
  select(-Canton) %>%
  distinct(Year,.keep_all = TRUE)

data_all <- data_all[-1, ]                   #Löscht die erste Zeile 1876 -> so mit Kaspar besprochen, dass wir das nicht reinnehmen
return(data_all)

}

#test<- function_5_year("Total")

#function_5_year("Total") %>%
 # kbl() %>%
  #kable_classic_2(full_width = F) %>%
  #save_kable("output/Table1.html")

#library(webshot2)
#webshot("output/Table1.html", "output/Table1.pdf")

Table1 <- function_5_year("Total")
write.xlsx(Table1,file="output/Table1.xlsx",rowNames=FALSE, overwrite = TRUE)

