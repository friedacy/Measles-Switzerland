library(dplyr)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


data_un <- read_excel_allsheets("data_raw/Age_groups.xlsx") 

alldata <- rbindlist(data_un, fill = TRUE) %>%
  replace(is.na(.), 0) %>%
  filter(!Jahr==1877) %>%
  mutate(`<1` =c(`0`+`<1`),                                                                     
         `1-4` = c(`1`+`2-4` + `2`+`3` +`4` + `1-4`),                                           #deleted `<1`
         `5-14` =c(`5-14`+`5-9` +`10-14`),
         `15-19` =c(`15-19` + `15-16` +`17` + `18-19`),
         `20-29` =c(`20-29` + `20-24` + `25-29`),
         `30-39` =c(`30-39` + `30-34` + `35-39`),
         `40-49` =c(`40-49` + `40-44` + `45-49`),
         `50-79` =c(`50-59` + `60-69` + `70-79` + `50-54` +`55-59`+`60-64`+
                      `65-69` + `70-74` + `75-79`),
         `>=80` = c(`>79`+ `80-84` + `85-89` + `90-94` + `95-99` + `>99`))%>%
  select(Jahr, Sex, `<1`, `1-4`,`5-14`,`15-19`,`20-29`,`30-39`, `40-49`, `50-79`, `>=80`)%>%
  gather(.,age_group, death,3:11) %>%
  spread(.,Sex, death) %>%
  mutate(Total_death = c(M+W)) %>%
  select(Year=Jahr, age_group, Total_death)


population <- read.csv("data_raw/population_hmdb.txt", header=TRUE, sep="") %>%
  mutate(Age= recode(Age, "110+" = "110"),
         Age= as.integer(Age),
         age_group = case_when(Age==0 ~ "<1",
                               Age>=1 & Age <=4 ~ "1-4",
                               Age>=5 & Age <=14 ~ "5-14",
                               Age>=15 & Age <=19 ~ "15-19",
                               Age>=20 & Age <=29 ~ "20-29",
                               Age>=30 & Age <=39 ~ "30-39",
                               Age >= 40 & Age <= 49 ~ "40-49",
                               Age >= 50 & Age <= 79 ~ "50-79",
                               Age >=80 ~ ">=80")) %>%
  select(Year, age_group, Total) %>%
  group_by(Year, age_group) %>%
  mutate(pop_age = sum(Total)) %>%
  distinct(Year, age_group, .keep_all = TRUE) %>%
  ungroup()%>%
  select(-Total)%>%
  mutate(pop_age= round(pop_age)) %>%
  group_by(Year) %>%
  mutate(total_pop = sum(pop_age))

age_o <- c("<1","1-4", "5-14","15-19",
           "20-29", "30-39", "40-49",
           "50-79", ">=80")

data_age <- alldata %>%
  left_join(population) %>%
  mutate(mortality = Total_death/pop_age*10000,
         mortality2 = Total_death/pop_age*100000,
         mortality_pop_total = Total_death/total_pop*100000) %>%
  arrange(factor(age_group, levels =  c("<1","1-4", "5-14","15-19",
                                        "20-29", "30-39", "40-49",
                                        "50-79", ">=80")))

# Berechnung des 95%-Konfidenzintervalls
data_age_ci <- data_age %>%
  select(obs=Total_death, pop=pop_age) %>%
  as.matrix(.) %>%
  epi.conf(., ctype = "inc.rate", method ="exact" ,
           conf.level = 0.95) %>%
  mutate(est=round(est*100000,2),
         lower=round(lower*100000,2),
         upper=round(upper*100000,2),
         `Mortality (95% CI)` = paste0(est," (",lower, "-", upper, ")"))%>%
  cbind(data_age) 

# Speicherung der aktualisierten Daten
save(data_age_ci, file = "data/data_age_ci.RData")
write.xlsx(data_age, file = "data/data_age_ci.xlsx", rowNames = FALSE, overwrite = TRUE)


save(data_age ,file="data/data_age.RData")
write.xlsx(data_age,file="data/data_age.xlsx",rowNames=FALSE, overwrite = TRUE)

